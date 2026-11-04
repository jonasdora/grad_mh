# Processes, combines, transforms and saves NCHA data sets
# This is outdated do not use
library(glue)
library(haven)
library(tidyverse)

# File containing all functions needed to process and clean data used in project
source(here::here("project_config.R"))
sapply(list.files(R_DIR, full.names = TRUE), source)

# 2008 - 2011 data 
ncha_F08_S11 <- load_and_prep_NCHA_file("{DATA_DIR}/NCHA-II_F08_S11.sav" %>% glue())
ncha_F08_S11_df_list <- create_study_datasets(ncha_F08_S11, '{MAPS_DIR}/NCHA_map_2008to2011.yaml' %>% glue())

# 2011 - 2015 data 
ncha_F11_S15 <- load_and_prep_NCHA_file("{DATA_DIR}/NCHA-II_F11_S15.sav" %>% glue())
ncha_F11_S15_df_list <- create_study_datasets(ncha_F11_S15, '{MAPS_DIR}/NCHA_map_2011to2015.yaml' %>% glue())

# 2015 - 2019 data 
ncha_F15_S19 <- load_and_prep_NCHA_file("{DATA_DIR}/NCHA-II_F15_S19.sav" %>% glue())
ncha_F15_S19_df_list <- create_study_datasets(ncha_F15_S19, '{MAPS_DIR}/NCHA_map_2015to2019.yaml' %>% glue())

# Need a special set of recodes to create a three-category gender variable
# Will operate on the base_df - then re-create the study_df using a data_map that generalizes
ncha_F15_S19_df_list[["base_df"]] <- ncha_F15_S19_df_list[["base_df"]] %>% 
    mutate(
        Q47_gender = ifelse(Q47_trans == "Yes", "Transgender", Q47_sex)
    )

# Extracting Study variables - Using the NCHA map for 2008 to 2011
ncha_map_F08_S11 <- yaml::read_yaml("{MAPS_DIR}/NCHA_map_2008to2011.yaml" %>% glue())
study_variables <- lapply(ncha_map_F08_S11, function(x) x[['study_name']]) %>% unlist()

ncha_F08_S11_df_list[["study_df"]] <- ncha_F08_S11_df_list[["base_df"]][study_variables]
ncha_F11_S15_df_list[["study_df"]] <- ncha_F11_S15_df_list[["base_df"]][study_variables]
ncha_F15_S19_df_list[["study_df"]] <- ncha_F15_S19_df_list[["base_df"]][study_variables]

complete_study_df <- rbind(
    ncha_F08_S11_df_list[["study_df"]], 
    ncha_F11_S15_df_list[["study_df"]], 
    ncha_F15_S19_df_list[["study_df"]]
) %>% 
    mutate(
        # releveled so Excellent is highest and Poor is lowest
        global_health_r = ifelse(global_health == "Don't know", NA, global_health) %>% 
            forcats::fct_relevel(., "Poor", "Fair", "Good", "Very good", "Excellent"), 
        # adding an arbitrary date to create a variable with date/time properties
        time_point = str_replace_all(survey_period, pattern = c("Fall " = "10-01-", "Spring " = "04-01-")) %>% 
            as.Date(., format = "%m-%d-%Y"),
        # A time variable centered at 0, and scaled so that 1 = 1 year
        c_Time = round(
            difftime(time_point, as.Date("10-01-2008", format="%m-%d-%Y"), units = "weeks")/52 , 
            digits = 1
        ) %>% 
            as.numeric(),
        # Re-level the distress and suicide variables so no, never is lowest and yes 2 weeks is highest
        across(starts_with('Q30'), 
               .fns = list(rlvl = ~forcats::fct_relevel(., 
                                                        'No, Never', 
                                                        'No, not in the last 12 months', 
                                                        'Yes, in the last 12 months', 
                                                        'Yes, in the last 30 days', 
                                                        'Yes, in the last 2 weeks')), 
               .names = "{col}_r"), 
        # create a two-week version of the distress variables
        across(starts_with('Q30') & ends_with('_r'), 
               .fns = list(two_weeks = ~ifelse(. == 'Yes, in the last 2 weeks', 1, 0)), 
               .names = "{col}_2wks"), 
        # Create a one-month version of the distress variables
        across(starts_with('Q30') & ends_with('_r'), 
               .fns = list(thirty_days = ~ifelse(. == 'Yes, in the last 2 weeks' | 
                                                     . == 'Yes, in the last 30 days', 1, 0)), 
               .names = "{col}_30days"), 
        # Create a one-year version of the distress variables
        across(starts_with('Q30') & ends_with('_r'), 
               .fns = list(year = ~ifelse(. == 'Yes, in the last 2 weeks' | 
                                              . == 'Yes, in the last 30 days' |
                                              . == 'Yes, in the last 12 months', 1, 0)), 
               .names = "{col}_12mos"), 
        # Create dichotomous variables for "diagnosis" 
        across(starts_with("Q31"), 
               .fns = list(dx = ~ifelse(. == "No", 0, 1)), 
               .names = "{col}_dich")
    )

# Select only the graduate students from the full data sets
grads_only_study_df <- complete_study_df %>% 
    filter(Q51_classyear == "Graduate or professional")

# Add a within PERMID + within survey case count variable - will be used later on to kick some schools' data out
grads_only_study_df <- grads_only_study_df %>%
    group_by(survey_period, school_id) %>% 
    summarize(total_obs = n()) %>% 
    merge(grads_only_study_df, ., by=c("school_id", "survey_period"))

# Lastly - bring in the raw data sets with SPSS formatting and add the cleaned up study variables back in. 
ncha_F08_S11_spss <- read_spss("{DATA_DIR}/NCHA-II/NCHA-II_F08_S11.sav" %>% glue()) %>% 
    sjlabelled::to_factor()

ncha_F11_S15_spss <- read_spss("{DATA_DIR}/NCHA-II/NCHA-II_F11_S15.sav" %>% glue()) %>% as.data.frame() %>% 
    sjlabelled::as_factor()

ncha_F15_S19_spss <- read_spss("{DATA_DIR}/NCHA-II/NCHA-II_F15_S19.sav" %>% glue()) %>% as.data.frame() %>% 
    sjlabelled::to_factor()

# This will drop every column that is not shared
keep_cols <- intersect(colnames(ncha_F08_S11_spss), colnames(ncha_F11_S15_spss)) %>% 
    intersect(., colnames(ncha_F15_S19_spss))

combined_spss <- rbind(ncha_F08_S11_spss[keep_cols], ncha_F11_S15_spss[keep_cols], ncha_F15_S19_spss[keep_cols])
complete_df <- cbind(combined_spss, complete_study_df)

# And now to write out all of the files
write.csv(grads_only_study_df,
          "{DATA_DIR}/grad_students_master_vars_{Sys.Date()}.csv" %>% glue(), 
          row.names = FALSE)

write.csv(complete_study_df,
          "{DATA_DIR}/all_students_master_vars_{Sys.Date()}.csv" %>% glue(), 
          row.names = FALSE)

write_sav(grads_only_study_df,
          "{DATA_DIR}/grad_students_master_vars_{Sys.Date()}.sav" %>% glue())

write_sav(complete_study_df,
          "{DATA_DIR}/all_students_master_vars_{Sys.Date()}.sav" %>% glue())

save(list = c("complete_study_df", "complete_df", "grads_only_study_df"), 
     file = "{DATA_DIR}/ncha_study_data_{Sys.Date()}.RData" %>% glue())