# Processes, combines, transforms and saves ACHA data sets
library(glue)
library(haven)
library(tidyverse)

# File containing all functions needed to process and clean data used in project
HOME <- Sys.getenv('HOME')
source(here::here("project_config.R"))
sapply(list.files(R_DIR, full.names = TRUE), source)

# 2008 - 2011 data 
acha_F08_S11 <- load_and_prep_ACHA_file("{DATA_DIR}/ACHA-II_F08_S11.sav" %>% glue())
acha_F08_S11_df_list <- create_study_datasets(acha_F08_S11, '{MAPS_DIR}/ACHA_map_2008to2011.yaml' %>% glue())

# Adding in a blank "sex" variable to make things easier down the road
acha_F08_S11_df_list[["base_df"]] <- acha_F08_S11_df_list[["base_df"]] %>% 
    mutate(
        Q47_sex = NA
    )

# 2011 - 2015 data 
acha_F11_S15 <- load_and_prep_ACHA_file("{DATA_DIR}/ACHA-II_F11_S15.sav" %>% glue())
acha_F11_S15_df_list <- create_study_datasets(acha_F11_S15, '{MAPS_DIR}/ACHA_map_2011to2015.yaml' %>% glue())

# Adding in a blank "sex" variable to make things easier down the road
acha_F11_S15_df_list[["base_df"]] <- acha_F11_S15_df_list[["base_df"]] %>% 
    mutate(
        Q47_sex = NA
    )

# 2015 - 2019 data 
acha_F15_S19 <- load_and_prep_ACHA_file("{DATA_DIR}/ACHA-II_F15_S19.sav" %>% glue())
acha_F15_S19_df_list <- create_study_datasets(acha_F15_S19, '{MAPS_DIR}/ACHA_map_2015to2019.yaml' %>% glue())

# Need a special set of recodes to create a three-category gender variable
# Will operate on the base_df - then re-create the study_df using a data_map that generalizes
acha_F15_S19_df_list[["base_df"]] <- acha_F15_S19_df_list[["base_df"]] %>% 
    mutate(
        Q47_gender = ifelse(Q47_trans == "Yes", "Transgender", Q47_sex)
    )

# Extracting Study variables - Using the ACHA map for 2008 to 2011
acha_map_F08_S11 <- yaml::read_yaml("{MAPS_DIR}/ACHA_map_2008to2011.yaml" %>% glue())
study_variables <- lapply(acha_map_F08_S11, function(x) x[['study_name']]) %>% unlist()
study_variables <- c(study_variables, "Q47_sex")

acha_F08_S11_df_list[["study_df"]] <- acha_F08_S11_df_list[["base_df"]][study_variables]
acha_F11_S15_df_list[["study_df"]] <- acha_F11_S15_df_list[["base_df"]][study_variables]
acha_F15_S19_df_list[["study_df"]] <- acha_F15_S19_df_list[["base_df"]][study_variables]

# useful for computing race inside the tidy pipeline below
race_vars <- acha_F08_S11_df_list[["study_df"]] %>% 
    select(starts_with('Q54')) %>% 
    colnames()

complete_study_df <- rbind(
    acha_F08_S11_df_list[["study_df"]], 
    acha_F11_S15_df_list[["study_df"]], 
    acha_F15_S19_df_list[["study_df"]]
) %>% 
    # This recodes missing values on race checklist items to 0
    mutate_at(.vars=race_vars, .funs = function(x) ifelse(is.na(x), 0, x)) %>% 
    mutate(
        # DeYoung wants PERMID name to be present
        PERMID = school_id,
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
        # Create dichotomous variables for "diagnosis" 
        across(starts_with("Q31"), 
               .fns = list(dx = ~ifelse(. == "No", 0, 1)), 
               .names = "{col}_dich"),
        tot_race = rowSums(.[race_vars], na.rm = TRUE), 
        race_ethn = ifelse(tot_race > 1, "multi", NA), 
        race_ethn = ifelse(Q54_white == 1 & is.na(race_ethn), "white", race_ethn), 
        race_ethn = ifelse(Q54_black == 1 & is.na(race_ethn), 'black', race_ethn), 
        race_ethn = ifelse(Q54_hispanic == 1 & is.na(race_ethn), 'hispanic', race_ethn), 
        race_ethn = ifelse(Q54_asian == 1 & is.na(race_ethn), 'asian', race_ethn), 
        race_ethn = ifelse(Q54_native == 1 & is.na(race_ethn), 'native', race_ethn),
        race_ethn = ifelse(Q54_biracial == 1 & is.na(race_ethn), 'multi', race_ethn),
        race_ethn = ifelse(Q54_other == 1 & is.na(race_ethn), 'other', race_ethn), 
        race_ethn = forcats::fct_relevel(race_ethn, 'white', 'black', 'hispanic', 'asian', 'native', 'multi', 'other'), 
        # Variable to be used for creating a consistent mapping with NSDUH peers
        # The slight discrepancies between variables are due to missingness on the Hispanic variable
        # KD Version
        RaceRecode_NSDUH = ifelse(tot_race == 0, NA, "TBD"),
        RaceRecode_NSDUH = ifelse(Q54_white == 1 & tot_race == 1, 'white', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_black == 1 & tot_race == 1, 'black', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_asian == 1 & tot_race == 1, 'asian', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_native == 1 & tot_race == 1, 'native', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_other == 1 & tot_race == 1, 'other', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_biracial == 1 & tot_race == 1, 'multi', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_hispanic == 1 & tot_race == 1, 'hispanic', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_hispanic == 1 & tot_race > 1, 'hispanic', RaceRecode_NSDUH),
        RaceRecode_NSDUH = ifelse(Q54_hispanic == 0 & tot_race > 1, 'multi', RaceRecode_NSDUH),
        RaceRecode_NSDUH = forcats::fct_relevel(RaceRecode_NSDUH, 'white', 'black', 'hispanic', 'asian', 'native', 'multi', 'other'),
        # school size
        school_size = forcats::fct_relevel(school_size, "< 2,500 students", "2,500 - 4,999 students", 
                                           "5,000 - 9,999 students", "10,000 - 19,999 students", 
                                           "20,000 students or more"),
        # Convert survey period to factor
        survey_period = forcats::fct_relevel(survey_period, 
                                             "Fall 2008", "Spring 2009", 
                                             "Fall 2009", "Spring 2010", 
                                             "Fall 2010", "Spring 2011", 
                                             "Fall 2011", "Spring 2012", 
                                             "Fall 2012", "Spring 2013", 
                                             "Fall 2013", "Spring 2014", 
                                             "Fall 2014", "Spring 2015", 
                                             "Fall 2015", "Spring 2016", 
                                             "Fall 2016", "Spring 2017", 
                                             "Fall 2017", "Spring 2018", 
                                             "Fall 2018", "Spring 2019"), 
        local_population = forcats::fct_relevel(local_population,
                                                "under 2,500",
                                                "2,500-9,999",
                                                "10,000 – 49,999",
                                                "50,000-249,999",
                                                "250,000-499,999",
                                                "over 500,000"), 
        us_region = forcats::fct_relevel(us_region, 
                                         "Northeast",
                                         "Midwest",
                                         "South",
                                         "West",
                                         "Outside U.S."), 
        carnegie_category = forcats::fct_relevel(carnegie_category, 
                                                 "Associates Colleges",
                                                 "Baccalaureate Colleges",
                                                 "Masters Colleges and Universities",
                                                 "Research Institutions",
                                                 "Special Focus Institutions",
                                                 "Miscellaneous/Not Classified"), 
        academic_year = case_when(
            survey_period %in% c("Fall 2008", "Spring 2009") ~ "AY2008", 
            survey_period %in% c("Fall 2009", "Spring 2010") ~ "AY2009",
            survey_period %in% c("Fall 2010", "Spring 2011") ~ "AY2010",
            survey_period %in% c("Fall 2011", "Spring 2012") ~ "AY2011",
            survey_period %in% c("Fall 2012", "Spring 2013") ~ "AY2012",
            survey_period %in% c("Fall 2013", "Spring 2014") ~ "AY2013",
            survey_period %in% c("Fall 2014", "Spring 2015") ~ "AY2014",
            survey_period %in% c("Fall 2015", "Spring 2016") ~ "AY2015",
            survey_period %in% c("Fall 2016", "Spring 2017") ~ "AY2016",
            survey_period %in% c("Fall 2017", "Spring 2018") ~ "AY2017",
            survey_period %in% c("Fall 2018", "Spring 2019") ~ "AY2018"
        )
    )

# Calculate a total and any diagnosis using row
# NSDUH "total" dx and "any" dx columns
total_dx_nsduh_cols <- complete_study_df %>% 
    select(starts_with('Q31') & ends_with("_dich")) %>% 
    select(-contains("substance"), -contains("adhd")) %>% 
    names()

# NHCA "total" dx and "any" dx columns
total_dx_ncha_cols <- complete_study_df %>% 
    select(starts_with('Q31') & ends_with("_dich")) %>% 
    names()

complete_study_df[["total_dx_nsduh"]] <- rowSums(complete_study_df[total_dx_nsduh_cols], na.rm = TRUE)
complete_study_df[["total_dx_nsduh"]] <- ifelse(rowSums(is.na(complete_study_df[total_dx_nsduh_cols])) == length(total_dx_nsduh_cols), 
                                               NA, complete_study_df[["total_dx_nsduh"]])
complete_study_df[["any_dx_nsduh"]] <- ifelse(complete_study_df[["total_dx_nsduh"]] > 0, 1, 0)

complete_study_df[["total_dx"]] <- rowSums(complete_study_df[total_dx_ncha_cols], na.rm = TRUE)
complete_study_df[["total_dx"]] <- ifelse(rowSums(is.na(complete_study_df[total_dx_ncha_cols])) == length(total_dx_ncha_cols), 
                                          NA, complete_study_df[["total_dx"]])
complete_study_df[["any_dx"]] <- ifelse(complete_study_df[["total_dx"]] > 0, 1, 0)

# Select only the graduate students from the full data sets
grads_only_study_df <- complete_study_df %>% 
    filter(Q51_classyear == "Graduate or professional")

# Add a within PERMID + within survey case count variable - will be used later on to kick some schools' data out
grads_only_study_df <- grads_only_study_df %>%
    group_by(survey_period, school_id) %>% 
    summarize(total_obs = n()) %>% 
    merge(grads_only_study_df, ., by=c("school_id", "survey_period"))

# Lastly - bring in the raw data sets with SPSS formatting and add the cleaned up study variables back in. 
acha_F08_S11_spss <- read_spss("{DATA_DIR}/ACHA-II_F08_S11.sav" %>% glue()) %>% 
    sjlabelled::to_factor()

acha_F11_S15_spss <- read_spss("{DATA_DIR}/ACHA-II_F11_S15.sav" %>% glue()) %>% as.data.frame() %>% 
    sjlabelled::as_factor()

acha_F15_S19_spss <- read_spss("{DATA_DIR}/ACHA-II_F15_S19.sav" %>% glue()) %>% as.data.frame() %>% 
    sjlabelled::to_factor()

# This will drop every column that is not shared
keep_cols <- intersect(colnames(acha_F08_S11_spss), colnames(acha_F11_S15_spss)) %>% 
    intersect(., colnames(acha_F15_S19_spss))

combined_spss <- rbind(acha_F08_S11_spss[keep_cols], acha_F11_S15_spss[keep_cols], acha_F15_S19_spss[keep_cols])
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
     file = "{DATA_DIR}/acha_study_data_{Sys.Date()}.RData" %>% glue())

