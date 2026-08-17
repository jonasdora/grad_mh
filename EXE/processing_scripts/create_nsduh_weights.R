library(tidyverse)
library(glue)
source('C:/Users/Jonas Dora/OneDrive - UW/studies/shackman/grad_mh/project_config.R')

load('{DATA_DIR}/NSDUH/nsduh_study_data_2023-01-15.RData' %>% glue())
load('{DATA_DIR}/acha_study_data_2021-02-04.RData' %>% glue())

# Will create weights using the probability of being in the ACHA-II sample each year
# Using age and sex categories only for the creation of the weights 
# Note not able to use gender as not present in the NSDUH data

# age categories: 
#   18, 21
#   22, 23
#   24, 25
#   26, 29
#   30, 34
#   35, 49
#   50, 64
#   65, Inf

table(nsduh_study_df$age_categories)
min(grads_only_study_df$Q46_age, na.rm=TRUE)

# First create an age bucket variable using the raw response from the NSDUH
# item - which was re-coded as age_categories for to create a variable name that
# includes semantic information about the variable
nsduh_study_df[['age_buckets']] <- case_when(
    nsduh_study_df[['age_categories']] %in% as.character(12:17) ~ '999',
    nsduh_study_df[['age_categories']] == '18' ~ '18, 21', 
    nsduh_study_df[['age_categories']] == '19' ~ '18, 21',
    nsduh_study_df[['age_categories']] == '20' ~ '18, 21',
    nsduh_study_df[['age_categories']] == '21' ~ '18, 21',
    nsduh_study_df[['age_categories']] == '22, 23' ~ '22, 23',
    nsduh_study_df[['age_categories']] == '24, 25' ~ '24, 25',
    nsduh_study_df[['age_categories']] == '26, 29' ~ '26, 29',
    nsduh_study_df[['age_categories']] == '30, 34' ~ '30, 34',
    nsduh_study_df[['age_categories']] == '35, 49' ~ '35, 49',
    nsduh_study_df[['age_categories']] == '50, 64' ~ '50, 64',
    nsduh_study_df[['age_categories']] == '65, Inf' ~ '65, Inf'
)

# Filter out cases with missing data and with college
nsduh_matched_df <- nsduh_study_df %>% 
    filter(age_buckets != '999' & college_adult_mask == 1) %>% 
    mutate(
    RaceRecode_NSDUH = case_when(
        race_ethn == 'NonHisp White' ~ 'white',
        race_ethn == 'NonHisp Asian' ~ 'asian',
        race_ethn == 'Hispanic' ~ 'hispanic',
        race_ethn == "NonHisp more than one race" ~ 'multi',
        race_ethn == "NonHisp Black/Afr Am" ~ 'black',
        race_ethn == "NonHisp Native Am/AK Native" ~ 'native',
        race_ethn == "NonHisp Native HI/Other Pac Isl" ~ 'native'
    )
)

# Generate a weights data.frame
grads_only_study_df[['age_buckets']] <- case_when(
    between(grads_only_study_df[['Q46_age']], 18, 21.999) ~ '18, 21',
    between(grads_only_study_df[['Q46_age']], 22, 23.999) ~ '22, 23',
    between(grads_only_study_df[['Q46_age']], 24, 25.999) ~ '24, 25',
    between(grads_only_study_df[['Q46_age']], 26, 29.999) ~ '26, 29',
    between(grads_only_study_df[['Q46_age']], 30, 34.999) ~ '30, 34',
    between(grads_only_study_df[['Q46_age']], 35, 49.999) ~ '35, 49',
    between(grads_only_study_df[['Q46_age']], 50, 64.999) ~ '50, 64',
    grads_only_study_df[['Q46_age']]>= 65 ~ '65, Inf'
)

grads_only_study_df <- grads_only_study_df %>%
    mutate(
        RaceRecode_NSDUH = case_when(
            race_ethn == 'white' ~ 'white',
            race_ethn == 'black' ~ 'black',
            race_ethn == 'hispanic' ~ 'hispanic',
            race_ethn == 'asian' ~ 'asian',
            race_ethn == 'native' ~ 'native',
            race_ethn == 'multi' ~ 'multi',
            race_ethn == 'other' ~ NA_character_,  # No mapping in NSDUH
            TRUE ~ NA_character_
        )
    )

# Make a data.frame with weights
grads_normalized_weights <- grads_only_study_df %>%
    # removing transgender as there is no indicator for this in NSDUH
    filter(Q47_gender != "Transgender") %>% 
    filter(!is.na(age_buckets) & !is.na(RaceRecode_NSDUH)) %>% 
    group_by(Q47_gender, age_buckets, RaceRecode_NSDUH) %>% 
    summarize(obs = n()) %>%
    ungroup() %>% 
    mutate(
        norm_prop = obs/sum(obs), 
        sex = Q47_gender
    ) %>% 
    select(sex, age_buckets, RaceRecode_NSDUH, norm_prop)

grads_raw_weights <- grads_only_study_df %>%
    # removing transgender as there is no indicator for this in NSDUH
    filter(Q47_gender != "Transgender") %>% 
    filter(!is.na(age_buckets)) %>% 
    group_by(Q47_gender, age_buckets, RaceRecode_NSDUH) %>% 
    summarize(obs = n()) %>%
    ungroup() %>% 
    mutate(
        raw_prop = obs/sum(obs), 
        sex = Q47_gender
    ) %>% 
    filter(!is.na(RaceRecode_NSDUH)) %>% 
    select(sex, age_buckets, RaceRecode_NSDUH, raw_prop)

nsduh_wght_basis <- nsduh_matched_df %>% 
    group_by(sex, age_buckets, RaceRecode_NSDUH) %>%
    summarize(obs = n()) %>% 
    ungroup() %>% 
    mutate(
        matched_prop = obs / sum(obs)
    ) %>% 
    select(sex, age_buckets, RaceRecode_NSDUH, matched_prop)

nsduh_weights <- merge(grads_raw_weights, grads_normalized_weights, by = c('sex', 'age_buckets', 'RaceRecode_NSDUH'))
nsduh_weights <- merge(nsduh_weights, nsduh_wght_basis, by = c('sex', 'age_buckets', 'RaceRecode_NSDUH'))
nsduh_weights[['normalized_weights']] <- nsduh_weights[['norm_prop']] / nsduh_weights[['matched_prop']]
nsduh_weights[['raw_weights']] <- nsduh_weights[['raw_prop']] / nsduh_weights[['matched_prop']]

nsduh_matched_df <- nsduh_weights %>% 
  select(sex, age_buckets, RaceRecode_NSDUH, raw_weights, normalized_weights) %>% 
  right_join(nsduh_matched_df, by=c('sex', 'age_buckets', 'RaceRecode_NSDUH'))

nsduh_weighted_summary_df <- nsduh_matched_df %>% 
    group_by(c_Time) %>% 
    summarize(
        anymi_12mos = sum(anymi_12mos * normalized_weights) / sum(normalized_weights) * 100,
        suic_thnk_12mos= sum(as.numeric(suic_thnk_12mos == 'Yes') * normalized_weights, na.rm = TRUE) / sum(normalized_weights) * 100, 
        suic_try_12mos = sum(as.numeric(suic_try_12mos == 'Yes') * normalized_weights, na.rm = TRUE) / sum(normalized_weights) * 100,
        global_health_dich = sum(as.numeric(global_health %in% c('Poor', 'Fair')) * normalized_weights) / sum(normalized_weights) *100,
    ) %>% 
    ungroup() %>% 
    select(c_Time, anymi_12mos, suic_thnk_12mos, suic_try_12mos, global_health_dich)

save(list=c('nsduh_matched_df', 'nsduh_weighted_summary_df'), 
     file='{DATA_DIR}/NSDUH/nsduh_matched_study_data_{Sys.Date()}.RData' %>% glue())
