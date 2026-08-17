
library(tidyverse)
library(glue)


NCHA_DATA_VERSION  <- "2021-02-04"
NCHA_DATA_SUBDIR   <- ""   

NSDUH_DATA_VERSION <- "2023-01-15"
NSDUH_DATA_SUBDIR  <- "NSDUH"


#' Load and prepare the ACHA-II / NCHA graduate student data.
#' @return data.frame `grads_model_base` with global_health_dich derived.
load_ncha_data <- function() {

    path <- file.path(DATA_DIR, NCHA_DATA_SUBDIR,
                      glue("acha_grad_students_base_{NCHA_DATA_VERSION}.RData"))
    if (!file.exists(path)) {
        stop(glue("NCHA data not found: {path}\n",
                  "  -> check NCHA_DATA_SUBDIR in R/data_prep.R"))
    }
    load(path)   # -> grads_model_base

    grads_model_base <- grads_model_base %>%
        mutate(
            global_health_dich = ifelse(global_health_r %in% c('Poor', 'Fair'), 1, 0),
            global_health_dich = ifelse(is.na(global_health_r), NA, global_health_dich),
            Q54_poc = ifelse(is.na(Q54_white), NA, 1L - Q54_white),

            Q47_gender_binary = factor(
                ifelse(Q47_gender == "Transgender", NA_character_, as.character(Q47_gender)),
                levels = c("Female", "Male")
            )
        )

    neg_emo_items <- c("Q30A_hopeless_r_2wks",  "Q30H_anger_r_2wks",
                       "Q30G_anxiety_r_2wks",   "Q30F_depressed_r_2wks",
                       "Q30D_lonely_r_2wks",    "Q30E_sad_r_2wks")

    stopifnot(all(neg_emo_items %in% names(grads_model_base)))

    .m <- as.matrix(grads_model_base[, neg_emo_items])
    grads_model_base$neg_emo_any <- dplyr::case_when(
        rowSums(.m == 1, na.rm = TRUE) > 0 ~ 1,   # any endorsement wins
        rowSums(is.na(.m)) > 0             ~ NA_real_,  # else NA if incomplete
        TRUE                               ~ 0
    )
    rm(.m)

    n_neg <- sum(!is.na(grads_model_base$neg_emo_any))
    grads_model_base
}


#' Shared NSDUH derivations, applied to both matched and unmatched samples.
#'
#' Provenance: the union of the mutate() blocks across the recovered
#' nsduh_*.R scripts. Each original script derived only the outcome it needed;
#' applying all of them is equivalent and removes a drift hazard.
.nsduh_derive <- function(df) {
    df %>%
        mutate(
            # from every nsduh_*.R
            quad_c_Time = as.numeric(c_Time)^2,

            # from nsduh_suic_thnk.R / nsduh_matched_suic_thnk.R
            suic_thnk_12mos = forcats::fct_relevel(suic_thnk_12mos, 'No', 'Yes'),

            # from nsduh_suic_try_linear.R / nsduh_matched_suic_try*.R
            suic_try_12mos  = forcats::fct_relevel(suic_try_12mos,  'No', 'Yes'),

            # from nsduh_global_health.R / nsduh_matched_global_health.R
            #

            global_health_dich = fct_collapse(
                global_health,
                # Note the ordering is intentional - want focal condition to be poor health
                good = c('Good', 'Very good', 'Excellent'),
                poor = c('Poor', 'Fair')
            )
        )
}


#' Load the full (unmatched) NSDUH sample, adults only.
#' @return data.frame with survey weights in column `weights`.
load_nsduh_data <- function() {

    path <- file.path(DATA_DIR, NSDUH_DATA_SUBDIR,
                      glue("nsduh_study_data_{NSDUH_DATA_VERSION}.RData"))
    if (!file.exists(path)) {
        stop(glue("NSDUH data not found: {path}\n",
                  "  -> see NOTES in R/data_prep.R; the recovered scripts used ",
                  "three different paths for this file."))
    }
    load(path)   # -> nsduh_study_df (+ large NSDUH_*_df_list objects)

    rm(list = ls()[stringr::str_starts(ls(), 'NSDUH_.*_df_list')])

    out <- nsduh_study_df %>%
        filter(adult_mask == 1) %>%     # adults filter: every unmatched script
        .nsduh_derive()

    cat(glue("NSDUH (unmatched) loaded: {nrow(out)} adult rows from {path}\n\n"))
    out
}


#' Load the matched NSDUH sample.
#' @return data.frame with survey weights in column `normalized_weights`.
load_nsduh_matched_data <- function() {

    path <- file.path(DATA_DIR, NSDUH_DATA_SUBDIR,
                      glue("nsduh_matched_study_data_{NSDUH_DATA_VERSION}.RData"))
    if (!file.exists(path)) {
        stop(glue("NSDUH matched data not found: {path}"))
    }
    load(path)   # -> nsduh_matched_df


    out <- .nsduh_derive(nsduh_matched_df)

    cat(glue("NSDUH (matched) loaded: {nrow(out)} rows from {path}\n\n"))
    out
}

