
BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh/project_config.R'
if (!file.exists(BASE_FILE)) stop(sprintf("Missing project config: %s", BASE_FILE))
source(BASE_FILE)

library(tidyverse)
library(glue)

source(file.path(R_DIR, "modeling_mod.R"))   
source(file.path(R_DIR, "data_prep.R"))

stopifnot("lv2_mod_var" %in% names(formals(logistic_model_wrapper)))

SKIP_EXISTING <- TRUE

grads_model_base <- load_ncha_data()


ID_VAR       <- "school_id"
LV1_RAN_VARS <- "c_Time"
LV2_INT_VARS <- c("school_size", "public_schl")
COVARIATES   <- c("c_Q46_age", "Q47_gender", "race_ethn",
                  "Q52_enrollment", "Q55_international", "survey_method")
CONTROL_LIST <- list(adapt_delta = .95)
WARMUP <- 5000; ITER <- 10000; CHAINS <- 4


build_lv1 <- function(quadratic) {
    c(if (quadratic) c("c_Time", "quad_c_Time") else "c_Time", COVARIATES)
}

COV_RHS <- paste("c_Q46_age + Q47_gender + race_ethn + Q52_enrollment +",
                 "Q55_international + survey_method + (1 + c_Time | school_id) +",
                 "school_size + public_schl")

expected_formula <- function(y, quadratic) {
    paste(y, "~", if (quadratic) "1 + c_Time + quad_c_Time" else "1 + c_Time", "+", COV_RHS)
}


MODELS <- list(
    list(save_name = "anxious_all_covariate",
         y_var = "Q30G_anxiety_r_2wks",    quadratic = TRUE,  verified = TRUE),
    list(save_name = "depressed_all_covariate",
         y_var = "Q30F_depressed_r_2wks",  quadratic = TRUE,  verified = TRUE),
    list(save_name = "dx_anxiety_all_covariate",
         y_var = "Q31A_anxiety_dich",      quadratic = TRUE,  verified = TRUE),
    list(save_name = "dx_depression_all_covariate",
         y_var = "Q31A_depression_dich",   quadratic = TRUE,  verified = TRUE),
    list(save_name = "dx_nsduh_any_all_covariate",
         y_var = "any_dx_nsduh",           quadratic = TRUE,  verified = TRUE),

    # Suicide attempt is fit twice: linear-only and linear+quadratic.
    list(save_name = "dx_suic_try_all_cov_linear",
         y_var = "Q30K_suic_try_r_12mos",  quadratic = FALSE, verified = TRUE),
    list(save_name = "suic_try_full_covariate",
         y_var = "Q30K_suic_try_r_12mos",  quadratic = TRUE,  verified = FALSE),

    list(save_name = "global_health_full_covariate",
         y_var = "global_health_dich",     quadratic = TRUE,  verified = TRUE),
    list(save_name = "suic_thnk_full_covariate",
         y_var = "Q30J_suic_thnk_r_12mos", quadratic = TRUE,  verified = TRUE),

    list(save_name = "neg_emo_any_all_cov",
         y_var = "neg_emo_any",            quadratic = TRUE,  verified = FALSE)
)

cat("\n================ PRE-FLIGHT ================\n")

stopifnot(ID_VAR %in% names(grads_model_base))
stopifnot(all(LV2_INT_VARS %in% names(grads_model_base)))
stopifnot(all(COVARIATES   %in% names(grads_model_base)))
stopifnot(all(c("c_Time", "quad_c_Time") %in% names(grads_model_base)))

missing_y <- vapply(MODELS, function(m) !(m$y_var %in% names(grads_model_base)), logical(1))
if (any(missing_y)) {
    stop("Outcome(s) not found in grads_model_base: ",
         paste(unique(vapply(MODELS[missing_y], `[[`, character(1), "y_var")), collapse = ", "))
}

for (m in MODELS) {
    built <- paste0(m$y_var, " ~ ", paste(c(1, build_lv1(m$quadratic)), collapse = " + "),
                    " + (", paste(c(1, LV1_RAN_VARS), collapse = " + "), " | ", ID_VAR, ")",
                    " + ", paste(LV2_INT_VARS, collapse = " + "))
    want  <- expected_formula(m$y_var, m$quadratic)
    if (!identical(gsub("\\s+", " ", built), gsub("\\s+", " ", want))) {
        stop(glue("FORMULA MISMATCH for {m$save_name}\n  built: {built}\n  want : {want}"))
    }
    cat(sprintf("  [ok] %-32s %s\n", m$save_name,
                if (m$verified) "(verified vs original script)" else "(reconstructed from report)"))
}
cat("All 10 specifications match the published formulas.\n")
cat("============================================\n\n")

results <- list()

for (m in MODELS) {
    out_file <- file.path(POSTERIOR_OUTPUTS, paste0(m$save_name, ".rds"))
    if (SKIP_EXISTING && file.exists(out_file)) {
        cat(sprintf("[skip] %s — output already exists.\n", m$save_name)); next
    }
    cat(sprintf("\n---- Fitting %s (%s) ----\n", m$save_name, m$y_var))

    results[[m$save_name]] <- logistic_model_wrapper(
        data            = grads_model_base,
        y_var           = m$y_var,
        id_var          = ID_VAR,
        lv1_vars        = build_lv1(m$quadratic),
        lv1_ran_vars    = LV1_RAN_VARS,
        lv2_int_vars    = LV2_INT_VARS,
        lv2_mod_var     = NULL,
        output_folder   = POSTERIOR_OUTPUTS,
        warmup          = WARMUP,
        iter            = ITER,
        chains          = CHAINS,
        control_list    = CONTROL_LIST,
        model_save_name = m$save_name
    )
}


