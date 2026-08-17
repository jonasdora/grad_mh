

BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh/project_config.R'
if (!file.exists(BASE_FILE)) stop(sprintf("Missing project config: %s", BASE_FILE))
source(BASE_FILE)   # -> DATA_DIR, R_DIR, POSTERIOR_OUTPUTS

library(tidyverse)
library(glue)

source(file.path(R_DIR, "modeling_mod.R"))
source(file.path(R_DIR, "data_prep.R"))

stopifnot("lv2_mod_var" %in% names(formals(logistic_model_wrapper)))

SKIP_EXISTING <- TRUE   

grads_model_base <- load_ncha_data()


ID_VAR       <- "school_id"
LV1_RAN_VARS <- "c_Time"
LV2_INT_VARS <- NULL          # no covariates in this script
CONTROL_LIST <- list(adapt_delta = .95)
WARMUP <- 5000; ITER <- 10000; CHAINS <- 4

build_lv1 <- function(quadratic) if (quadratic) c("c_Time", "quad_c_Time") else "c_Time"

expected_formula <- function(y, quadratic) {
    paste0(y, " ~ ", paste(c(1, build_lv1(quadratic)), collapse = " + "),
           " + (1 + ", LV1_RAN_VARS, " | ", ID_VAR, ")")
}


MODELS <- list(
    list(save_name = "anxious_no_cov",            y_var = "Q30G_anxiety_r_2wks",    quadratic = TRUE,  published = TRUE),
    list(save_name = "depressed_no_cov",          y_var = "Q30F_depressed_r_2wks",  quadratic = TRUE,  published = TRUE),
    list(save_name = "dx_anxiety_no_cov",         y_var = "Q31A_anxiety_dich",      quadratic = TRUE,  published = TRUE),
    list(save_name = "dx_depression_no_cov",      y_var = "Q31A_depression_dich",   quadratic = TRUE,  published = TRUE),
    list(save_name = "dx_nsduh_any_no_cov",       y_var = "any_dx_nsduh",           quadratic = TRUE,  published = TRUE),
    list(save_name = "dx_suic_thnk_no_cov",       y_var = "Q30J_suic_thnk_r_12mos", quadratic = TRUE,  published = TRUE),

    # Suicide attempt is fit twice: linear-only and linear+quadratic.
    list(save_name = "dx_suic_try_no_cov_linear", y_var = "Q30K_suic_try_r_12mos",  quadratic = FALSE, published = TRUE),
    list(save_name = "dx_suic_try_no_cov",        y_var = "Q30K_suic_try_r_12mos",  quadratic = TRUE,  published = TRUE),

    list(save_name = "global_health_dich_no_cov", y_var = "global_health_dich",     quadratic = TRUE,  published = TRUE),
    list(save_name = "neg_emo_any_no_cov",        y_var = "neg_emo_any",            quadratic = TRUE,  published = TRUE),

    list(save_name = "hopeless_no_cov",           y_var = "Q30A_hopeless_r_2wks",   quadratic = TRUE,  published = FALSE),  # "Hopeless"
    list(save_name = "anger_no_cov",              y_var = "Q30H_anger_r_2wks",      quadratic = TRUE,  published = FALSE),  # "Anger"
    list(save_name = "lonely_no_cov",             y_var = "Q30D_lonely_r_2wks",     quadratic = TRUE,  published = FALSE),  # "Lonely"
    list(save_name = "sad_no_cov",                y_var = "Q30E_sad_r_2wks",        quadratic = TRUE,  published = FALSE),  # "Sad"

    list(save_name = "exhausted_item_no_cov_linear", y_var = "Q30C_exhausted_r_2wks",   quadratic = FALSE, published = FALSE),
    list(save_name = "exhausted_item_no_cov",        y_var = "Q30C_exhausted_r_2wks",   quadratic = TRUE,  published = FALSE),
    list(save_name = "ovrwhlm_item_no_cov_linear",   y_var = "Q30B_overwhelmed_r_2wks", quadratic = FALSE, published = FALSE),
    list(save_name = "ovrwhlm_item_no_cov",          y_var = "Q30B_overwhelmed_r_2wks", quadratic = TRUE,  published = FALSE),

    list(save_name = "dx_panic_no_cov",           y_var = "Q31B_panic_dich",        quadratic = TRUE,  published = FALSE),  # "Panic Attacks"
    list(save_name = "dx_bipolar_no_cov",         y_var = "Q31A_bipolar_dich",      quadratic = TRUE,  published = FALSE),  # "Bipolar Disorder"
    list(save_name = "dx_schizo_no_cov",          y_var = "Q31B_schizo_dich",       quadratic = TRUE,  published = FALSE)   # "Schizophrenia"
)



stopifnot(ID_VAR %in% names(grads_model_base))
stopifnot(all(c("c_Time", "quad_c_Time") %in% names(grads_model_base)))

missing_y <- vapply(MODELS, function(m) !(m$y_var %in% names(grads_model_base)), logical(1))
if (any(missing_y)) {
    stop("Outcome(s) not found in grads_model_base: ",
         paste(unique(vapply(MODELS[missing_y], `[[`, character(1), "y_var")), collapse = ", "))
}

PUBLISHED <- c(
    anxious_no_cov            = "Q30G_anxiety_r_2wks ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    depressed_no_cov          = "Q30F_depressed_r_2wks ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    dx_anxiety_no_cov         = "Q31A_anxiety_dich ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    dx_depression_no_cov      = "Q31A_depression_dich ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    dx_nsduh_any_no_cov       = "any_dx_nsduh ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    dx_suic_thnk_no_cov       = "Q30J_suic_thnk_r_12mos ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    dx_suic_try_no_cov_linear = "Q30K_suic_try_r_12mos ~ 1 + c_Time + (1 + c_Time | school_id)",
    dx_suic_try_no_cov        = "Q30K_suic_try_r_12mos ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    global_health_dich_no_cov = "global_health_dich ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)",
    neg_emo_any_no_cov        = "neg_emo_any ~ 1 + c_Time + quad_c_Time + (1 + c_Time | school_id)"
)

for (m in MODELS) {
    built <- expected_formula(m$y_var, m$quadratic)
    if (isTRUE(m$published)) {
        want <- PUBLISHED[[m$save_name]]
        if (is.null(want)) stop(glue("No published formula on record for {m$save_name}"))
        if (!identical(built, want)) {
            stop(glue("FORMULA MISMATCH for {m$save_name}\n  built: {built}\n  published: {want}"))
        }
        cat(sprintf("  [reproduce] %-30s %s\n", m$save_name, built))
    } else {
        cat(sprintf("  [NEW]       %-30s %s\n", m$save_name, built))
    }
}
n_pub <- sum(vapply(MODELS, function(m) isTRUE(m$published), logical(1)))

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
        lv2_int_vars    = LV2_INT_VARS,   # NULL -> no covariate block appended
        lv2_mod_var     = NULL,
        output_folder   = POSTERIOR_OUTPUTS,
        warmup          = WARMUP,
        iter            = ITER,
        chains          = CHAINS,
        control_list    = CONTROL_LIST,
        model_save_name = m$save_name
    )
}

