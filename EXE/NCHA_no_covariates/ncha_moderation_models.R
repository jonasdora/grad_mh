
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
CONTROL_LIST <- list(adapt_delta = .95)
WARMUP <- 5000; ITER <- 10000; CHAINS <- 4

build_lv1 <- function(quadratic) if (quadratic) c("c_Time", "quad_c_Time") else "c_Time"

OUTCOMES <- list(
    list(base = "anxious_no_cov",            y_var = "Q30G_anxiety_r_2wks",    quadratic = TRUE),
    list(base = "depressed_no_cov",          y_var = "Q30F_depressed_r_2wks",  quadratic = TRUE),
    list(base = "dx_anxiety_no_cov",         y_var = "Q31A_anxiety_dich",      quadratic = TRUE),
    list(base = "dx_depression_no_cov",      y_var = "Q31A_depression_dich",   quadratic = TRUE),
    list(base = "dx_nsduh_any_no_cov",       y_var = "any_dx_nsduh",           quadratic = TRUE),
    list(base = "dx_suic_thnk_no_cov",       y_var = "Q30J_suic_thnk_r_12mos", quadratic = TRUE),
    list(base = "dx_suic_try_no_cov_linear", y_var = "Q30K_suic_try_r_12mos",  quadratic = FALSE),
    list(base = "global_health_dich_no_cov", y_var = "global_health_dich",     quadratic = TRUE),

    list(base = "neg_emo_any_no_cov",        y_var = "neg_emo_any",            quadratic = TRUE)
)


MODERATORS <- list(
    list(tag = "enrollment",    var = "Q52_enrollment"),     # ref = Full-time; other = Part-time
    list(tag = "international", var = "Q55_international"),   # ref = No;        other = Yes

    list(tag = "race",          var = "Q54_white"),           # ref = 0 (POC);   other = 1 (White)


    list(tag = "race",          var = "Q54_poc"),               # ref = White;     other = POC


    list(tag = "sex",           var = "Q47_gender_binary")    # ref = Female;    other = Male
)

MODELS <- list()
for (o in OUTCOMES) for (md in MODERATORS) {
    MODELS[[length(MODELS) + 1]] <- list(
        save_name = paste0(o$base, "_mod_", md$tag),
        y_var = o$y_var, quadratic = o$quadratic, mod_var = md$var,

        reproduces_published = (md$tag %in% c("enrollment", "international")) &&
                               (o$base != "neg_emo_any_no_cov")
    )
}
stopifnot(length(MODELS) == 36)


expected_formula <- function(y, quadratic, mod) {
    lv1 <- build_lv1(quadratic)
    paste0(y, " ~ ", paste(c(1, lv1), collapse = " + "),
           " + (1 + ", LV1_RAN_VARS, " | ", ID_VAR, ")",
           " + ", mod, " + ", paste(paste0(lv1, ":", mod), collapse = " + "))
}


stopifnot(ID_VAR %in% names(grads_model_base))
stopifnot(all(c("c_Time", "quad_c_Time") %in% names(grads_model_base)))

need <- unique(c(vapply(MODELS, `[[`, character(1), "y_var"),
                 vapply(MODELS, `[[`, character(1), "mod_var")))
absent <- need[!need %in% names(grads_model_base)]
if (length(absent)) stop("Variable(s) not found in grads_model_base: ",
                         paste(absent, collapse = ", "))

PUBLISHED_MOD <- local({
    bases <- list(
        anxious_no_cov            = c("Q30G_anxiety_r_2wks",    TRUE),
        depressed_no_cov          = c("Q30F_depressed_r_2wks",  TRUE),
        dx_anxiety_no_cov         = c("Q31A_anxiety_dich",      TRUE),
        dx_depression_no_cov      = c("Q31A_depression_dich",   TRUE),
        dx_nsduh_any_no_cov       = c("any_dx_nsduh",           TRUE),
        dx_suic_thnk_no_cov       = c("Q30J_suic_thnk_r_12mos", TRUE),
        dx_suic_try_no_cov_linear = c("Q30K_suic_try_r_12mos",  FALSE),
        global_health_dich_no_cov = c("global_health_dich",     TRUE)
    )

    pub_mods <- c(enrollment = "Q52_enrollment",
                  international = "Q55_international")
    out <- list()
    for (bn in names(bases)) for (mt in names(pub_mods)) {
        y <- bases[[bn]][1]; quad <- as.logical(bases[[bn]][2])
        out[[paste0(bn, "_mod_", mt)]] <- expected_formula(y, quad, pub_mods[[mt]])
    }
    out
})

n_reproduce <- 0L; n_new <- 0L
for (m in MODELS) {
    built <- expected_formula(m$y_var, m$quadratic, m$mod_var)
    want  <- PUBLISHED_MOD[[m$save_name]]
    if (isTRUE(m$reproduces_published)) {
        if (is.null(want)) stop(glue("Model {m$save_name} is flagged as reproducing a ",
                                     "published model but no published formula is on record."))
        if (!identical(built, want)) {
            stop(glue("FORMULA MISMATCH for {m$save_name}\n  built    : {built}\n  published: {want}"))
        }
        cat(sprintf("  [reproduce] %-42s\n", m$save_name)); n_reproduce <- n_reproduce + 1L
    } else {
        cat(sprintf("  [NEW/CORR]  %-42s %s\n", m$save_name, built)); n_new <- n_new + 1L
    }
}


results <- list()

for (m in MODELS) {
    out_file <- file.path(POSTERIOR_OUTPUTS, paste0(m$save_name, ".rds"))
    if (SKIP_EXISTING && file.exists(out_file)) {
        cat(sprintf("[skip] %s\n", m$save_name)); next
    }
    cat(sprintf("\n---- Fitting %s ----\n", m$save_name))
    cat("     ", expected_formula(m$y_var, m$quadratic, m$mod_var), "\n")

    results[[m$save_name]] <- logistic_model_wrapper(
        data            = grads_model_base,
        y_var           = m$y_var,
        id_var          = ID_VAR,
        lv1_vars        = build_lv1(m$quadratic),
        lv1_ran_vars    = LV1_RAN_VARS,
        lv2_int_vars    = NULL,
        lv2_mod_var     = m$mod_var,
        output_folder   = POSTERIOR_OUTPUTS,
        warmup          = WARMUP,
        iter            = ITER,
        chains          = CHAINS,
        control_list    = CONTROL_LIST,
        model_save_name = m$save_name
    )
}


