
BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh/project_config.R'
if (!file.exists(BASE_FILE)) stop(sprintf("Missing project config: %s", BASE_FILE))
source(BASE_FILE)

library(tidyverse)
library(glue)

source(file.path(R_DIR, "modeling_mod.R"))       
source(file.path(R_DIR, "modeling_weighted.R"))  
source(file.path(R_DIR, "data_prep.R"))

SKIP_EXISTING <- TRUE

nsduh_adults_df  <- load_nsduh_data()
nsduh_matched_df <- load_nsduh_matched_data()

SAMPLES <- list(
    list(tag = "",         data = nsduh_adults_df,  weights_var = "weights"),
    list(tag = "matched_", data = nsduh_matched_df, weights_var = "normalized_weights")
)

OUTCOMES <- list(
    list(y_var = "anymi_12mos",        quadratic = TRUE,  suffix = ""),
    list(y_var = "global_health_dich", quadratic = TRUE,  suffix = ""),
    list(y_var = "suic_thnk_12mos",    quadratic = TRUE,  suffix = ""),
    list(y_var = "suic_try_12mos",     quadratic = FALSE, suffix = "_linear")
)

MODERATORS <- c("race", "sex")

CONTROL_LIST <- list(adapt_delta = .95)
WARMUP <- 5000; ITER <- 10000; CHAINS <- 4

build_lv1 <- function(quadratic) if (quadratic) c("c_Time", "quad_c_Time") else "c_Time"

MODELS <- list()
for (md in MODERATORS) for (s in SAMPLES) for (o in OUTCOMES) {
    MODELS[[length(MODELS) + 1]] <- list(
        save_name   = paste0("nsduh_", s$tag, o$y_var, o$suffix, "_mod_", md),
        y_var       = o$y_var,
        quadratic   = o$quadratic,
        weights_var = s$weights_var,
        mod_var     = md,
        data        = s$data
    )
}
stopifnot(length(MODELS) == 16)

expected_formula <- function(y, weights_var, quadratic, mod) {
    lv1 <- build_lv1(quadratic)
    paste0(y, " | weights(", weights_var, ") ~ ",
           paste(c(1, lv1), collapse = " + "),
           " + ", mod, " + ", paste(paste0(lv1, ":", mod), collapse = " + "))
}

PUBLISHED <- c(
    nsduh_anymi_12mos_mod_race                   = "anymi_12mos | weights(weights) ~ 1 + c_Time + quad_c_Time + race + c_Time:race + quad_c_Time:race",
    nsduh_global_health_dich_mod_race            = "global_health_dich | weights(weights) ~ 1 + c_Time + quad_c_Time + race + c_Time:race + quad_c_Time:race",
    nsduh_suic_thnk_12mos_mod_race               = "suic_thnk_12mos | weights(weights) ~ 1 + c_Time + quad_c_Time + race + c_Time:race + quad_c_Time:race",
    nsduh_suic_try_12mos_linear_mod_race         = "suic_try_12mos | weights(weights) ~ 1 + c_Time + race + c_Time:race",
    nsduh_matched_anymi_12mos_mod_race           = "anymi_12mos | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time + race + c_Time:race + quad_c_Time:race",
    nsduh_matched_global_health_dich_mod_race    = "global_health_dich | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time + race + c_Time:race + quad_c_Time:race",
    nsduh_matched_suic_thnk_12mos_mod_race       = "suic_thnk_12mos | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time + race + c_Time:race + quad_c_Time:race",
    nsduh_matched_suic_try_12mos_linear_mod_race = "suic_try_12mos | weights(normalized_weights) ~ 1 + c_Time + race + c_Time:race",

    nsduh_anymi_12mos_mod_sex                    = "anymi_12mos | weights(weights) ~ 1 + c_Time + quad_c_Time + sex + c_Time:sex + quad_c_Time:sex",
    nsduh_global_health_dich_mod_sex             = "global_health_dich | weights(weights) ~ 1 + c_Time + quad_c_Time + sex + c_Time:sex + quad_c_Time:sex",
    nsduh_suic_thnk_12mos_mod_sex                = "suic_thnk_12mos | weights(weights) ~ 1 + c_Time + quad_c_Time + sex + c_Time:sex + quad_c_Time:sex",
    nsduh_suic_try_12mos_linear_mod_sex          = "suic_try_12mos | weights(weights) ~ 1 + c_Time + sex + c_Time:sex",
    nsduh_matched_anymi_12mos_mod_sex            = "anymi_12mos | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time + sex + c_Time:sex + quad_c_Time:sex",
    nsduh_matched_global_health_dich_mod_sex     = "global_health_dich | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time + sex + c_Time:sex + quad_c_Time:sex",
    nsduh_matched_suic_thnk_12mos_mod_sex        = "suic_thnk_12mos | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time + sex + c_Time:sex + quad_c_Time:sex",
    nsduh_matched_suic_try_12mos_linear_mod_sex  = "suic_try_12mos | weights(normalized_weights) ~ 1 + c_Time + sex + c_Time:sex"
)


for (m in MODELS) {
    for (v in c(m$y_var, m$weights_var, m$mod_var, "c_Time", "quad_c_Time")) {
        if (!v %in% names(m$data)) stop(glue("`{v}` not found in data for {m$save_name}"))
    }
    built <- expected_formula(m$y_var, m$weights_var, m$quadratic, m$mod_var)
    want  <- PUBLISHED[[m$save_name]]
    if (is.null(want)) stop(glue("No published formula on record for {m$save_name}"))
    if (!identical(built, want)) {
        stop(glue("FORMULA MISMATCH for {m$save_name}\n  built: {built}\n  published: {want}"))
    }
    cat(sprintf("  [ok] %-46s\n", m$save_name))
}


results <- list()

for (m in MODELS) {
    out_file <- file.path(POSTERIOR_OUTPUTS, paste0(m$save_name, ".RData"))
    if (SKIP_EXISTING && file.exists(out_file)) {
        cat(sprintf("[skip] %s\n", m$save_name)); next
    }
    cat(sprintf("\n---- Fitting %s ----\n", m$save_name))

    results[[m$save_name]] <- weighted_logistic_wrapper(
        data            = m$data,
        y_var           = m$y_var,
        weights_var     = m$weights_var,
        lv1_vars        = build_lv1(m$quadratic),
        lv2_mod_var     = m$mod_var,
        output_folder   = POSTERIOR_OUTPUTS,
        warmup          = WARMUP,
        iter            = ITER,
        chains          = CHAINS,
        control_list    = CONTROL_LIST,
        model_save_name = m$save_name
    )
}

