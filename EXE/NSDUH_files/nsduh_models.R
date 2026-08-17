
BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh/project_config.R'
if (!file.exists(BASE_FILE)) stop(sprintf("Missing project config: %s", BASE_FILE))
source(BASE_FILE)

library(tidyverse)
library(glue)

source(file.path(R_DIR, "modeling_mod.R"))     
source(file.path(R_DIR, "modeling_weighted.R")) 
source(file.path(R_DIR, "data_prep.R"))

SKIP_EXISTING <- TRUE

nsduh_adults_df  <- load_nsduh_data()          # weights column: `weights`
nsduh_matched_df <- load_nsduh_matched_data()  # weights column: `normalized_weights`

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

CONTROL_LIST <- list(adapt_delta = .95)
WARMUP <- 5000; ITER <- 10000; CHAINS <- 4

build_lv1 <- function(quadratic) if (quadratic) c("c_Time", "quad_c_Time") else "c_Time"

MODELS <- list()
for (s in SAMPLES) for (o in OUTCOMES) {
    MODELS[[length(MODELS) + 1]] <- list(
        save_name   = paste0("nsduh_", s$tag, o$y_var, o$suffix),
        y_var       = o$y_var,
        quadratic   = o$quadratic,
        weights_var = s$weights_var,
        data        = s$data
    )
}
stopifnot(length(MODELS) == 8)

expected_formula <- function(y, weights_var, quadratic) {
    paste0(y, " | weights(", weights_var, ") ~ ",
           paste(c(1, build_lv1(quadratic)), collapse = " + "))
}

PUBLISHED <- c(
    nsduh_anymi_12mos                   = "anymi_12mos | weights(weights) ~ 1 + c_Time + quad_c_Time",
    nsduh_global_health_dich            = "global_health_dich | weights(weights) ~ 1 + c_Time + quad_c_Time",
    nsduh_suic_thnk_12mos               = "suic_thnk_12mos | weights(weights) ~ 1 + c_Time + quad_c_Time",
    nsduh_suic_try_12mos_linear         = "suic_try_12mos | weights(weights) ~ 1 + c_Time",
    nsduh_matched_anymi_12mos           = "anymi_12mos | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time",
    nsduh_matched_global_health_dich    = "global_health_dich | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time",
    nsduh_matched_suic_thnk_12mos       = "suic_thnk_12mos | weights(normalized_weights) ~ 1 + c_Time + quad_c_Time",
    nsduh_matched_suic_try_12mos_linear = "suic_try_12mos | weights(normalized_weights) ~ 1 + c_Time"
)


for (m in MODELS) {
    d <- m$data
    for (v in c(m$y_var, m$weights_var, "c_Time", "quad_c_Time")) {
        if (!v %in% names(d)) stop(glue("`{v}` not found in data for {m$save_name}"))
    }
    built <- expected_formula(m$y_var, m$weights_var, m$quadratic)
    want  <- PUBLISHED[[m$save_name]]
    if (is.null(want)) stop(glue("No published formula on record for {m$save_name}"))
    if (!identical(built, want)) {
        stop(glue("FORMULA MISMATCH for {m$save_name}\n  built: {built}\n  published: {want}"))
    }
    cat(sprintf("  [ok] %-36s %s\n", m$save_name, built))
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
        lv2_mod_var     = NULL,
        output_folder   = POSTERIOR_OUTPUTS,
        warmup          = WARMUP,
        iter            = ITER,
        chains          = CHAINS,
        control_list    = CONTROL_LIST,
        model_save_name = m$save_name
    )
}


