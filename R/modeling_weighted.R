
library(tidyverse)

#' Weighted logistic (bernoulli) regression wrapper for the NSDUH analyses.
#'
#' Distinct from logistic_model_wrapper() in two ways, both dictated by the
#' published models: the outcome carries a `weights()` addition term (survey
#' weights), and there are NO random effects — NSDUH is a repeated
#' cross-section with no school/cluster grouping, so there is no id_var.
#'
#' @param data data.frame containing the training data.
#' @param y_var character label for the outcome variable.
#' @param weights_var character label of the survey-weight column. In the
#'   published models this is "weights" for the full NSDUH samples and
#'   "normalized_weights" for the matched samples.
#' @param lv1_vars character vector of predictors. An explicit intercept is
#'   prepended, reproducing the published formulas.
#' @param lv2_mod_var character, optional. A moderator. When supplied, its main
#'   effect plus its interaction with every lv1_var is appended — matching the
#'   16 published NSDUH moderation models.
#' @param output_folder character filepath for saved output.
#' @param warmup,iter,chains numeric sampler settings. Published: 5000 / 10000 / 4.
#' @param control_list list passed to brms::brm(). Published: adapt_delta = .95.
#' @param prior brmsprior, optional. Defaults to the published NSDUH priors:
#'   normal(0, 3) on b and normal(0, 3) on Intercept.
#' @param model_save_name character; the .RData basename. LOAD-BEARING — the
#'   Rmd reads models by these exact filenames.
#' @param future_arg boolean, optional.

weighted_logistic_wrapper <- function(data, y_var, weights_var, lv1_vars,
                                      lv2_mod_var = NULL, output_folder,
                                      warmup, iter, chains, control_list,
                                      prior = NULL, model_save_name = NULL,
                                      future_arg = FALSE) {

    start_time <- Sys.time()

    # Field order here is load-bearing: it reproduces names(results_list) from
    # the published outputs. Do not reorder.
    results_list <- list()
    results_list[["model_data"]]    <- data
    results_list[["model_success"]] <- FALSE
    results_list[["error_message"]] <- NULL   # no-op: <- NULL deletes. Kept for fidelity.

    model_id <- ifelse(!is.null(model_save_name), model_save_name, "unnamed_weighted_model")
    cat("Fitting model:", model_id, "\n")

    # ── Formula ──────────────────────────────────────────────────────────────
    tryCatch({
        model <- "{y_var} | weights({weights_var}) ~ {paste(c(1, lv1_vars), collapse=' + ')}" %>%
            glue::glue()

        if (!is.null(lv2_mod_var)) {
            model <- "{model} + {lv2_mod_var}" %>% glue::glue()
            interactions <- paste0(lv1_vars, ":", lv2_mod_var)
            model <- "{model} + {paste(interactions, collapse=' + ')}" %>% glue::glue()
        }

        model <- model %>% brms::bf() + brms::bernoulli()
        results_list[["brms_formula"]] <- model

    }, error = function(e) {
        cat("x Error constructing model formula:", e$message, "\n")
        results_list[["error_message"]] <<- paste("Formula construction error:", e$message)
        return(results_list)
    })

    # ── Priors ───────────────────────────────────────────────────────────────
    # Unlike the NCHA models (normal(0,2)/lognormal/lkj), the published NSDUH
    # models used normal(0,3) on BOTH b and Intercept. With no random effects
    # there are no sd or cor classes to set.
    if (is.null(prior)) {
        prior <- c(brms::set_prior("normal(0, 3)", class = "Intercept"),
                   brms::set_prior("normal(0, 3)", class = "b"))
    }
    cat("Prior object being passed to brm():\n")
    print(prior)
    cat("\n")

    # ── Fit ──────────────────────────────────────────────────────────────────
    tryCatch({
        fit <- brms::brm(
            model,
            data    = data,
            prior   = prior,
            iter    = iter,
            warmup  = warmup,
            chains  = chains,
            cores   = chains,
            control = control_list,
            future  = future_arg
        )

        run_time <- difftime(Sys.time(), start_time, units = "hours")
        results_list[["run_time"]]      <- run_time
        results_list[["brms_fit"]]      <- fit
        results_list[["model_success"]] <- TRUE
        results_list[["brms_priors"]]   <- brms::prior_summary(fit)
        results_list[["diagnostics"]]   <- extract_diagnostics(fit)

        cat("v Model completed successfully in", round(as.numeric(run_time), 2), "hours\n")

    }, error = function(e) {
        run_time <- difftime(Sys.time(), start_time, units = "hours")
        results_list[["run_time"]] <<- run_time

        cat("x Model fitting failed:", e$message, "\n")
        results_list[["error_message"]]   <<- paste("Model fitting error:", e$message)
        results_list[["error_traceback"]] <<- capture.output(traceback())

        if (!is.null(model_save_name)) {
            error_file <- "{output_folder}/{model_save_name}_ERROR.RData" %>% glue::glue()
            save(list = c('results_list'), file = error_file)
            cat("  Error results saved to:", error_file, "\n")
        }
        return(results_list)
    })

    # ── Save ─────────────────────────────────────────────────────────────────
    if (results_list[["model_success"]] && !is.null(model_save_name)) {
        tryCatch({
            # [DEVIATION — one line, intentional]
            # Every original wrapper assigned model_name AFTER save(), so the
            # saved object never contained it. That is why the published report
            # prints no "Model Name:" line for any of the 76 models. Assigning
            # before save() fixes the bug; the consequence is that a re-rendered
            # report will now show "Model Name:" per model. To reproduce the
            # published report exactly, move this line back below save().
            results_list[["model_name"]] <- model_save_name

            save(list = c('results_list'),
                 file = "{output_folder}/{model_save_name}.RData" %>% glue::glue())
        }, error = function(e) {
            cat("x Failed to save model results:", e$message, "\n")
            results_list[["save_error"]] <<- e$message
        })
    }

    return(results_list)
}
