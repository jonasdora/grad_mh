library(tidyverse)

#' logistic model wrapper is a function that wraps brms functions
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing). This function is the primary model
#' function used for the ACHA-NCHA graduate student analyses
#' 
#' @param data data.frame containing the training data
#' 
#' @param prior_config list containing configuration for the model parameter's priors
#' 
#' @param y_var character label for the y variable in the data.frame
#' 
#' @param id_var character label for the group identity variable - used to define the levels of random effects
#' 
#' @param lv1_vars character vector of labels for the predictors in the model at the "observation" level of the model. 
#' sometimes also referred to as the level-1 variables
#' 
#' @param lv1_ran_vars character vector of labels for the predictors free to vary across the id_var variable levels.
#' 
#' @param lv2_int_vars character vector of labels for predictors of the random intercept for each level of the id_var. 
#' These are variables measured at the level of the id_var.
#' 
#' @param lv2_mod_var character label (single variable) for a level-2 moderator that interacts with all lv1_vars.
#' This variable is measured at the level of the id_var and will create interaction terms with each lv1_var.
#' 
#' @param output_folder character filepath to location where outputs should be stored from the model
#' 
#' @param warmup numeric integer value for the number of warmup iterations. Passed to the brms model API.
#' 
#' @param iter numeric integer value for the number of total iterations. Passed to the brms model API.
#' 
#' @param chains numeric integer value for total number of MCMC chains to sample. Passed to the brms model API.
#' 
#' @param control_list list of additional arguments to configure the model fitting process. 
#' 
#' @param future_arg boolean, optional. If set to TRUE attempts to leverage additional parallelization of the 
#' MCMC computations. Default is set to FALSE.
#' 
#' @param model_save_name character filename to save the compiled Stan model produced when fitting a model with brms.
#' 
#' @param chain_hyper_threading boolean, optional. Default is FALSE which parallelizes computations for each chain
#' on a single core. If TRUE, the computations for each chain will be threaded - adding additional parallelization and speeding
#' compute times. TRUE is only recommended for multicore machines (recommend at least 12 cores). See this url for 
#' additional information: https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html.
#' 
#' @param max_threads integer, optional. The number of threads to use for each chain when chain_hyperthreading is TRUE.
#' Default value is 6 - adjust to what makes sense for your machine (running 3 chains, with a max number of 4 threads per
#' chain requires 12 cores.)

logistic_model_wrapper <- function(data, y_var, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                   lv2_mod_var = NULL,  # NEW PARAMETER
                                   output_folder, warmup, iter, chains, control_list, future_arg = FALSE, 
                                   prior=NULL, model_save_name=NULL, chain_hyperthreading=FALSE, max_threads=6){
    
    start_time <- Sys.time()
    
    # Initialize results list
    results_list <- list()
    results_list[["model_data"]] <- data
    results_list[["model_success"]] <- FALSE
    results_list[["error_message"]] <- NULL
    
    model_id <- ifelse(!is.null(model_save_name), model_save_name, "unnamed_model")
    cat("Fitting model:", model_id, "\n")
    
    # Build model formula
    tryCatch({
        model <- "
        {y_var} ~ {paste(c(1, lv1_vars), collapse=' + ')} +
        ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) 
        " %>% glue::glue() 
        
        if(!is.null(lv2_int_vars)) {
            model <- "{model} + {paste(lv2_int_vars, collapse=' + ')}" %>% glue::glue()
        }
        
        # Add level 2 moderator and interactions if provided
        if(!is.null(lv2_mod_var)) {
            # Add main effect of moderator
            model <- "{model} + {lv2_mod_var}" %>% glue::glue()
            
            # Add interactions between each lv1_var and the moderator
            interactions <- paste0(lv1_vars, ":", lv2_mod_var)
            model <- "{model} + {paste(interactions, collapse=' + ')}" %>% glue::glue()
        }
        
        model <- model %>% brms::bf() + brms::bernoulli()
        
        results_list[["brms_formula"]] <- model
        #results_list[["brms_priors"]] <- brms::get_prior(model, data)
        
    }, error = function(e) {
        cat("✗ Error constructing model formula:", e$message, "\n")
        results_list[["error_message"]] <<- paste("Formula construction error:", e$message)
        return(results_list)
    })
    
    if(is.null(prior)) {
        prior <- c(brms::set_prior("normal(0, 2)", class="b"),
                   brms::set_prior("lognormal(-.5, .5)", class="sd"),
                   brms::set_prior("lkj(2)", class="cor"))
    }
    cat("Prior object being passed to brm():\n")
    print(prior)
    cat("\n")
    # Fit the model with error handling
    tryCatch({
        if(chain_hyperthreading){
            fit <- brms::brm(
                model, 
                data = data, 
                prior=prior,
                iter = iter, 
                warmup = warmup,
                chains = chains, 
                cores = chains,
                backend = "cmdstanr",
                threads = threading(max_threads),
                control = control_list,
                future = future_arg, 
                save_model = ifelse(!is.null(model_save_name), model_save_name, NULL)
            )
        } else {
            fit <- brms::brm(
                model, 
                data = data, 
                prior=prior,
                iter = iter, 
                warmup = warmup,
                chains = chains, 
                cores = chains,
                control = control_list,
                future = future_arg, 
                save_model = ifelse(!is.null(model_save_name), model_save_name, NULL)
            )
        }
        
        run_time <- difftime(Sys.time(), start_time, units = "hours")
        results_list[["run_time"]] <- run_time
        results_list[["brms_fit"]] <- fit
        results_list[["model_success"]] <- TRUE
        results_list[["brms_priors"]] <- brms::prior_summary(fit)
        
        
        # Extract diagnostics for HTML reporting
        results_list[["diagnostics"]] <- extract_diagnostics(fit)
        
        cat("✓ Model completed successfully in", round(as.numeric(run_time), 2), "hours\n")
        
    }, error = function(e) {
        run_time <- difftime(Sys.time(), start_time, units = "hours")
        results_list[["run_time"]] <<- run_time
        
        cat("✗ Model fitting failed:", e$message, "\n")
        results_list[["error_message"]] <<- paste("Model fitting error:", e$message)
        results_list[["error_traceback"]] <<- capture.output(traceback())
        
        # Save partial results even on failure
        if(!is.null(model_save_name)) {
            error_file <- "{output_folder}/{model_save_name}_ERROR.rds" %>% glue::glue()
            saveRDS(results_list, file = error_file)
            cat("  Error results saved to:", error_file, "\n")
        }
        
        return(results_list)
    })
    
    # Save results if model fitting succeeded
    if(results_list[["model_success"]] && !is.null(model_save_name)) {
        tryCatch({
            saveRDS(results_list, 
                    file = "{output_folder}/{model_save_name}.rds" %>% glue::glue())
            results_list[["model_name"]] <- model_save_name
        }, error = function(e) {
            cat("✗ Failed to save model results:", e$message, "\n")
            results_list[["save_error"]] <<- e$message
        })
    }
    
    return(results_list)
}


#' ordinal regression using a cumulative log-odds model structure that wraps brms functions.
#' 
#' In addition to running the model and returning the output, the function also accepts several arguments that govern 
#' the saving of the model object and output files (required for post-processing). Currently this particular function
#' is only relevant for the global health variable - which is a single item 5-response measure tapping overall health.
#' 
#' The function uses a logit function for the log-odds and employs a flexible threshold (which ensures that not all 
#' transitions in rank from a lower to a higher value need to maintain the same log-odds). 
#' 
#' @param data data.frame containing the training data
#' 
#' @param prior_config list containing configuration for the model parameter's priors
#' 
#' @param y_var character label for the y variable in the data.frame
#' 
#' @param id_var character label for the group identity variable - used to define the levels of random effects
#' 
#' @param lv1_vars character vector of labels for the predictors in the model at the "observation" level of the model. 
#' sometimes also referred to as the level-1 variables
#' 
#' @param lv1_ran_vars character vector of labels for the predictors free to vary across the id_var variable levels.
#' 
#' @param lv2_int_vars character vector of labels for predictors of the random intercept for each level of the id_var. 
#' These are variables measured at the level of the id_var.
#' 
#' @param lv2_mod_var character label (single variable) for a level-2 moderator that interacts with all lv1_vars.
#' This variable is measured at the level of the id_var and will create interaction terms with each lv1_var.
#' 
#' @param output_folder character filepath to location where outputs should be stored from the model
#' 
#' @param warmup numeric integer value for the number of warmup iterations. Passed to the brms model API.
#' 
#' @param iter numeric integer value for the number of total iterations. Passed to the brms model API.
#' 
#' @param chains numeric integer value for total number of MCMC chains to sample. Passed to the brms model API.
#' 
#' @param control_list list of additional arguments to configure the model fitting process. 
#' 
#' @param future_arg boolean, optional. If set to TRUE attempts to leverage additional parallelization of the 
#' MCMC computations. Default is set to FALSE.
#' 
#' @param model_save_name character filename to save the compiled Stan model produced when fitting a model with brms.
#' 
#' @param chain_hyper_threading boolean, optional. Default is FALSE which parallelizes computations for each chain
#' on a single core. If TRUE, the computations for each chain will be threaded - adding additional parallelization and speeding
#' compute times. TRUE is only recommended for multicore machines (recommend at least 12 cores). See this url for 
#' additional information: https://cran.r-project.org/web/packages/brms/vignettes/brms_threading.html.
#' 
#' @param max_threads integer, optional. The number of threads to use for each chain when chain_hyperthreading is TRUE.
#' Default value is 6 - adjust to what makes sense for your machine (running 3 chains, with a max number of 4 threads per
#' chain requires 12 cores.)

ordinal_model_wrapper <- function(data, prior_config, y_var, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                  lv2_mod_var = NULL, output_folder, warmup, iter, chains, control_list, 
                                  future_arg = FALSE, model_save_name=NULL, chain_hyperthreading=FALSE, 
                                  max_threads=6){
  
  start_time <- Sys.time()
  
  # Initialize results list
  results_list <- list()
  results_list[["model_data"]] <- data
  results_list[["model_success"]] <- FALSE
  results_list[["error_message"]] <- NULL
  
  model_id <- ifelse(!is.null(model_save_name), model_save_name, "unnamed_ordinal_model")
  cat("Fitting model:", model_id, "\n")
  
  # Build model formula
  tryCatch({
    # Start with base fixed effects
    fixed_effects <- paste(c(1, lv1_vars), collapse=' + ')
    
    # Add level-2 intercept variables if present
    if(!is.null(lv2_int_vars)) {
      fixed_effects <- paste(fixed_effects, paste(lv2_int_vars, collapse=' + '), sep=' + ')
    }
    
    # Add moderator main effect and interactions if present
    if(!is.null(lv2_mod_var)) {
      # Add main effect of moderator
      fixed_effects <- paste(fixed_effects, lv2_mod_var, sep=' + ')
      
      # Add interactions between moderator and all level-1 variables
      interaction_terms <- paste(lv1_vars, lv2_mod_var, sep=':')
      fixed_effects <- paste(fixed_effects, paste(interaction_terms, collapse=' + '), sep=' + ')
    }
    
    # Build full formula with random effects
    model <- "
        {y_var} ~ {fixed_effects} +
        ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var}) 
        " %>% glue::glue() 
    
    model <- model %>% brms::bf() + cumulative(link = "logit", threshold = "flexible")
    
    results_list[["brms_formula"]] <- model
    results_list[["brms_priors"]] <- brms::get_prior(model, data)
    
  }, error = function(e) {
    cat("✗ Error constructing model formula:", e$message, "\n")
    results_list[["error_message"]] <<- paste("Formula construction error:", e$message)
    return(results_list)
  })
  
  # Fit the model with error handling
  tryCatch({
    if(chain_hyperthreading){
      fit <- brms::brm(
        model, 
        data = data, 
        iter = iter, 
        warmup = warmup,
        chains = chains, 
        cores = chains,
        backend = "cmdstanr",
        threads = threading(max_threads),
        control = control_list,
        future = future_arg, 
        save_model = ifelse(!is.null(model_save_name), model_save_name, NULL)
      )
    } else {
      fit <- brms::brm(
        model, 
        data = data, 
        iter = iter, 
        warmup = warmup,
        chains = chains, 
        cores = chains,
        control = control_list,
        future = future_arg, 
        save_model = ifelse(!is.null(model_save_name), model_save_name, NULL)
      )
    }
    
    run_time <- difftime(Sys.time(), start_time, units = "hours")
    results_list[["run_time"]] <- run_time
    results_list[["brms_fit"]] <- fit
    results_list[["model_success"]] <- TRUE
    
    # Extract diagnostics for HTML reporting
    results_list[["diagnostics"]] <- extract_diagnostics(fit)
    
    cat("✓ Model completed successfully in", round(as.numeric(run_time), 2), "hours\n")
    
  }, error = function(e) {
    run_time <- difftime(Sys.time(), start_time, units = "hours")
    results_list[["run_time"]] <<- run_time
    
    cat("✗ Model fitting failed:", e$message, "\n")
    results_list[["error_message"]] <<- paste("Model fitting error:", e$message)
    results_list[["error_traceback"]] <<- capture.output(traceback())
    
    # Save partial results even on failure
    if(!is.null(model_save_name)) {
      error_file <- "{output_folder}/{model_save_name}_ERROR.RData" %>% glue::glue()
      save(list = c('results_list'), file = error_file)
      cat("  Error results saved to:", error_file, "\n")
    }
    
    return(results_list)
  })
  
  # Save results if model fitting succeeded
  if(results_list[["model_success"]] && !is.null(model_save_name)) {
    tryCatch({
      save(list = c('results_list'), 
           file = "{output_folder}/{model_save_name}.RData" %>% glue::glue())
      results_list[["model_name"]] <- model_save_name
    }, error = function(e) {
      cat("✗ Failed to save model results:", e$message, "\n")
      results_list[["save_error"]] <<- e$message
    })
  }
  
  return(results_list)
} 


#' binomial regression model that wraps brms functions.
#' 
#' This is an alternative specification of a logistic model
#' 
#' Note that this is not the primary modeling function. We use the logistic_model_wrapper throughout
#' 
#' @param data data.frame containing the training data
#' 
#' @param prior_config list containing configuration for the model parameter's priors
#' 
#' @param y_var character label for the y variable in the data.frame
#' 
#' @param trials character or numeric for the trials specification
#' 
#' @param id_var character label for the group identity variable - used to define the levels of random effects
#' 
#' @param lv1_vars character vector of labels for the predictors in the model at the "observation" level of the model. 
#' sometimes also referred to as the level-1 variables
#' 
#' @param lv1_ran_vars character vector of labels for the predictors free to vary across the id_var variable levels.
#' 
#' @param lv2_int_vars character vector of labels for predictors of the random intercept for each level of the id_var. 
#' These are variables measured at the level of the id_var.
#' 
#' @param lv2_mod_var character label (single variable) for a level-2 moderator that interacts with all lv1_vars.
#' This variable is measured at the level of the id_var and will create interaction terms with each lv1_var.
#' 
#' @param output_folder character filepath to location where outputs should be stored from the model
#' 
#' @param warmup numeric integer value for the number of warmup iterations. Passed to the brms model API.
#' 
#' @param iter numeric integer value for the number of total iterations. Passed to the brms model API.
#' 
#' @param chains numeric integer value for total number of MCMC chains to sample. Passed to the brms model API.
#' 
#' @param control_list list of additional arguments to configure the model fitting process. 
#' 
#' @param future_arg boolean, optional. If set to TRUE attempts to leverage additional parallelization of the 
#' MCMC computations. Default is set to FALSE.
#' 
#' @param model_save_name character filename to save the compiled Stan model produced when fitting a model with brms.

binomial_model_wrapper <- function(data, prior_config, y_var, trials, id_var, lv1_vars, lv1_ran_vars, lv2_int_vars,
                                   lv2_mod_var = NULL, output_folder, warmup, iter, chains, control_list, 
                                   future_arg = FALSE, model_save_name=NULL){
  
  start_time <- Sys.time()
  
  # Initialize results list
  results_list <- list()
  results_list[["model_data"]] <- data
  results_list[["model_success"]] <- FALSE
  results_list[["error_message"]] <- NULL
  
  model_id <- ifelse(!is.null(model_save_name), model_save_name, "unnamed_binomial_model")
  cat("Fitting model:", model_id, "\n")
  
  # Build model formula
  tryCatch({
    # Start with base fixed effects
    fixed_effects <- paste(c(1, lv1_vars), collapse=' + ')
    
    # Add level-2 intercept variables if present
    if(!is.null(lv2_int_vars)) {
      fixed_effects <- paste(fixed_effects, paste(lv2_int_vars, collapse=' + '), sep=' + ')
    }
    
    # Add moderator main effect and interactions if present
    if(!is.null(lv2_mod_var)) {
      # Add main effect of moderator
      fixed_effects <- paste(fixed_effects, lv2_mod_var, sep=' + ')
      
      # Add interactions between moderator and all level-1 variables
      interaction_terms <- paste(lv1_vars, lv2_mod_var, sep=':')
      fixed_effects <- paste(fixed_effects, paste(interaction_terms, collapse=' + '), sep=' + ')
    }
    
    # Build full formula with random effects
    model <- "
        {y_var} | trials({trials}) ~ {fixed_effects} +
        ({paste(c(1, lv1_ran_vars), collapse=' + ')} | {id_var})
        " %>% glue::glue() %>% 
      brms::bf() + binomial()
    
    results_list[["brms_formula"]] <- model
    results_list[["brms_priors"]] <- brms::get_prior(model, data)
    
  }, error = function(e) {
    cat("✗ Error constructing model formula:", e$message, "\n")
    results_list[["error_message"]] <<- paste("Formula construction error:", e$message)
    return(results_list)
  })
  
  # Fit the model with error handling
  tryCatch({
    fit <- brms::brm(
      model, 
      data = data, 
      iter = iter, 
      warmup = warmup,
      chains = chains, 
      cores = chains, 
      control = control_list,
      future = future_arg, 
      save_model = ifelse(!is.null(model_save_name), model_save_name, NULL)
    )
    
    run_time <- difftime(Sys.time(), start_time, units = "hours")
    results_list[["run_time"]] <- run_time
    results_list[["brms_fit"]] <- fit
    results_list[["model_success"]] <- TRUE
    
    # Extract diagnostics for HTML reporting
    results_list[["diagnostics"]] <- extract_diagnostics(fit)
    
    cat("✓ Model completed successfully in", round(as.numeric(run_time), 2), "hours\n")
    
  }, error = function(e) {
    run_time <- difftime(Sys.time(), start_time, units = "hours")
    results_list[["run_time"]] <<- run_time
    
    cat("✗ Model fitting failed:", e$message, "\n")
    results_list[["error_message"]] <<- paste("Model fitting error:", e$message)
    results_list[["error_traceback"]] <<- capture.output(traceback())
    
    # Save partial results even on failure
    if(!is.null(model_save_name)) {
      error_file <- "{output_folder}/{model_save_name}_ERROR.RData" %>% glue::glue()
      save(list = c('results_list'), file = error_file)
      cat("  Error results saved to:", error_file, "\n")
    }
    
    return(results_list)
  })
  
  # Save results if model fitting succeeded
  if(results_list[["model_success"]] && !is.null(model_save_name)) {
    tryCatch({
      save(list = c('results_list'), 
           file = "{output_folder}/{model_save_name}.RData" %>% glue::glue())
      results_list[["model_name"]] <- model_save_name
    }, error = function(e) {
      cat("✗ Failed to save model results:", e$message, "\n")
      results_list[["save_error"]] <<- e$message
    })
  }
  
  return(results_list)
} 


#' Extract model diagnostics for storage and reporting
#' 
#' @param fit brmsfit object
#' @return list of diagnostic information

extract_diagnostics <- function(fit) {
  diagnostics <- list()
  
  tryCatch({
    # Divergent transitions
    np <- brms::nuts_params(fit)
    diagnostics$n_divergent <- sum(np$Parameter == "divergent__" & np$Value == 1)
    
    # Rhat values
    rhat_vals <- brms::rhat(fit)
    diagnostics$max_rhat <- max(rhat_vals, na.rm = TRUE)
    diagnostics$n_high_rhat <- sum(rhat_vals > 1.01, na.rm = TRUE)
    
    # Effective sample size
    neff_vals <- brms::neff_ratio(fit)
    diagnostics$min_ess_ratio <- min(neff_vals, na.rm = TRUE)
    diagnostics$n_low_ess <- sum(neff_vals < 0.1, na.rm = TRUE)
    
    # Additional diagnostics
    diagnostics$max_treedepth <- max(np$Value[np$Parameter == "treedepth__"])
    diagnostics$n_max_treedepth <- sum(np$Value[np$Parameter == "treedepth__"] >= 10)
    
  }, error = function(e) {
    diagnostics$extraction_error <- e$message
  })
  
  return(diagnostics)
}


#' Converts values on a logit scale to probabilities
#' 
#' @param logits a numeric vector of values on a logit scale that require transformation to probabilities.

logits_to_prob <- function(logits){
  odds <- exp(logits)
  prob <- odds / (1 + odds)
  return(prob)
}