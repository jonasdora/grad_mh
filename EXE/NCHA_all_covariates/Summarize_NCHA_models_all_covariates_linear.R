library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)
library(GGally)


BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh/project_config.R'

if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)

source(file.path(R_DIR, "modeling_mod.R"))
source(file.path(R_DIR, "plotting.R"))
source(file.path(R_DIR, "utils.R"))      

VERSION <- Sys.Date()

yvars <- list('Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='dx_suic_try_all_cov_linear'))


time_vars <- c('c_Time')
covariates <- c('Intercept'='Intercept', 'Time'='c_Time',
                'Age'='c_Q46_age',
                'Male v. Female'='Q47_genderMale', 'Transgender v. Female'='Q47_genderTransgender',
                'Black v. White'='race_ethnblack', 'Hispanic v. White'='race_ethnhispanic',
                'Asian v. White'='race_ethnasian', 'Native v. White'='race_ethnnative',
                'Multiracial v. White'='race_ethnmulti', 'Other v. White'='race_ethnother',
                'Part-Time'='Q52_enrollmentPartMtime',
                'International'='Q55_internationalYes',
                'Web Survey'='survey_methodWeb',
                'Size: 2,500-4,999 v. <2,500'='school_size2500M4999students',
                'Size: 5,000-9,999 v. <2,500'='school_size5000M9999students',
                'Size: 10,000-19,999 v. <2,500'='school_size10000M19999students',
                'Size: >20,000 v. <2,500'='school_size20000studentsormore',
                'Public School'='public_schl'
)

xvar <- 'c_Time'

plotlist <- list()

for(yvar in names(yvars)){
    var_name <- yvars[[yvar]]['var_name']
    names(var_name) <- NULL
    data_prefix <- yvars[[yvar]]['data_prefix']
    names(data_prefix) <- NULL

    data_filepath <- "{POSTERIOR_OUTPUTS}/{data_prefix}.rds" %>% glue()

    if(!file.exists(data_filepath)) {
        stop("Missing model output: {data_filepath}\n  ",
             "(Every yvar must have a fitted .rds. If this outcome was ",
             "intentionally dropped, remove it from `yvars`.)" %>% glue())
    }

    results_list <- readRDS(data_filepath)
    testthat::expect_true(inherits(results_list[["brms_fit"]], "brmsfit"))

    data <- results_list[["brms_fit"]]$data
    model <- results_list[["brms_fit"]]

    fixef_tbl <- fixef(model)
    for(row in 1:nrow(fixef_tbl)) {
        rownames(fixef_tbl)[row] <- names(covariates)[which(covariates == rownames(fixef_tbl)[row])]
    }
    fixef_tbl %>%
        write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_coefficients_table_all_cov_linear_{VERSION}.csv" %>% glue(), row.names = TRUE)

    grp_df <- data %>%
        group_by(school_id, !!sym(xvar)) %>%
        summarize(
            count = n(),
            perc = mean(!!sym(var_name), na.rm = TRUE) * 100
        ) %>%
        filter(count >= 30)

    par_names <- paste0('b_', covariates)
    post_df <- as_draws_df(model, variable = par_names) %>% as.data.frame()
    post_df <- post_df[, par_names]
    names(post_df)[1:2] <- c('b0', 'b1')

    diagnostic_plot <- ggpairs(post_df[,c('b0', 'b1')],
                               upper = list(continuous = wrap('cor', size = 4, color='black',
                                                              stars=FALSE)),
                               diag = list(continuous = wrap("densityDiag",
                                                             fill='#426EBD')),
                               lower = list(continuous = wrap('points', color='#426EBD')),
                               display_grid=FALSE) +
        labs(title='{yvar} Posterior Parameter Distributions' %>% glue(),
             subtitle='All covariates',
             caption = paste('b0 = Model Intercept',
                             'b1 = Slope for linear effect of time',
                             sep='\n'))

    ggsave(
        diagnostic_plot,
        filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_parameter_diagnostic_pairsplot_all_cov_linear.png" %>% glue(),
        device = "png",
        units = "in",
        height = 9,
        width = 8,
        dpi = 600
    )

    set.seed(20260617)
    n_rep    <- min(4000L, nrow(data))
    rep_rows <- data[sample(nrow(data), n_rep), , drop = FALSE]
    rep_rows$c_Time <- 0

    eta_cov <- posterior_linpred(model, newdata = rep_rows, re_formula = NA)  
    b_time  <- as_draws_df(model, variable = "b_c_Time")$b_c_Time            

    time_grid <- seq(0, 10.5, by = .125)
    plot_df <- purrr::map_dfr(time_grid, function(tt) {
        p <- plogis(eta_cov + b_time * tt)    
        data.frame(.draw  = seq_len(nrow(p)),
                   c_Time = tt,
                   perc   = rowMeans(p) * 100)
    })
    # ═════════════════════════════════════════════════════════════════════════

    plotlist[[yvar]] <- create_percent_summary_plot(
        plot_df,
        grp_df,
        title = yvar,
        x_breaks = seq(0.5, 10.5, by = 2),
        x_labels = seq(2009, 2019, by = 2),
        y_breaks = PERC_PLOT_CONFIG[[yvar]][['y_breaks']],
        y_labels = PERC_PLOT_CONFIG[[yvar]][['y_labels']],
        y_limits = PERC_PLOT_CONFIG[[yvar]][['y_limits']],
        color_pal = "Blues",
    )

    max_y <- max(PERC_PLOT_CONFIG[[yvar]][['y_breaks']])

    ggsave(
        plotlist[[yvar]],
        filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_summary_plot_all_cov_linear_{max_y}.png" %>% glue(),
        device = "png",
        units = "in",
        height = 5,
        width = 9,
        dpi = 600
    )

    create_bin_summary_table(data, plot_df, yvar = var_name) %>%
        write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_summary_stats_all_cov_linear_{VERSION}.csv" %>% glue(), row.names = FALSE)


    remove(list = c('data', 'model', 'plot_df', 'post_df', 'results_list'))
    gc()
}
