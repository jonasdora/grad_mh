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

yvars <- list('Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='dx_suic_try_no_cov_linear'),
              'Exhausted' = c(var_name='Q30C_exhausted_r_2wks', data_prefix='exhausted_item_no_cov_linear'),
              'Overwhelmed' = c(var_name='Q30B_overwhelmed_r_2wks', data_prefix='ovrwhlm_item_no_cov_linear'))

time_vars <- c('c_Time')
covariates <- c('Intercept'='Intercept', 'Time'='c_Time')

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
        write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_no_cov_linear_coefficients_table_{VERSION}.csv" %>% glue(), row.names = TRUE)

    grp_df <- data %>%
        group_by(school_id, !!sym(xvar)) %>%
        summarize(
            count = n(),
            perc = mean(!!sym(var_name), na.rm = TRUE) * 100
        ) %>%
        filter(count >= 30)

    post_df <- as_draws_df(model, variable = c('b_Intercept', 'b_c_Time')) %>%
        as.data.frame()
    post_df <- post_df[, c('b_Intercept', 'b_c_Time')]
    names(post_df) <- c('b0', 'b1')

    diagnostic_plot <- ggpairs(post_df,
            upper = list(continuous = wrap('cor', size = 4, color='black',
                                           stars=FALSE)),
            diag = list(continuous = wrap("densityDiag",
                                          fill='#426EBD')),
            lower = list(continuous = wrap('points', color='#426EBD')),
            display_grid=FALSE) +
        labs(title='{yvar} Posterior Parameter Distributions' %>% glue(),
             subtitle='No covariates',
             caption = paste('b0 = Model Intercept',
                             'b1 = Slope for linear effect of time',
                             sep='\n'))

    ggsave(
        diagnostic_plot,
        filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_linear_parameter_diagnostic_pairsplot.png" %>% glue(),
        device = "png",
        units = "in",
        height = 9,
        width = 8,
        dpi = 600
    )

    design_matrix <- data.frame(
        intrcpt = 1,
        time = seq(0, 10.5, by = .125)
    )

    plot_df <- data.frame()
    for(r in 1:nrow(post_df)) {
        pred_y <- design_matrix[['intrcpt']] * post_df[['b0']][r] +
            design_matrix[['time']] * post_df[['b1']][r]

        tmp_df <- data.frame(
            '.draw' = r,
            c_Time = design_matrix[['time']],
            perc = logits_to_prob(pred_y) * 100
        )

        plot_df <- rbind(plot_df, tmp_df)
    }

    plotlist[[yvar]] <- create_percent_summary_plot(
        plot_df,
        grp_df,
        title = PERC_PLOT_CONFIG[[yvar]][['title']],
        x_breaks = seq(0.5, 10.5, by = 2),
        x_labels = seq(2009, 2019, by = 2),
        y_breaks = PERC_PLOT_CONFIG[[yvar]][['y_breaks']],
        y_labels = PERC_PLOT_CONFIG[[yvar]][['y_labels']],
        y_limits = PERC_PLOT_CONFIG[[yvar]][['y_limits']],
        color_pal = "Blues",
        caption = "For convenience, plot excludes schools with fewer than 30 respondents."
    )

    max_y <- max(PERC_PLOT_CONFIG[[yvar]][['y_breaks']])

    ggsave(
        plotlist[[yvar]],
        filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_no_cov_linear_summary_plot_{max_y}.png" %>% glue(),
        device = "png",
        units = "in",
        height = 5,
        width = 9,
        dpi = 600
    )

    ggsave(
        plotlist[[yvar]],
        filename = "{PLOT_OUTPUT}/ACHA-NCHA_{yvar}_no_cov_linear_summary_plot_{max_y}.eps" %>% glue(),
        device = "eps",
        units = "in",
        height = 5,
        width = 9,
        dpi = 600
    )

    create_bin_summary_table(data, plot_df, yvar = var_name) %>%
        write.csv("{SUMMARY_OUTPUT}/ACHA-NCHA_{yvar}_no_cov_linear_summary_stats_{VERSION}.csv" %>% glue(), row.names = FALSE)


    remove(list = c('data', 'model', 'plot_df', 'post_df', 'results_list'))
    gc()
}
