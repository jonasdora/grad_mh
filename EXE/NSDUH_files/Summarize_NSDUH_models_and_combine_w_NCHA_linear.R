library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)
library(Cairo)


BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh/project_config.R'
DATA_VERSION <- "2023-01-15"
VERSION <- format(Sys.Date(), "v%Y%m%d")

if(!file.exists(BASE_FILE)){
    stop('ERROR: Missing project config file. {BASE_FILE}' %>% glue())
}

source(BASE_FILE)
source(file.path(R_DIR, "modeling_mod.R"))
source(file.path(R_DIR, "plotting.R"))
source(file.path(R_DIR, "utils.R"))       

load('{DATA_DIR}/nsduh_matched_study_data_{DATA_VERSION}.RData' %>% glue())


acha_overlay_from_rds_linear <- function(data_prefix) {
    f <- "{POSTERIOR_OUTPUTS}/{data_prefix}.rds" %>% glue()
    if(!file.exists(f)) stop("Missing ACHA-NCHA model for overlay: {f}" %>% glue())
    m <- readRDS(f)[["brms_fit"]]
    pd <- as_draws_df(m, variable = c('b_Intercept','b_c_Time')) %>% as.data.frame()
    names(pd)[1:2] <- c('b0','b1')
    tt <- seq(0, 10.5, by = .125)
    out <- data.frame()
    for(r in 1:nrow(pd)) {
        perc <- logits_to_prob(pd$b0[r] + tt*pd$b1[r]) * 100
        out <- rbind(out, data.frame(.draw = r, c_Time = tt, perc = perc))
    }
    out %>% group_by(c_Time) %>%
        summarize(perc_lb = quantile(perc, .025),
                  perc_ub = quantile(perc, .975),
                  perc    = mean(perc), .groups = "drop")
}

yvars <- list(
    'Suicide Attempt' = c(var_name='suic_try_12mos', data_prefix='nsduh'),
    'Matched Suicide Attempt' = c(var_name='suic_try_12mos', data_prefix='nsduh_matched')
)

time_vars <- c('c_Time')
covariates <- c('Intercept'='Intercept', 'Time'='c_Time')

plotlist <- list()

color_palette <- c('#324961', analogous_palette[1], '#AD0037')
names(color_palette) <- c('Fitted All Adults', 'Fitted ACHA-NCHA', 'Fitted Matched Adults')

fill_palette <- c("#C2CCDD", "#e2979a")
names(fill_palette) <- c('Fitted ACHA-NCHA', 'Fitted Matched Adults')

for(yvar in names(yvars)){
    var_name <- yvars[[yvar]]['var_name']
    names(var_name) <- NULL
    data_prefix <- yvars[[yvar]]['data_prefix']
    names(data_prefix) <- NULL

    data_filepath <- "{POSTERIOR_OUTPUTS}/{data_prefix}_{var_name}_linear.RData" %>% glue()

    if(!file.exists(data_filepath)) {
        stop("Missing NSDUH model output: {data_filepath}" %>% glue())
    }

    load(data_filepath)
    testthat::expect_true(exists("results_list"))

    data <- results_list[["brms_fit"]]$data
    model <- results_list[["brms_fit"]]

    fixef_tbl <- fixef(model)
    for(row in 1:nrow(fixef_tbl)) {
        rownames(fixef_tbl)[row] <- names(covariates)[which(covariates == rownames(fixef_tbl)[row])]
    }

    if(grepl('Matched', yvar)) {
        filename_base <- 'NSDUH_matched_{yvar}_linear' %>% glue()
    } else {
        filename_base <- 'NSDUH_{yvar}_linear' %>% glue()
    }

    fixef_tbl %>%
        write.csv("{SUMMARY_OUTPUT}/{filename_base}_coefficients_table.csv" %>% glue(), row.names = TRUE)

    post_df <- as_draws_df(model, variable = c('b_Intercept', 'b_c_Time')) %>% as.data.frame()
    post_df <- post_df[, c('b_Intercept', 'b_c_Time')]
    names(post_df) <- c('b0', 'b1')

    design_matrix <- data %>%
        data_grid(c_Time=seq_range(c_Time, 22)) %>%
        mutate(
            intrcpt = 1
        )

    fitted_df <- data.frame()
    for(r in 1:nrow(post_df)) {
        pred_y <- design_matrix[['intrcpt']] * post_df[['b0']][r] +
            design_matrix[['c_Time']] * post_df[['b1']][r]

        pred_y <- logits_to_prob(pred_y)

        tmp_df <- data.frame(
            .draw = r,
            c_Time = design_matrix[['c_Time']],
            prob = pred_y
        )

        fitted_df<- rbind(fitted_df, tmp_df)
    }

    fitted_df <- fitted_df %>%
        mutate(perc = prob * 100)

    plot_df <- fitted_df %>%
        group_by(c_Time) %>%
        summarize(
            perc = mean(perc),
            perc_lb = quantile(prob * 100, .025),
            perc_ub = quantile(prob * 100, .975)
        )

    create_bin_summary_table(data, fitted_df, yvar = var_name, weights = TRUE) %>%
        write.csv("{SUMMARY_OUTPUT}/{filename_base}_summary_stats_{VERSION}.csv" %>% glue(), row.names = FALSE)

    plt <- plot_df %>%
        ggplot(aes(x = c_Time, y=perc))

    if(grepl('Matched', yvar)) {
        plt <- plt +
            geom_ribbon(aes(ymax=perc_ub, ymin=perc_lb,
                            fill='Fitted Matched Adults'), alpha=.5) +
            geom_line(lwd = 1.25, aes(color = 'Fitted Matched Adults')) +
            scale_fill_manual(values = fill_palette)

    } else {
        plt <- plt +
            geom_line(lwd = 1.25, aes(color = 'Fitted All Adults'))
    }

    plt <- plt +
        scale_x_continuous(breaks = seq(0.5, 10.5, by = 2),
                           labels = seq(2009, 2019, by = 2)) +
        scale_y_continuous(breaks = NSDUH_PLOT_CONFIG[[yvar]][['y_breaks']],
                           labels = NSDUH_PLOT_CONFIG[[yvar]][['y_labels']],
                           limits = NSDUH_PLOT_CONFIG[[yvar]][['y_limits']]) +
        scale_color_manual(values = color_palette) +
        labs(title = paste0('NSDUH Outcomes: ', yvar),
             x = 'Year',
             y = 'Percentage') +
        theme_bw()

    plotlist[[yvar]] <- plt

    ggsave(
        plotlist[[yvar]],
        filename = "{PLOT_OUTPUT}/{filename_base}_summary_plot_{VERSION}.png" %>% glue(),
        device = "png",
        units = "in",
        height = 5,
        width = 9,
        dpi = 600
    )


    remove(list=c('data', 'model', 'plt'))
    gc()
}

base_vars <- c('Suicide Attempt')
ncha_yvars <- list('Suicide Attempt' = c(var_name='Q30K_suic_try_r_12mos', data_prefix='dx_suic_try_no_cov_linear'))

for(var in base_vars) {
    if(var %in% names(ncha_yvars)){

        acha_plot_df <- acha_overlay_from_rds_linear(ncha_yvars[[var]]['data_prefix'] %>% unname())

        list_pos <- which(grepl(var, names(plotlist)))

        tmp_plot <- plotlist[[max(list_pos)]] +
            geom_line(data=plotlist[[min(list_pos)]]$data, aes(color='Fitted All Adults'), lwd=1.25) +
            geom_ribbon(data = acha_plot_df, aes(x = c_Time, ymin = perc_lb, ymax = perc_ub, fill='Fitted ACHA-NCHA'),
                        alpha=.5) +
            geom_line(data = acha_plot_df, aes(x = c_Time, y = perc, color='Fitted ACHA-NCHA'), lwd = 1.25) +
            scale_fill_manual(values=fill_palette) +
            labs(title = '{var}: Comparing ACHA-NCHA, U.S. Adults, and Matched Population of U.S. Adults' %>% glue(),
                 color = 'Comparison Group',
                 fill = "95% HDIs",
                 caption = "HDI = Highest Density Interval around the model-implied population mean",
                 x="",
                 y="") +
            theme(axis.text.x = element_text(size=16),
                  axis.text.y = element_text(size=16))

        ggsave(
            tmp_plot,
            filename = "{PLOT_OUTPUT}/NSDUH_{var}_linear_combined_summary_plot_{VERSION}.png" %>% glue(),
            device = "png",
            units = "in",
            height = 8,
            width = 11,
            dpi = 600
        )

        ggsave(
            tmp_plot,
            filename = "{PLOT_OUTPUT}/NSDUH_{var}_linear_combined_summary_plot_{VERSION}.eps" %>% glue(),
            device = cairo_ps,
            units = "in",
            height = 8,
            width = 11,
            dpi = 600
        )
    }
}
