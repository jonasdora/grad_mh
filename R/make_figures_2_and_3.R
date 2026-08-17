library(brms)
library(modelr)
library(tidybayes)
library(tidyverse)
library(glue)
library(patchwork)   # multi-panel composition
library(Cairo)

library(systemfonts)   # discovers installed system fonts
suppressWarnings({
  installed <- systemfonts::system_fonts()$family
})
pick_font <- function(candidates, installed) {
  for (f in candidates) if (any(tolower(installed) == tolower(f))) return(f)
  NULL
}
PLOT_FONT <- pick_font(
  c("Myriad Pro", "Source Sans 3", "Source Sans Pro", "SourceSans3"),
  installed
)
if (is.null(PLOT_FONT)) {
  PLOT_FONT <- ""   # "" -> ggplot uses the default family
} else {
  message("Using font: ", PLOT_FONT)
}

if (nzchar(PLOT_FONT)) {
  library(showtext)
  # Register from system fonts so the family name resolves in showtext.
  fp <- systemfonts::match_font(PLOT_FONT)$path
  if (!is.null(fp) && file.exists(fp)) {
    sysfonts::font_add(family = PLOT_FONT, regular = fp,
                       bold = tryCatch(systemfonts::match_font(PLOT_FONT, bold = TRUE)$path,
                                       error = function(e) fp))
  }
  showtext::showtext_auto()
  # showtext_opts(dpi = 600) is set per-ggsave below so raster sizes are correct.
}

font_theme <- if (nzchar(PLOT_FONT)) {
  theme(text = element_text(family = PLOT_FONT), plot.margin = margin(4, 4, 4, 4))
} else {
  theme(plot.margin = margin(4, 4, 4, 4))
}

BASE_FILE <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh/project_config.R'
if(!file.exists(BASE_FILE)) stop(glue('Missing project config: {BASE_FILE}'))
source(BASE_FILE)
source(file.path(R_DIR, "modeling_mod.R"))   # logits_to_prob
source(file.path(R_DIR, "plotting.R"))       # create_percent_summary_plot
source(file.path(R_DIR, "utils.R"))

xvar <- 'c_Time'
VERSION <- format(Sys.Date(), "%Y%m%d")

# FIGURE 2 

fig2_panels <- list(
    list(label='Overwhelming Anxiety', var='Q30G_anxiety_r_2wks',    prefix='anxious_no_cov',            quad=TRUE),
    list(label='Felt Depressed',       var='Q30F_depressed_r_2wks',  prefix='depressed_no_cov',          quad=TRUE),
    list(label='Anxiety Dx',           var='Q31A_anxiety_dich',      prefix='dx_anxiety_no_cov',         quad=TRUE),
    list(label='Depression Dx',        var='Q31A_depression_dich',   prefix='dx_depression_no_cov',      quad=TRUE),
    list(label='Any Dx',               var='any_dx_nsduh',           prefix='dx_nsduh_any_no_cov',       quad=TRUE),
    list(label='Considered Suicide',   var='Q30J_suic_thnk_r_12mos', prefix='dx_suic_thnk_no_cov',       quad=TRUE),
    list(label='Attempted Suicide',    var='Q30K_suic_try_r_12mos',  prefix='dx_suic_try_no_cov_linear', quad=FALSE),
    list(label='Poor Health',          var='global_health_dich',     prefix='global_health_dich_no_cov', quad=TRUE)
)

# Build one ACHA-NCHA panel (marginal posterior trajectory + school bubbles).
build_ncha_panel <- function(p, show_legend = FALSE) {
    f <- file.path(POSTERIOR_OUTPUTS, paste0(p$prefix, ".rds"))
    if(!file.exists(f)) stop(glue("Missing model for Figure 2 panel '{p$label}': {f}"))
    model <- readRDS(f)[["brms_fit"]]
    data  <- model$data

    grp_df <- data %>%
        group_by(school_id, !!sym(xvar)) %>%
        summarize(count = n(), perc = mean(!!sym(p$var), na.rm = TRUE) * 100, .groups = "drop") %>%
        filter(count >= 30)

    vars <- if (p$quad) c('b_Intercept','b_c_Time','b_quad_c_Time') else c('b_Intercept','b_c_Time')
    pd <- as_draws_df(model, variable = vars) %>% as.data.frame()
    names(pd) <- if (p$quad) c('b0','b1','b2') else c('b0','b1')

    tt <- seq(0, 10.5, by = .125)
    plot_df <- purrr::map_dfr(seq_len(nrow(pd)), function(r) {
        eta <- pd$b0[r] + tt*pd$b1[r] + (if (p$quad) tt^2*pd$b2[r] else 0)
        data.frame(.draw = r, c_Time = tt, perc = logits_to_prob(eta) * 100)
    })

    # Use the study's plotting fn if the outcome has a PERC_PLOT_CONFIG entry;
    # fall back to sensible defaults otherwise.
    cfg <- PERC_PLOT_CONFIG[[p$label]]
    if (is.null(cfg)) {
        rng <- range(c(grp_df$perc, plot_df$perc), na.rm = TRUE)
        ybrk <- pretty(c(0, rng[2]))
        cfg <- list(y_breaks = ybrk,
                    y_labels = paste0(ybrk, '%'),
                    y_limits = c(0, max(ybrk)))
    }

    g <- create_percent_summary_plot(
        plot_df, grp_df,
        title    = p$label,
        x_breaks = seq(0.5, 10.5, by = 2),
        x_labels = seq(2009, 2019, by = 2),
        y_breaks = cfg[['y_breaks']],
        y_labels = cfg[['y_labels']],
        y_limits = cfg[['y_limits']],
        color_pal = "Blues"
    )
    if (!show_legend) g <- g + theme(legend.position = "none")
    g + labs(caption = NULL, x = NULL, y = NULL) + font_theme
}

panels2 <- lapply(fig2_panels, build_ncha_panel, show_legend = FALSE)


panelA_with_legend <- build_ncha_panel(fig2_panels[[1]], show_legend = TRUE)

design <- "
ABL
CDE
FGH
"
fig2 <- patchwork::wrap_plots(
    A = panelA_with_legend,
    B = panels2[[2]],
    C = panels2[[3]], D = panels2[[4]], E = panels2[[5]],
    F = panels2[[6]], G = panels2[[7]], H = panels2[[8]],
    L = patchwork::guide_area(),
    design = design
) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
        theme = theme(plot.title = element_text(size = 16),
                      legend.justification = "center")
    )

fig2_final <- fig2

if (nzchar(PLOT_FONT)) showtext::showtext_opts(dpi = 600)
ggsave(file.path(PLOT_OUTPUT, glue("Figure2_ACHA-NCHA_multipanel_{VERSION}.png")),
       fig2_final, device = "png", units = "in", height = 12, width = 14, dpi = 600)
ggsave(file.path(PLOT_OUTPUT, glue("Figure2_ACHA-NCHA_multipanel_{VERSION}.eps")),
       fig2_final, device = cairo_ps, units = "in", height = 12, width = 14, dpi = 600)

# FIGURE 3 


load(glue('{DATA_DIR}/nsduh_matched_study_data_2023-01-15.RData'))  

color_palette <- c('#324961', '#4E79A7', '#AD0037')
names(color_palette) <- c('U.S. Adults', 'Graduate Students (ACHA-NCHA)', 'Matched Adults')
fill_palette <- c("#C2CCDD", "#e2979a")
names(fill_palette) <- c('Graduate Students (ACHA-NCHA)', 'Matched Adults')

fig3_panels <- list(
    list(label='Any Mental Illness',  nsduh_var='anymi_12mos',        ncha_prefix='dx_nsduh_any_no_cov',       quad=TRUE,  flip=FALSE),
    list(label='Suicidal Ideation',   nsduh_var='suic_thnk_12mos',    ncha_prefix='dx_suic_thnk_no_cov',       quad=TRUE,  flip=FALSE),
    list(label='Suicide Attempt',     nsduh_var='suic_try_12mos',     ncha_prefix='dx_suic_try_no_cov_linear', quad=FALSE, flip=FALSE),
    # Poor Health: NSDUH outcome is reverse-coded vs ACHA -> flip the NSDUH
    # traces to P(poor). Mirrors the Rmd's needs_outcome_flip(). See nsduh_traj().
    list(label='Poor Health',         nsduh_var='global_health_dich', ncha_prefix='global_health_dich_no_cov', quad=TRUE,  flip=TRUE)
)

# helper: NSDUH fitted trajectory (mean + 95% HDI) from an .RData results_list
nsduh_traj <- function(prefix, var, quad, flip = FALSE) {
    suffix <- if (quad) "" else "_linear"
    f <- file.path(POSTERIOR_OUTPUTS, glue("{prefix}_{var}{suffix}.RData"))
    if(!file.exists(f)) stop(glue("Missing NSDUH model: {f}"))
    e <- new.env(); load(f, envir = e); model <- e$results_list[["brms_fit"]]
    vars <- if (quad) c('b_Intercept','b_c_Time','b_quad_c_Time') else c('b_Intercept','b_c_Time')
    pd <- as_draws_df(model, variable = vars) %>% as.data.frame(); names(pd) <- substr(vars,3,nchar(vars))
    tt <- seq(0, 10.5, by = .125)
    purrr::map_dfr(tt, function(x) {
        eta <- pd[['Intercept']] + pd[['c_Time']]*x + (if (quad) pd[['quad_c_Time']]*x^2 else 0)
        p <- plogis(eta) * 100
        # The NSDUH global_health_dich outcome is coded in the OPPOSITE direction
        # from the ACHA-NCHA version: the NSDUH models predict P(GOOD health)
        # while ACHA predicts P(POOR health). 
        if (flip) p <- 100 - p
        data.frame(c_Time = x, perc = mean(p), lb = quantile(p,.025), ub = quantile(p,.975))
    })
}

# helper: ACHA-NCHA trajectory (mean + 95% HDI) from an .rds
ncha_traj <- function(prefix, quad) {
    f <- file.path(POSTERIOR_OUTPUTS, paste0(prefix, ".rds"))
    if(!file.exists(f)) stop(glue("Missing ACHA model: {f}"))
    model <- readRDS(f)[["brms_fit"]]
    vars <- if (quad) c('b_Intercept','b_c_Time','b_quad_c_Time') else c('b_Intercept','b_c_Time')
    pd <- as_draws_df(model, variable = vars) %>% as.data.frame(); names(pd) <- substr(vars,3,nchar(vars))
    tt <- seq(0, 10.5, by = .125)
    purrr::map_dfr(tt, function(x) {
        eta <- pd[['Intercept']] + pd[['c_Time']]*x + (if (quad) pd[['quad_c_Time']]*x^2 else 0)
        p <- plogis(eta) * 100
        data.frame(c_Time = x, perc = mean(p), lb = quantile(p,.025), ub = quantile(p,.975))
    })
}

build_fig3_panel <- function(p, show_legend = FALSE) {
    matched <- nsduh_traj('nsduh_matched', p$nsduh_var, p$quad, flip = isTRUE(p$flip))
    alladlt <- nsduh_traj('nsduh',         p$nsduh_var, p$quad, flip = isTRUE(p$flip))
    acha    <- ncha_traj(p$ncha_prefix, p$quad)   # ACHA is the reference direction — never flipped

    g <- ggplot() +
        # ACHA-NCHA (blue)
        geom_ribbon(data = acha, aes(x=c_Time, ymin=lb, ymax=ub, fill='Graduate Students (ACHA-NCHA)'), alpha=.5) +
        geom_line(data = acha, aes(x=c_Time, y=perc, color='Graduate Students (ACHA-NCHA)'), lwd=1.1) +
        # Matched adults (red)
        geom_ribbon(data = matched, aes(x=c_Time, ymin=lb, ymax=ub, fill='Matched Adults'), alpha=.5) +
        geom_line(data = matched, aes(x=c_Time, y=perc, color='Matched Adults'), lwd=1.1) +
        # All US adults (dark line, no HDI — population-level)
        geom_line(data = alladlt, aes(x=c_Time, y=perc, color='U.S. Adults'), lwd=1.1) +
        scale_color_manual("", values = color_palette) +
        scale_fill_manual("95% HDI", values = fill_palette) +
        scale_x_continuous(breaks = seq(0.5,10.5,by=2), labels = seq(2009,2019,by=2)) +
        labs(title = p$label, x = NULL, y = NULL) +
        theme_bw()
    if (!show_legend) g <- g + theme(legend.position = "none")
    g + font_theme
}

panels3 <- lapply(fig3_panels, build_fig3_panel, show_legend = FALSE)

panels3[[1]] <- build_fig3_panel(fig3_panels[[1]], show_legend = TRUE) +
    ggplot2::theme(
        legend.direction = "horizontal",
        legend.box       = "horizontal"
    )

design3 <- "
AB
CD
LL
"
fig3_final <- patchwork::wrap_plots(
    A = panels3[[1]], B = panels3[[2]],
    C = panels3[[3]], D = panels3[[4]],
    L = patchwork::guide_area(),
    design = design3,
    heights = c(1, 1, 0.12)
) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
        theme = theme(legend.position = "bottom",
                      legend.justification = "center")
    )


ggsave(file.path(PLOT_OUTPUT, glue("Figure3_NSDUH_combined_multipanel_{VERSION}.png")),
       fig3_final, device = "png", units = "in", height = 9, width = 11, dpi = 600)
ggsave(file.path(PLOT_OUTPUT, glue("Figure3_NSDUH_combined_multipanel_{VERSION}.eps")),
       fig3_final, device = cairo_ps, units = "in", height = 9, width = 11, dpi = 600)


