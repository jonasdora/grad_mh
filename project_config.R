
pkgs <- c('rstan', 'shinystan', 'brms', 'ggraph', 'GGally', 'tufte', 'nFactors', 'tidygraph', 'lavaan', 
          'Cairo')

for(pkg in pkgs){
    if(!require(pkg, character.only=TRUE)){
        install.packages(pkg, 
                         repos="https://cloud.r-project.org",
                         dependencies=TRUE
                         )
    }
}


if(!require(cmdstanr)){
    install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
    # Installation step will set the path for the cmdstan backend.
    cmdstanr::install_cmdstan()
}

# File containing basic configurations and settings for project
library(glue)
library(magrittr)

LOCAL_REPO <- 'C:/Users/Jonas Dora/OneDrive - UW/studies/2026_shackman/grad_mh'
DATA_DIR <- paste0(LOCAL_REPO, '/', 'data')
R_DIR <- paste0(LOCAL_REPO, '/', 'R')
MAPS_DIR <- paste0(LOCAL_REPO, '/', 'data_maps')
POSTERIOR_OUTPUTS <- paste0(DATA_DIR, "/", "OUTPUTS")
PLOT_OUTPUT <- paste0(LOCAL_REPO, '/outputs/figures')
SUMMARY_OUTPUT <- paste0(LOCAL_REPO, '/outputs/tables')

# Create a directory at the location above if one does not already exist
for(fldr in c(POSTERIOR_OUTPUTS, PLOT_OUTPUT, SUMMARY_OUTPUT)) {
    if(!dir.exists(fldr)) {
        dir.create(fldr, recursive = TRUE)
    }
}

NSDUH_MISSING_CODES <- c(85, 89, 94, 97, 98, 99)

base_palette <- c(line = '#426EBD', 
                  fill = '#F7FBFF')

analogous_palette <- c(blue = '#426EBD', 
                       purple = "#6942bd", 
                       green = "#42bd92")

PRIOR_CONFIG <- c(
    brms::set_prior(
        'normal(0, 2)',
        class = 'b'
    ), 
    brms::set_prior(
        'lognormal(-.5, .5)',
        class = 'sd'
    ), 
    brms::set_prior(
        'lkj(2)',
        class = 'cor'
    )
)

POIS_PRIOR_CONFIG <- c(
    brms::set_prior(
        'normal(0, 2)',
        class = 'b'
    ), 
    brms::set_prior(
        'lkj(2)',
        class = 'cor'
    )
)

pad_numbers <- function(x, width, justify = "right") {
    x <- format(x, width = width, justify = justify)
    gsub(" ", "\u2007", x)  # figure space (U+2007); was the literal string /u2007
}

DEFAULT_BREAKS <- seq(0, 40, by=10)
DEFAULT_LABELS <- paste0(pad_numbers(DEFAULT_BREAKS, width=6), '%')
DEFAULT_YLIM <- c(0, max(DEFAULT_BREAKS) * 1.1)

PERC_PLOT_CONFIG <- list(
    'Anxiety' = list(
        title='... Anxiety Diagnosis or Treatment',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ),
    'Any Psychiatric Disorder' = list(
        title='... Any Mental Illness (excluding ADHD and SUDs)',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ),
    'Bipolar Disorder' = list(
        title='... Bipolar Disorder',
        y_breaks=seq(0, 10, by=2.5), 
        y_labels=paste0(pad_numbers(seq(0, 10, by=2.5), width=6), "%"), 
        y_limits=c(0, max(seq(0, 10, by=2.5)) * 1.1)
    ),
    'Depression' = list(
        title='... Depression Diagnosis or Treatment',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ), 
    'Panic Attacks' = list(
        title='... Panic Attacks',
        y_breaks=seq(0, 45, by=15), 
        y_labels=paste0(pad_numbers(seq(0, 45, by=15), width=6), "%"), 
        y_limits=c(0, max(seq(0, 45, by=15)) * 1.1)
    ), 
    'Schizophrenia' = list(
        title='... Schizophrenia',
        y_breaks=seq(0, 10, by=2.5), 
        y_labels=paste0(pad_numbers(seq(0, 10, by=2.5), width=6), "%"), 
        y_limits=c(0, max(seq(0, 10, by=2.5)) * 1.1)
    ), 
    'Suicidal Thoughts' = list(
        title='... seriously considered suicide',
        y_breaks=seq(0, 15, by=3), 
        y_labels=paste0(pad_numbers(seq(0, 15, by=3), width=6), "%"), 
        y_limits=c(0, max(seq(0, 15, by=3)) * 1.1)
    ), 
    'Suicide Attempt' = list(
        title='... attempted suicide',
        y_breaks=seq(0, 4, by=1), 
        y_labels=paste0(pad_numbers(seq(0, 4, by=1), width=6), "%"), 
        y_limits=c(0, max(seq(0, 4, by=1)) * 1.1)
    ), 
    'Emotional Distress' = list(
        title='... felt any emotional distress',
        y_breaks=seq(0, 55, by=10), 
        y_labels=paste0(pad_numbers(seq(0, 55, by=10), width=6), '%'), 
        y_limits=c(0, 55)
    ),
    'Overwhelmed or Exhausted' = list(
        title='... felt overwhelmed or exhausted',
        y_breaks=seq(0, 75, by=15), 
        y_labels=paste0(pad_numbers(seq(0, 75, by=15), width=6), "%"), 
        y_limits=c(0, 82.5)
    ),
    'Overwhelmed' = list(
        title='... felt overwhelmed by all you had to do',
        y_breaks=seq(0, 75, by=15), 
        y_labels=paste0(pad_numbers(seq(0, 75, by=15), width=6), "%"), 
        y_limits=c(0, 82.5)
    ),
    'Exhausted' = list(
        title='... felt exhausted',
        y_breaks=seq(0, 75, by=15), 
        y_labels=paste0(pad_numbers(seq(0, 75, by=15), width=6), "%"), 
        y_limits=c(0, 82.5)
    ), 
    'Poor Health' = list(
        title='... fair/poor health',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ), 
    'Too Depressed to Function' = list(
        title='... felt so depressed that it was difficult to function',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ), 
    'Overwhelming Anxiety' = list(
        title='... felt overwhelming anxiety',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ), 
    'Lonely' = list(
        title='... felt very lonely',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ), 
    'Hopeless' = list(
        title='... felt things were hopeless',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ),
    'Sad' = list(
        title='... felt very sad',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    ),
    'Anger' = list(
        title='... felt overwhelming anger',
        y_breaks=DEFAULT_BREAKS, 
        y_labels=DEFAULT_LABELS, 
        y_limits=DEFAULT_YLIM
    )
)

NSDUH_PLOT_CONFIG <- list(
    'Any Psychiatric Disorder' = list(
        y_breaks=seq(10, 40, by=10), 
        y_labels=paste0(pad_numbers(seq(10, 40, by=10), width=6), "%"), 
        y_limits=c(10, 41)
    ),
    'Suicidal Thoughts' = list(
        y_breaks=seq(0, 10, by=2.5), 
        y_labels=paste0(pad_numbers(seq(0, 10, by=2.5), width=6), "%"), 
        y_limits=c(0, 11)
    ), 
    'Suicide Attempt' = list(
        y_breaks=seq(0, 1, by=.25), 
        y_labels=paste0(pad_numbers(seq(0, 1, by=.25), width=6), "%"), 
        y_limits=c(0, 1.1)
    ), 
    'Poor Health' = list(
        y_breaks=seq(0, 15, by=5), 
        y_labels=paste0(pad_numbers(seq(0, 15, by=5), width=6), "%"), 
        y_limits=c(0,16)
    ), 
    'Matched Any Psychiatric Disorder' = list(
        y_breaks=seq(10, 40, by=10), 
        y_labels=paste0(pad_numbers(seq(10, 40, by=10), width=6), "%"), 
        y_limits=c(10, 41)
    ),
    'Matched Suicidal Thoughts' = list(
        y_breaks=seq(0, 10, by=2.5), 
        y_labels=paste0(pad_numbers(seq(0, 10, by=2.5), width=6), "%"), 
        y_limits=c(0, 11)
    ), 
    'Matched Suicide Attempt' = list(
        y_breaks=seq(0, 1, by=.25), 
        y_labels=paste0(pad_numbers(seq(0, 1, by=.25), width=6), "%"), 
        y_limits=c(0, 1.1)
    ), 
    'Matched Poor Health' = list(
        y_breaks=seq(0, 15, by=5), 
        y_labels=paste0(pad_numbers(seq(0, 15, by=5), width=6), "%"), 
        y_limits=c(0, 16)
    )
)
