# Graduate student mental health

This repository contains the code supporting DeYoung et al.'s manuscript comparing trends in U.S. graduate student mental health levels with those of the general U.S. adult population and a sample of education-matched adults.

Some of the data used in these analyses are publicly available. Specifically, the comparison samples were created from the ongoing National Survey on Drug Use and Health available from the U.S. government's [Substance Abuse & Mental Health Data Archive](https://www.datafiles.samhsa.gov/data-sources) (SAMHDA).

Some of the data are not publicly available and need to be requested from the data producers. Specifically our sample of graduate students was created from the [National College Health Assessment](https://www.acha.org/NCHA/About_ACHA_NCHA/Survey/NCHA/About/Survey.aspx?hkey=7e9f6752-2b47-4671-8ce7-ba7a529c9934) - conducted twice a year by the American College Health Association.

## Data access

As noted above, some of the data used are publicly available and some are not. For colleagues working on this project or extensions, please reach out to the research team directly.

## Environment

The project runs in **R, from RStudio**. Open the repository through its **`.Rproj` file** before running anything: all file paths are resolved relative to the project root with the [`here`] package (`here::here(...)`).

Run `project_config.R` once to install and load the R packages the codebase uses. The models are fit with [**brms**] on a Stan backend; you will need a working brms + Stan installation on your machine.

## Re-creating a model

1. **Have the raw data in place** under the repo's `data/` folder. Contact the primary research team about data access; we are not in a position to publish all data used.

2. **Run the data-processing pipeline.** A short sequence of scripts in `EXE/` loads the raw data from the original producers and writes date-versioned clean data files into `data/`. This includes `EXE/create_nsduh_weights.R`, which builds the NSDUH survey-weight columns used by the weighted models (`weights` for the full adult sample, `normalized_weights` for the matched sample).

3. **`R/data_prep.R` loads the clean data and derivs analysis variables. Every modeling script sources it. The clean-data location is set by four constants at the top of the file (`NCHA_DATA_SUBDIR`, `NCHA_DATA_VERSION`, `NSDUH_DATA_SUBDIR`, `NSDUH_DATA_VERSION`).

4. **Fit the models** by running one of the **runner scripts** in `EXE/` from RStudio:

   | Script | Models |
   | --- | --- |
   | `EXE/ncha_no_cov_models.R` | NCHA, no covariates (21) |
   | `EXE/ncha_all_cov_models.R` | NCHA, covariate-adjusted (10) |
   | `EXE/ncha_moderation_models.R` | NCHA moderation (36) |
   | `EXE/nsduh_models.R` | NSDUH main (9) |
   | `EXE/nsduh_moderation_models.R` | NSDUH moderation (16) |

   Each sources `R/data_prep.R`, asserts every model formula against the published strings in a pre-flight block (so a wrong spec fails immediately rather than after hours of sampling), then fits. `SKIP_EXISTING <- TRUE` means a run skips any model whose output already exists, so re-running only fills gaps. NCHA fits are saved as `.rds`, NSDUH fits as `.RData`, under `data/OUTPUTS/`. 

## Reports and figures

The per-model diagnostics and derived-effect tables are rendered by five parameterized reports in `rmd_split/` — `01_ncha_no_cov`, `02_ncha_all_cov`, `03_ncha_moderation`, `04_nsduh_main`, `05_nsduh_moderation`. They share one renderer, `rmd_split/_report_body.Rmd`; each parent file only defines which saved models it reports. Knit the parent you want from RStudio. Figures 2 and 3 come from `EXE/make_figures_2_and_3.R`, and the manuscript summary tables from the `EXE/Summarize_*.R` scripts.

## Sampler settings

All models are fit with **4 chains, iter = 10000, warmup = 5000**, `adapt_delta = 0.95`. Priors: NCHA `b ~ normal(0, 2)`, `sd ~ lognormal(-0.5, 0.5)`, `cor ~ lkj(2)`; NSDUH `b ~ normal(0, 3)` and `Intercept ~ normal(0, 3)` (survey-weighted, no random effects).

## Toy example

A `toy_example/` folder, created by the original analyst, lets users exercise the model code on stand-in data without access to the restricted datasets.
