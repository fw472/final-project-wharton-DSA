# 31_optionB_lasso.R
#
# LASSO with k-fold CV for Option B, both crises, repeated with 5 different
# random seeds to check whether variable selection is STABLE or wobbles --
# a small sample (~735-800 counties, 18 predictors+controls) is exactly the
# regime where CV fold assignment can flip which variables survive.
#
# Response: recovery_ratio_2008 / recovery_ratio_covid (raw ratio, same
# scale used throughout this project's viability checks). Predictors:
# Option B's studied predictors (self-employment, migration, unemployment,
# age structure, education, 6 industry sectors) + the same controls as
# Option A (poverty, income, population, density, rucc, pop_trend).
# wealth_band is NOT included here -- it's a stratification variable for
# the separate within-band test (33_optionB_wealthband.R), not a pooled-
# model predictor (it's partly derived from median_hh_income, already a
# control).

library(dplyr)
library(glmnet)

source("pipeline/29_predictor_set_optionB.R")

sample_2008  <- readRDS("raw_data/30_optionB_sample_2008.rds")
sample_covid <- readRDS("raw_data/30_optionB_sample_covid.rds")

build_xy <- function(data, predictors, controls, outcome) {
  cols <- c(predictors, controls)
  X <- as.matrix(data[cols])
  storage.mode(X) <- "double"
  y <- data[[outcome]]
  list(X = X, y = y, varnames = cols)
}

xy_2008  <- build_xy(sample_2008, STUDIED_PREDICTORS_2007_OPTIONB, CONTROLS_COUNTY_2008, "recovery_ratio_2008")
xy_covid <- build_xy(sample_covid, STUDIED_PREDICTORS_2019_OPTIONB, CONTROLS_COUNTY_COVID, "recovery_ratio_covid")

seeds <- c(101, 202, 303, 404, 505)

run_lasso_seeds <- function(xy, label) {
  message("\n============================================================")
  message("LASSO -- ", label, " (n = ", nrow(xy$X), ")")
  message("============================================================")
  selected <- matrix(FALSE, nrow = length(seeds), ncol = length(xy$varnames),
                      dimnames = list(paste0("seed_", seeds), xy$varnames))
  coefs_1se <- matrix(NA_real_, nrow = length(seeds), ncol = length(xy$varnames),
                       dimnames = list(paste0("seed_", seeds), xy$varnames))
  selected_min <- matrix(FALSE, nrow = length(seeds), ncol = length(xy$varnames),
                          dimnames = list(paste0("seed_", seeds), xy$varnames))
  coefs_min <- matrix(NA_real_, nrow = length(seeds), ncol = length(xy$varnames),
                       dimnames = list(paste0("seed_", seeds), xy$varnames))
  lambda_1se_vals <- numeric(length(seeds))

  for (i in seq_along(seeds)) {
    set.seed(seeds[i])
    cvfit <- cv.glmnet(xy$X, xy$y, alpha = 1, nfolds = 10, standardize = TRUE)
    lambda_1se_vals[i] <- cvfit$lambda.1se
    cf <- as.matrix(coef(cvfit, s = "lambda.1se"))[-1, 1]  # drop intercept
    selected[i, ] <- cf != 0
    coefs_1se[i, ] <- cf
    cfm <- as.matrix(coef(cvfit, s = "lambda.min"))[-1, 1]
    selected_min[i, ] <- cfm != 0
    coefs_min[i, ] <- cfm
  }

  selection_freq <- colSums(selected)
  message("\nSelection frequency out of 5 seeds (lambda.1se):")
  freq_df <- data.frame(variable = xy$varnames, times_selected = selection_freq,
                         mean_coef_when_selected = sapply(xy$varnames, function(v) {
                           vals <- coefs_1se[selected[, v], v]
                           if (length(vals) == 0) NA else mean(vals)
                         }))
  freq_df <- freq_df[order(-freq_df$times_selected), ]
  print(freq_df, row.names = FALSE)

  n_stable <- sum(selection_freq == 5 | selection_freq == 0)
  n_wobbly <- sum(selection_freq > 0 & selection_freq < 5)
  message(sprintf("\n%d / %d variables STABLE (selected in all 5 or none); %d WOBBLE (selected in some but not all seeds).",
                   n_stable, length(xy$varnames), n_wobbly))
  if (n_wobbly > 0) {
    message("Wobbly variables: ", paste(freq_df$variable[freq_df$times_selected > 0 & freq_df$times_selected < 5], collapse = ", "))
  }

  selection_freq_min <- colSums(selected_min)
  freq_min_df <- data.frame(variable = xy$varnames, times_selected_min = selection_freq_min,
                             mean_coef_when_selected_min = sapply(xy$varnames, function(v) {
                               vals <- coefs_min[selected_min[, v], v]
                               if (length(vals) == 0) NA else mean(vals)
                             }))
  freq_min_df <- freq_min_df[order(-freq_min_df$times_selected_min), ]
  message("\nSame thing at lambda.min (less conservative -- shows near-zero-but-nonzero effects lambda.1se shrinks away):")
  print(freq_min_df, row.names = FALSE)

  list(freq_df = freq_df, selected = selected, coefs_1se = coefs_1se, lambda_1se = lambda_1se_vals,
       freq_min_df = freq_min_df, selected_min = selected_min, coefs_min = coefs_min)
}

lasso_2008  <- run_lasso_seeds(xy_2008, "2008 crisis")
lasso_covid <- run_lasso_seeds(xy_covid, "COVID crisis")

saveRDS(list(lasso_2008 = lasso_2008, lasso_covid = lasso_covid), "raw_data/31_optionB_lasso_results.rds")
message("\nSaved raw_data/31_optionB_lasso_results.rds")
