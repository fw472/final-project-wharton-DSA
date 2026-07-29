# 33_optionB_wealthband.R
#
# THE key stress test: does Option B still work split by wealth_band? The
# user's expectation going in was ~250 counties/band -- but Option B's own
# complete-case sample is skewed by education's ACS 65k-population
# threshold (favors larger, higher-income counties), so the REAL band sizes
# in the Option B sample turned out far from that assumption (see
# 30_optionB_sample.R output: lower=57/99, middle=214/238, higher=468/463
# for 2008/COVID). This script runs both OLS (interpretable, shows
# convergence/degeneracy directly) and LASSO (single seed each -- 5-seed
# stability testing isn't meaningful at n=57) within each band, for both
# crises, and reports whether they hold up.

library(dplyr)
library(glmnet)

source("pipeline/29_predictor_set_optionB.R")

sample_2008  <- readRDS("raw_data/30_optionB_sample_2008.rds")
sample_covid <- readRDS("raw_data/30_optionB_sample_covid.rds")

run_band <- function(data, band_col, band_value, predictors, controls, outcome, label) {
  sub <- data %>% filter(.data[[band_col]] == band_value)
  n <- nrow(sub)
  p <- length(predictors) + length(controls)
  message(sprintf("\n--- %s / %s band: n = %d, p = %d predictors+controls (ratio %.1f:1) ---",
                   label, band_value, n, p, n / p))

  cols <- c(predictors, controls)
  form <- as.formula(paste(outcome, "~", paste(cols, collapse = " + ")))

  # OLS -- shows degeneracy directly (NA coefficients, huge SEs, R2 near 1 from overfitting)
  ols <- tryCatch(lm(form, data = sub), error = function(e) e)
  if (inherits(ols, "error")) {
    message("  OLS FAILED: ", conditionMessage(ols))
  } else {
    s <- summary(ols)
    n_na_coef <- sum(is.na(coef(ols)))
    n_sig <- sum(s$coefficients[, 4] < 0.05, na.rm = TRUE) - 1  # exclude intercept if significant
    message(sprintf("  OLS: R^2 = %.3f, adj R^2 = %.3f, %d/%d coefficients NA (aliased/collinear), %d significant at p<0.05",
                     s$r.squared, s$adj.r.squared, n_na_coef, length(coef(ols)), max(n_sig, 0)))
    if (n_na_coef > 0) {
      message("    NA coefficients (perfectly collinear / rank-deficient at this n): ",
              paste(names(coef(ols))[is.na(coef(ols))], collapse = ", "))
    }
  }

  # LASSO, single seed, 5-fold (10-fold too many for n=57) -- does it even
  # pick a sensible lambda, or collapse to the null (intercept-only) model?
  X <- as.matrix(sub[cols]); storage.mode(X) <- "double"
  y <- sub[[outcome]]
  nfolds <- if (n < 100) 5 else 10
  lasso <- tryCatch({
    set.seed(42)
    cv.glmnet(X, y, alpha = 1, nfolds = nfolds, standardize = TRUE)
  }, error = function(e) e)
  if (inherits(lasso, "error")) {
    message("  LASSO FAILED: ", conditionMessage(lasso))
  } else {
    cf <- as.matrix(coef(lasso, s = "lambda.1se"))[-1, 1]
    n_selected <- sum(cf != 0)
    message(sprintf("  LASSO (%d-fold): %d/%d variables selected at lambda.1se%s",
                     nfolds, n_selected, length(cf),
                     if (n_selected == 0) " -- COLLAPSED TO INTERCEPT-ONLY (null model)" else ""))
    if (n_selected > 0) message("    Selected: ", paste(names(cf)[cf != 0], collapse = ", "))
  }

  list(n = n, ols = ols, lasso = lasso)
}

message("============================================================")
message("WEALTH-BAND STRESS TEST -- Option B")
message("============================================================")

bands <- c("lower", "middle", "higher")
results_2008 <- lapply(bands, function(b) run_band(sample_2008, "wealth_band_2008", b,
                                                     STUDIED_PREDICTORS_2007_OPTIONB, CONTROLS_COUNTY_2008,
                                                     "recovery_ratio_2008", "2008"))
results_covid <- lapply(bands, function(b) run_band(sample_covid, "wealth_band_covid", b,
                                                      STUDIED_PREDICTORS_2019_OPTIONB, CONTROLS_COUNTY_COVID,
                                                      "recovery_ratio_covid", "COVID"))
names(results_2008) <- bands
names(results_covid) <- bands

saveRDS(list(results_2008 = results_2008, results_covid = results_covid),
        "raw_data/33_optionB_wealthband_results.rds")
message("\nSaved raw_data/33_optionB_wealthband_results.rds")
