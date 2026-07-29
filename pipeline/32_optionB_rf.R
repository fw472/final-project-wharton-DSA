# 32_optionB_rf.R
#
# Random Forest for Option B, both crises. Reports %IncMSE variable
# importance (permutation importance -- more robust than node-purity for a
# small-N, moderate-p setting like this) and flags whether the ranking looks
# plausible (controls dominating vs. studied predictors contributing
# something real).

library(dplyr)
library(randomForest)

source("pipeline/29_predictor_set_optionB.R")

sample_2008  <- readRDS("raw_data/30_optionB_sample_2008.rds")
sample_covid <- readRDS("raw_data/30_optionB_sample_covid.rds")

run_rf <- function(data, predictors, controls, outcome, label, seed = 42) {
  cols <- c(predictors, controls)
  df <- data[c(outcome, cols)]
  names(df)[1] <- "y"
  set.seed(seed)
  rf <- randomForest(y ~ ., data = df, ntree = 2000, importance = TRUE, na.action = na.omit)

  message("\n============================================================")
  message("RANDOM FOREST -- ", label, " (n = ", nrow(df), ")")
  message("============================================================")
  message(sprintf("%% variance explained (OOB): %.1f%%", rf$rsq[length(rf$rsq)] * 100))

  imp <- importance(rf, type = 1)  # %IncMSE, permutation-based
  imp_df <- data.frame(variable = rownames(imp), pct_inc_mse = imp[, 1])
  imp_df <- imp_df[order(-imp_df$pct_inc_mse), ]
  print(imp_df, row.names = FALSE)

  list(rf = rf, importance = imp_df)
}

rf_2008 <- run_rf(sample_2008, STUDIED_PREDICTORS_2007_OPTIONB, CONTROLS_COUNTY_2008,
                   "recovery_ratio_2008", "2008 crisis")
rf_covid <- run_rf(sample_covid, STUDIED_PREDICTORS_2019_OPTIONB, CONTROLS_COUNTY_COVID,
                    "recovery_ratio_covid", "COVID crisis")

saveRDS(list(rf_2008 = rf_2008, rf_covid = rf_covid), "raw_data/32_optionB_rf_results.rds")
message("\nSaved raw_data/32_optionB_rf_results.rds")
