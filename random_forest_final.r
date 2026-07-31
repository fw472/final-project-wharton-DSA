# Random-forest model for county economic recovery
# (optionc dataset, county only, user‑specified predictors)
# Hyperparameter tuning: 5 mtry × 5 nodesize
#
# Predictors:
#   poverty_rate, median_hh_income, log_density (from population_density),
#   rucc (factor), proprietor_share, net_migration_rate,
#   pct_hs_plus, pct_bachelors_plus, birth_rate, wealth_band (factor),
#   working_age_share, all industry share variables, region (factor)
#
# Target: recovery ratio (Great Recession or COVID‑19)
#
# Outputs: figures/random_forest_optionc_county_fullspec/

required_packages <- c("tidyverse", "randomForest", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(randomForest)
  library(scales)
})

# ---- Locate project directory (based on spine_data_optionc.csv) -------------
command_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
command_dir <- if (length(command_file)) {
  dirname(normalizePath(sub("^--file=", "", command_file[[1]]), mustWork = FALSE))
} else {
  character()
}
source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
source_dir <- if (!is.null(source_file)) {
  dirname(normalizePath(source_file, mustWork = FALSE))
} else {
  character()
}
candidate_dirs <- unique(c(command_dir, source_dir, getwd()))
project_dir <- candidate_dirs[
  vapply(candidate_dirs,
         function(path) file.exists(file.path(path, "spine_data_optionc.csv")),
         logical(1))
][1]
if (is.na(project_dir)) stop("Could not locate spine_data_optionc.csv.")

county_output <- file.path(project_dir, "figures",
                           "random_forest_optionc_county_fullspec")
dir.create(county_output, recursive = TRUE, showWarnings = FALSE)

county_raw <- readr::read_csv(
  file.path(project_dir, "spine_data_optionc.csv"),
  col_types = cols(fips = col_character()),
  show_col_types = FALSE
)

# ---- Helper functions -------------------------------------------------------
as_flag <- function(x) {
  if (is.logical(x)) return(x)
  result <- rep(NA, length(x))
  text <- tolower(as.character(x))
  result[text %in% c("true", "t", "1", "yes")] <- TRUE
  result[text %in% c("false", "f", "0", "no")] <- FALSE
  result
}

model_metrics <- function(observed, predicted, validation) {
  tibble(
    validation = validation,
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAE  = mean(abs(observed - predicted)),
    R_squared = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
  )
}

pretty_predictor <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_replace("^pct ", "Percent ") %>%
    str_replace("^log ", "Log ") %>%
    str_to_sentence()
}

# Map state FIPS code (first two digits of county FIPS) to Census region
map_region <- function(fips) {
  state_code <- str_sub(fips, 1, 2)
  case_when(
    state_code %in% c("09","23","25","33","44","50","34","36","42") ~ "Northeast",
    state_code %in% c("17","18","19","20","26","27","29","31","38","39","46","55") ~ "Midwest",
    state_code %in% c("10","11","12","13","24","37","45","51","54","01","21","28","47","05","22","40","48") ~ "South",
    state_code %in% c("04","08","16","30","32","35","49","53","56","02","06","15","41") ~ "West",
    TRUE ~ "Other"
  )
}

# ---- Data preparation (county, optionc, user‑specified predictors) ----------
prepare_recovery_data <- function(data, crisis) {
  suffix         <- if (crisis == "Great Recession") "2007" else "2019"
  outcome_suffix <- if (crisis == "Great Recession") "2008" else "covid"
  outcome_name   <- paste0("recovery_ratio_", outcome_suffix)
  flag_name      <- paste0("flag_implausible_", outcome_suffix)

  if (!outcome_name %in% names(data)) stop("Missing outcome column: ", outcome_name)

  # Core predictors (names without year suffix)
  core_vars <- c("poverty_rate", "median_hh_income", "population_density",
                 "proprietor_share", "net_migration_rate",
                 "pct_hs_plus", "pct_bachelors_plus", "birth_rate",
                 "working_age_share")

  # RUCC and wealth band (names vary by year)
  rucc_var    <- if (suffix == "2007") "rucc_2003_2007" else "rucc_2013_2019"
  wealth_var  <- paste0("wealth_band_", outcome_suffix)

  # All industry share columns ending with the suffix
  share_cols <- names(data)[grepl(paste0("share.*_", suffix, "$"), names(data))]

  # Combine all source columns (the raw names in the CSV)
  source_cols <- c(
    paste0(core_vars, "_", suffix),
    rucc_var,
    wealth_var,
    share_cols
  )

  # Ensure they exist
  missing_cols <- setdiff(source_cols, names(data))
  if (length(missing_cols)) {
    stop("Missing predictor columns: ", paste(missing_cols, collapse = ", "))
  }

  id_columns <- c("fips", "county_name")

  result <- data %>%
    select(all_of(id_columns), all_of(outcome_name), any_of(flag_name),
           all_of(source_cols))

  names(result)[names(result) == outcome_name] <- "recovery"
  if (flag_name %in% names(result)) {
    names(result)[names(result) == flag_name] <- "implausible"
  } else {
    result$implausible <- FALSE
  }

# --------------------------------------------------------------------------
# Rename columns to common names
# --------------------------------------------------------------------------

rename_map <- c()

# Core variables
for (v in core_vars) {
  old_name <- paste0(v, "_", suffix)
  if (old_name %in% names(result)) {
    rename_map[old_name] <- v
  }
}

# Industry share variables
for (v in share_cols) {
  rename_map[v] <- str_remove(v, paste0("_", suffix, "$"))
}

# RUCC
if (rucc_var %in% names(result)) {
  rename_map[rucc_var] <- "rucc"
}

# Wealth band
if (wealth_var %in% names(result)) {
  rename_map[wealth_var] <- "wealth_band"
}

# Apply renaming
for (old in names(rename_map)) {
  names(result)[names(result) == old] <- rename_map[[old]]
}

  result <- result %>%
    mutate(
      crisis      = crisis,
      implausible = as_flag(implausible),
      across(where(is.numeric), ~ if_else(is.finite(.x), .x, NA_real_))
    ) %>%
    filter(!is.na(recovery), !implausible | is.na(implausible)) %>%
    select(-implausible)

  # Log transform population_density -> log_density
  result <- result %>%
    mutate(
      log_density = if_else(population_density > 0, log(population_density), NA_real_)
    ) %>%
    select(-population_density)

  # Convert categoricals to factors
  result$rucc        <- factor(result$rucc, ordered = FALSE)
  if ("wealth_band" %in% names(result)) {
    result$wealth_band <- factor(result$wealth_band, ordered = FALSE)
  }

  # Create region from FIPS
  result <- result %>%
    mutate(region = factor(map_region(fips), 
                           levels = c("Northeast", "Midwest", "South", "West")))

  result
}

county_data <- bind_rows(
  prepare_recovery_data(county_raw, "Great Recession"),
  prepare_recovery_data(county_raw, "COVID-19")
) %>%
  mutate(
    fips   = str_pad(fips, 5, pad = "0"),
    crisis = factor(crisis, levels = c("Great Recession", "COVID-19"))
  )

# Identifiers that are NOT predictors
identifier_names <- c("fips", "county_name", "crisis", "recovery")

# ---- Imputation and predictor cleaning -------------------------------------
impute_from_training <- function(training, testing, predictors) {
  for (variable in predictors) {
    if (is.numeric(training[[variable]])) {
      replacement <- median(training[[variable]], na.rm = TRUE)
      if (!is.finite(replacement)) replacement <- 0
      training[[variable]][is.na(training[[variable]])] <- replacement
      testing[[variable]][is.na(testing[[variable]])]  <- replacement
    } else {
      all_levels <- union(
        levels(factor(training[[variable]])),
        levels(factor(testing[[variable]]))
      )
      training[[variable]] <- factor(training[[variable]], levels = all_levels)
      testing[[variable]]  <- factor(testing[[variable]],  levels = all_levels)
      mode_value <- names(sort(table(training[[variable]]), decreasing = TRUE))[1]
      training[[variable]][is.na(training[[variable]])] <- mode_value
      testing[[variable]][is.na(testing[[variable]])]  <- mode_value
      training[[variable]] <- droplevels(training[[variable]])
      testing[[variable]]  <- factor(testing[[variable]],
                                     levels = levels(training[[variable]]))
    }
  }
  list(training = training, testing = testing)
}

remove_unusable_predictors <- function(data, predictors) {
  predictors[
    vapply(predictors, function(variable) {
      values <- data[[variable]]
      length(unique(values[!is.na(values)])) > 1
    }, logical(1))
  ]
}

# ---- Tuning with 5 mtry × 5 nodesize grid -----------------------------------
tune_forest <- function(training, formula, predictor_count, seed) {
  mtry_values <- unique(pmax(1L, pmin(
    predictor_count,
    as.integer(round(seq(1, predictor_count, length.out = 5)))
  )))
  nodesize_values <- c(1L, 3L, 5L, 7L, 10L)

  tuning_grid <- tidyr::expand_grid(mtry = mtry_values, nodesize = nodesize_values)
  results <- purrr::pmap_dfr(tuning_grid, function(mtry, nodesize) {
    set.seed(seed + mtry + 100 * nodesize)
    fit <- randomForest(
      formula, data = training, mtry = mtry, nodesize = nodesize,
      ntree = 300, importance = FALSE, na.action = na.omit
    )
    oob_mse <- tail(fit$mse, 1)
    train_pred <- predict(fit, newdata = training)
    train_mse <- mean((train_pred - training$recovery)^2)
    tibble(mtry = mtry, nodesize = nodesize,
           train_rmse = sqrt(train_mse), val_rmse = sqrt(oob_mse))
  })
  list(best = results %>% slice_min(val_rmse, n = 1, with_ties = FALSE),
       results = results)
}

# ---- Model fitting (county only) -------------------------------------------
fit_county_forest <- function(data, crisis_name, seed) {
  model_data <- data %>% filter(crisis == crisis_name)
  predictors <- setdiff(names(model_data), identifier_names)
  predictors <- remove_unusable_predictors(model_data, predictors)

  set.seed(seed)
  training_rows <- sample(seq_len(nrow(model_data)),
                          size = floor(0.80 * nrow(model_data)))
  training <- model_data[training_rows, ]
  testing  <- model_data[-training_rows, ]
  imputed <- impute_from_training(training, testing, predictors)
  training <- imputed$training
  testing  <- imputed$testing

  formula <- reformulate(predictors, response = "recovery")
  tuning <- tune_forest(training, formula, length(predictors), seed)
  best_mtry     <- tuning$best$mtry[[1]]
  best_nodesize <- tuning$best$nodesize[[1]]
  set.seed(seed + 1000)
  model <- randomForest(
    formula, data = training, ntree = 1000, mtry = best_mtry,
    nodesize = best_nodesize, importance = TRUE, keep.forest = TRUE
  )
  prediction <- as.numeric(predict(model, newdata = testing))

  list(
    geography   = "County",
    crisis      = crisis_name,
    model       = model,
    training    = training,
    validation  = testing,
    predictors  = predictors,
    mtry        = best_mtry,
    nodesize    = best_nodesize,
    tuning      = tuning$results,
    predictions = testing %>%
      transmute(geography = "County", fips, county_name,
                crisis = crisis_name, observed = recovery,
                predicted = prediction, residual = observed - predicted),
    metrics     = model_metrics(testing$recovery, prediction, "20% holdout")
  )
}

# ---- Fit models for both crises --------------------------------------------
rf_models <- list(
  county_2008  = fit_county_forest(county_data, "Great Recession", 2008),
  county_covid = fit_county_forest(county_data, "COVID-19", 2020)
)

# ---- Post‑processing tables ------------------------------------------------
extract_importance <- function(result) {
  values <- importance(result$model, type = 1, scale = FALSE)
  baseline_oob_mse <- tail(result$model$mse, 1)
  tibble(geography  = result$geography,
         crisis     = result$crisis,
         predictor  = rownames(values),
         importance = 100 * values[, 1] / baseline_oob_mse)
}

importance_table <- map_dfr(rf_models, extract_importance) %>%
  mutate(predictor_label = pretty_predictor(predictor))

prediction_table <- map_dfr(rf_models, "predictions")

metrics_table <- map_dfr(rf_models, function(x) {
  x$metrics %>%
    mutate(geography     = x$geography,
           crisis        = x$crisis,
           observations  = nrow(x$validation),
           predictors    = length(x$predictors),
           mtry          = x$mtry,
           nodesize      = x$nodesize)
}) %>%
  select(geography, crisis, validation, observations, predictors, mtry, everything())

# Partial dependence (top 4 numeric predictors)
partial_dependence <- function(result, importance_data, top_n = 4) {
  numeric_predictors <- result$predictors[
    vapply(result$training[result$predictors], is.numeric, logical(1))
  ]
  selected <- importance_data %>%
    filter(geography == result$geography,
           crisis    == result$crisis,
           predictor %in% numeric_predictors) %>%
    slice_max(importance, n = top_n, with_ties = FALSE) %>%
    pull(predictor)

  set.seed(44)
  reference <- result$training %>%
    slice_sample(n = min(500, nrow(result$training)))

  map_dfr(selected, function(variable) {
    grid <- unique(as.numeric(quantile(reference[[variable]],
                                       probs = seq(0.05, 0.95, length.out = 25),
                                       na.rm = TRUE)))
    map_dfr(grid, function(value) {
      new_data <- reference
      new_data[[variable]] <- value
      tibble(predictor           = variable,
             predictor_value     = value,
             predicted_recovery  = mean(predict(result$model, newdata = new_data)))
    })
  }) %>%
    mutate(geography        = result$geography,
           crisis           = result$crisis,
           predictor_label  = pretty_predictor(predictor))
}
partial_table <- map_dfr(rf_models, ~ partial_dependence(.x, importance_table))

error_table <- map_dfr(rf_models, function(result) {
  tibble(geography = result$geography,
         crisis    = result$crisis,
         trees     = seq_along(result$model$mse),
         oob_mse   = result$model$mse)
})

rf_tuning_table <- map_dfr(rf_models, function(result) {
  result$tuning %>%
    mutate(geography = result$geography, crisis = result$crisis)
})

# ---- Plots ------------------------------------------------------------------
crisis_colors <- c("Great Recession" = "#173F73", "COVID-19" = "#38BDF8")
theme_rf <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", color = "#173F73"),
          plot.subtitle = element_text(color = "grey30"),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom")
}

predictions    <- prediction_table
importance_data <- importance_table %>%
  group_by(crisis) %>%
  slice_max(importance, n = 15, with_ties = FALSE) %>%   # show top 15
  ungroup()
partial_data   <- partial_table
errors         <- error_table
tuning         <- rf_tuning_table
metrics        <- metrics_table %>%
  select(crisis, RMSE, MAE, R_squared) %>%
  pivot_longer(-crisis, names_to = "metric", values_to = "value")

# 1. Observed vs predicted
p_obs <- ggplot(predictions, aes(observed, predicted, color = crisis)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey45") +
  geom_point(alpha = 0.65, size = 1.3) +
  facet_wrap(~crisis, scales = "free") +
  scale_color_manual(values = crisis_colors) +
  labs(title = "County random forest (full spec): observed vs predicted",
       subtitle = "20% holdout set",
       x = "Observed recovery ratio", y = "Predicted recovery ratio") +
  theme_rf() + theme(legend.position = "none")

# 2. Variable importance (top 15)
p_imp <- ggplot(importance_data,
                aes(importance, reorder(predictor_label, importance), fill = crisis)) +
  geom_col(width = 0.72) +
  facet_wrap(~crisis, scales = "free_y") +
  scale_fill_manual(values = crisis_colors) +
  labs(title = "County variable importance (full spec)",
       subtitle = "% increase in MSE when predictor is shuffled",
       x = "% increase in MSE", y = NULL,
       caption = "Importance indicates predictive reliance, not causality.") +
  theme_rf() + theme(legend.position = "none")

# 3. Partial dependence (top 4 numeric)
p_pd <- ggplot(partial_data,
               aes(predictor_value, predicted_recovery, color = crisis)) +
  geom_line(linewidth = 0.9) +
  facet_grid(crisis ~ predictor_label, scales = "free_x") +
  scale_color_manual(values = crisis_colors) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(title = "County response profiles (full spec)",
       subtitle = "Marginal prediction curves for top numeric predictors",
       x = "Predictor value", y = "Average predicted recovery ratio",
       caption = "Curves describe model behaviour, not causation.") +
  theme_rf(base_size = 8) + theme(legend.position = "none")

# 4. OOB convergence
p_oob <- ggplot(errors, aes(trees, oob_mse, color = crisis)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~crisis, scales = "free_y") +
  scale_color_manual(values = crisis_colors) +
  labs(title = "County convergence (full spec)",
       subtitle = "OOB MSE stabilises as trees are added",
       x = "Number of trees", y = "OOB MSE") +
  theme_rf()

# 5. Performance metrics
p_met <- ggplot(metrics, aes(crisis, value, fill = crisis)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = number(value, accuracy = 0.001)), vjust = -0.3) +
  facet_wrap(~metric, scales = "free_y") +
  scale_fill_manual(values = crisis_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.17))) +
  labs(title = "County performance (full spec)",
       subtitle = "Lower RMSE/MAE better; higher R² better",
       x = NULL, y = NULL) +
  theme_rf() + theme(legend.position = "none")

# 6. Hyperparameter heatmap (5x5)
p_heat <- ggplot(tuning, aes(factor(mtry), factor(nodesize), fill = val_rmse)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = number(val_rmse, accuracy = 0.0001)), size = 3) +
  facet_wrap(~crisis, scales = "free") +
  scale_fill_viridis_c(option = "C", direction = -1) +
  labs(title = "County hyperparameter tuning (5 mtry × 5 nodesize)",
       subtitle = "Validation (OOB) RMSE per mtry / nodesize",
       x = "mtry", y = "nodesize", fill = "Val. RMSE") +
  theme_rf()

# 7. Train/validation loss curves
tuning_long <- tuning %>%
  pivot_longer(cols = c(train_rmse, val_rmse),
               names_to = "dataset", values_to = "rmse") %>%
  mutate(dataset = recode(dataset, train_rmse = "Training", val_rmse = "Validation"))

p_loss <- ggplot(tuning_long,
                 aes(mtry, rmse, color = dataset, linetype = dataset)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  facet_grid(crisis ~ nodesize, labeller = labeller(nodesize = label_both)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "County train/validation loss vs hyperparameters",
       subtitle = "Lines show RMSE for each nodesize across mtry",
       x = "mtry", y = "RMSE", caption = "Validation = OOB error") +
  theme_rf()

# 8. Q‑Q residuals
p_qq <- ggplot(predictions, aes(sample = residual)) +
  stat_qq(alpha = 0.6) + stat_qq_line(color = "red") +
  facet_wrap(~crisis, scales = "free") +
  labs(title = "County Q‑Q plot of residuals",
       subtitle = "Residual normality check",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_rf()

# Combine and save
plots <- list(
  observed_vs_predicted   = p_obs,
  variable_importance     = p_imp,
  response_profiles       = p_pd,
  oob_convergence         = p_oob,
  model_performance       = p_met,
  hyperparameter_tuning   = p_heat,
  tuning_train_val_loss   = p_loss,
  qq_residuals            = p_qq
)

save_plot_gallery <- function(plot_list, output_path, pdf_name) {
  file_names <- sprintf("county_%02d_%s.png", seq_along(plot_list), names(plot_list))
  walk2(plot_list, file_names, function(plot, file_name) {
    ggsave(file.path(output_path, file_name), plot,
           width = 10.5, height = 6.5, dpi = 300, bg = "white")
  })
  pdf(file.path(output_path, pdf_name), width = 10.5, height = 6.5, onefile = TRUE)
  walk(plot_list, print)
  dev.off()
}

save_plot_gallery(plots, county_output, "county_random_forest_fullspec.pdf")

# Export tables
readr::write_csv(metrics_table,      file.path(county_output, "county_random_forest_metrics.csv"))
readr::write_csv(prediction_table,   file.path(county_output, "county_random_forest_predictions.csv"))
readr::write_csv(importance_table,   file.path(county_output, "county_random_forest_importance.csv"))
readr::write_csv(rf_tuning_table,    file.path(county_output, "county_random_forest_tuning.csv"))

# Save models
saveRDS(map(rf_models, "model"),
        file.path(county_output, "fitted_random_forest_fullspec.rds"))

message("County full‑spec random forest analysis complete (5x5 tuning).\nOutputs: ",
        normalizePath(county_output, winslash = "/"))
print(metrics_table)