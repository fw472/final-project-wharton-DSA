# Random-forest models for county and state economic recovery
#
# Run:
#   source("random_forest_models.R")
#   Rscript random_forest_models.R
#
# Inputs:
#   spine_data.csv
#   state_spine.csv
#
# Outputs:
#   figures/random_forest/county/
#   figures/random_forest/state/
#
# The script fits separate Great Recession and COVID-19 models. It never uses
# peak, trough, or recovery employment as predictors because those variables
# are components of the recovery-ratio target.

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

# ---- Locate project ---------------------------------------------------------

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
  vapply(
    candidate_dirs,
    function(path) {
      file.exists(file.path(path, "spine_data.csv")) &&
        file.exists(file.path(path, "state_spine.csv"))
    },
    logical(1)
  )
][1]
if (is.na(project_dir)) {
  stop("Could not locate spine_data.csv and state_spine.csv.")
}

county_output <- file.path(project_dir, "figures", "random_forest", "county")
state_output <- file.path(project_dir, "figures", "random_forest", "state")
dir.create(county_output, recursive = TRUE, showWarnings = FALSE)
dir.create(state_output, recursive = TRUE, showWarnings = FALSE)

county_raw <- readr::read_csv(
  file.path(project_dir, "spine_data.csv"),
  col_types = cols(fips = col_character()),
  show_col_types = FALSE
)
state_raw <- readr::read_csv(
  file.path(project_dir, "state_spine.csv"),
  col_types = cols(state_fips = col_character()),
  show_col_types = FALSE
)

# ---- Data preparation -------------------------------------------------------

as_flag <- function(x) {
  if (is.logical(x)) return(x)
  result <- rep(NA, length(x))
  text <- tolower(as.character(x))
  result[text %in% c("true", "t", "1", "yes")] <- TRUE
  result[text %in% c("false", "f", "0", "no")] <- FALSE
  result
}

first_existing <- function(candidates, available) {
  answer <- candidates[candidates %in% available]
  if (length(answer)) answer[[1]] else NA_character_
}

prepare_recovery_data <- function(data, geography, crisis) {
  suffix <- if (crisis == "Great Recession") "2007" else "2019"
  outcome_suffix <- if (crisis == "Great Recession") "2008" else "covid"
  outcome_name <- paste0("recovery_ratio_", outcome_suffix)
  flag_name <- paste0("flag_implausible_", outcome_suffix)

  if (!outcome_name %in% names(data)) {
    stop("Missing outcome column: ", outcome_name)
  }

  id_columns <- if (geography == "county") {
    c("fips", "county_name")
  } else {
    c("state_fips", "state_abbr", "state_name")
  }

  # These are all pre-crisis predictors. Optional fields from an expanded
  # dataset—unemployment, working-age share, population trend, and industry
  # shares—are included automatically when present.
  standard_bases <- c(
    "poverty_rate", "median_hh_income", "total_population",
    "population_density", "proprietor_share", "net_migration_rate",
    "birth_rate",
    "pre_unemployment", "working_age_share", "pop_trend"
  )
  if (tolower(Sys.getenv("INCLUDE_SPARSE_EDUCATION", "false")) == "true") {
    standard_bases <- c(
      standard_bases, "pct_hs_plus", "pct_bachelors_plus"
    )
  }
  candidate_columns <- paste0(standard_bases, "_", suffix)

  # Support the alternate name total_pop_YEAR if used in a revised file.
  population_column <- first_existing(
    c(paste0("total_population_", suffix), paste0("total_pop_", suffix)),
    names(data)
  )
  candidate_columns <- setdiff(
    candidate_columns, paste0("total_population_", suffix)
  )
  if (!is.na(population_column)) {
    candidate_columns <- c(candidate_columns, population_column)
  }

  rural_column <- if (geography == "county") {
    if (suffix == "2007") "rucc_2003_2007" else "rucc_2013_2019"
  } else {
    paste0("pct_rural_", suffix)
  }

  industry_columns <- names(data)[
    str_detect(names(data), paste0("^share_.+_", suffix, "$"))
  ]
  selected <- unique(c(
    candidate_columns[candidate_columns %in% names(data)],
    rural_column[rural_column %in% names(data)],
    industry_columns
  ))

  if (length(selected) < 3) {
    stop("Too few pre-crisis predictors were found for ", geography, " ", crisis)
  }

  result <- data %>%
    select(all_of(id_columns), all_of(outcome_name), any_of(flag_name), all_of(selected))
  names(result)[names(result) == outcome_name] <- "recovery"
  if (flag_name %in% names(result)) {
    names(result)[names(result) == flag_name] <- "implausible"
  } else {
    result$implausible <- FALSE
  }

  # Give both periods identical feature names.
  renamed <- selected
  renamed <- str_remove(renamed, paste0("_", suffix, "$"))
  renamed[renamed %in% c("total_population", "total_pop")] <- "population"
  renamed[renamed == str_remove(rural_column, paste0("_", suffix, "$"))] <- "rurality"
  names(result)[match(selected, names(result))] <- renamed

  result <- result %>%
    mutate(
      crisis = crisis,
      implausible = as_flag(implausible),
      across(where(is.numeric), ~ if_else(is.finite(.x), .x, NA_real_))
    ) %>%
    filter(!is.na(recovery), !implausible | is.na(implausible)) %>%
    select(-implausible)

  # Log transforms reduce the visual dominance of highly skewed population
  # variables. For trees these transforms preserve the ordering of values.
  if ("population" %in% names(result)) {
    result <- result %>%
      mutate(log_population = if_else(population > 0, log(population), NA_real_)) %>%
      select(-population)
  }
  if ("population_density" %in% names(result)) {
    result <- result %>%
      mutate(
        log_density = if_else(
          population_density > 0, log(population_density), NA_real_
        )
      ) %>%
      select(-population_density)
  }
  if ("rurality" %in% names(result) && geography == "county") {
    result$rurality <- factor(result$rurality, ordered = FALSE)
  }

  result
}

county_data <- bind_rows(
  prepare_recovery_data(county_raw, "county", "Great Recession"),
  prepare_recovery_data(county_raw, "county", "COVID-19")
) %>%
  mutate(
    fips = str_pad(fips, 5, pad = "0"),
    crisis = factor(crisis, levels = c("Great Recession", "COVID-19"))
  )

state_data <- bind_rows(
  prepare_recovery_data(state_raw, "state", "Great Recession"),
  prepare_recovery_data(state_raw, "state", "COVID-19")
) %>%
  mutate(
    state_fips = str_pad(state_fips, 2, pad = "0"),
    crisis = factor(crisis, levels = c("Great Recession", "COVID-19"))
  )

identifier_names <- c(
  "fips", "county_name", "state_fips", "state_abbr", "state_name",
  "crisis", "recovery"
)

impute_from_training <- function(training, testing, predictors) {
  for (variable in predictors) {
    if (is.numeric(training[[variable]])) {
      replacement <- median(training[[variable]], na.rm = TRUE)
      if (!is.finite(replacement)) replacement <- 0
      training[[variable]][is.na(training[[variable]])] <- replacement
      testing[[variable]][is.na(testing[[variable]])] <- replacement
    } else {
      all_levels <- union(
        levels(factor(training[[variable]])),
        levels(factor(testing[[variable]]))
      )
      training[[variable]] <- factor(training[[variable]], levels = all_levels)
      testing[[variable]] <- factor(testing[[variable]], levels = all_levels)
      mode_value <- names(sort(table(training[[variable]]), decreasing = TRUE))[1]
      training[[variable]][is.na(training[[variable]])] <- mode_value
      testing[[variable]][is.na(testing[[variable]])] <- mode_value
      training[[variable]] <- droplevels(training[[variable]])
      testing[[variable]] <- factor(
        testing[[variable]], levels = levels(training[[variable]])
      )
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

model_metrics <- function(observed, predicted, validation) {
  tibble(
    validation = validation,
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAE = mean(abs(observed - predicted)),
    R_squared = 1 -
      sum((observed - predicted)^2) /
      sum((observed - mean(observed))^2)
  )
}

tune_forest <- function(training, formula, predictor_count, seed) {
  candidates <- unique(pmax(
    1L,
    pmin(
      predictor_count,
      as.integer(round(c(sqrt(predictor_count), predictor_count / 3, predictor_count / 2)))
    )
  ))
  tuning_grid <- tidyr::expand_grid(
    mtry = candidates,
    nodesize = c(1L, 5L, 10L)
  )
  results <- purrr::pmap_dfr(tuning_grid, function(mtry, nodesize) {
    set.seed(seed + mtry + 100 * nodesize)
    fit <- randomForest(
      formula, data = training, mtry = mtry, nodesize = nodesize,
      ntree = 300, importance = FALSE, na.action = na.omit
    )
    tibble(mtry = mtry, nodesize = nodesize, oob_mse = tail(fit$mse, 1))
  })
  list(
    best = results %>% slice_min(oob_mse, n = 1, with_ties = FALSE),
    results = results
  )
}

fit_county_forest <- function(data, crisis_name, seed) {
  model_data <- data %>% filter(crisis == crisis_name)
  predictors <- setdiff(names(model_data), identifier_names)
  predictors <- remove_unusable_predictors(model_data, predictors)

  set.seed(seed)
  training_rows <- sample(
    seq_len(nrow(model_data)), size = floor(0.80 * nrow(model_data))
  )
  training <- model_data[training_rows, ]
  testing <- model_data[-training_rows, ]
  imputed <- impute_from_training(training, testing, predictors)
  training <- imputed$training
  testing <- imputed$testing

  formula <- reformulate(predictors, response = "recovery")
  tuning <- tune_forest(training, formula, length(predictors), seed)
  best_mtry <- tuning$best$mtry[[1]]
  best_nodesize <- tuning$best$nodesize[[1]]
  set.seed(seed + 1000)
  model <- randomForest(
    formula, data = training, ntree = 1000, mtry = best_mtry,
    nodesize = best_nodesize,
    importance = TRUE, keep.forest = TRUE
  )
  prediction <- as.numeric(predict(model, newdata = testing))

  list(
    geography = "County", crisis = crisis_name, model = model,
    training = training, validation = testing, predictors = predictors,
    mtry = best_mtry, nodesize = best_nodesize,
    tuning = tuning$results,
    predictions = testing %>%
      transmute(
        geography = "County", fips, county_name, crisis = crisis_name,
        observed = recovery, predicted = prediction,
        residual = observed - predicted
      ),
    metrics = model_metrics(testing$recovery, prediction, "20% holdout")
  )
}

fit_state_forest <- function(data, crisis_name, seed) {
  model_data <- data %>% filter(crisis == crisis_name)
  predictors <- setdiff(names(model_data), identifier_names)
  predictors <- remove_unusable_predictors(model_data, predictors)
  imputed <- impute_from_training(model_data, model_data, predictors)
  model_data <- imputed$training

  formula <- reformulate(predictors, response = "recovery")
  tuning <- tune_forest(model_data, formula, length(predictors), seed)
  best_mtry <- tuning$best$mtry[[1]]
  best_nodesize <- tuning$best$nodesize[[1]]
  set.seed(seed + 1000)
  model <- randomForest(
    formula, data = model_data, ntree = 1000, mtry = best_mtry,
    nodesize = best_nodesize,
    importance = TRUE, keep.inbag = TRUE
  )
  prediction <- as.numeric(model$predicted)

  list(
    geography = "State", crisis = crisis_name, model = model,
    training = model_data, validation = model_data, predictors = predictors,
    mtry = best_mtry, nodesize = best_nodesize,
    tuning = tuning$results,
    predictions = model_data %>%
      transmute(
        geography = "State", state_fips, state_abbr, state_name,
        crisis = crisis_name,
        observed = recovery, predicted = prediction,
        residual = observed - predicted
      ),
    metrics = model_metrics(model_data$recovery, prediction, "Out-of-bag")
  )
}

# ---- Fit four separate models ----------------------------------------------

rf_models <- list(
  county_2008 = fit_county_forest(county_data, "Great Recession", 2008),
  county_covid = fit_county_forest(county_data, "COVID-19", 2020),
  state_2008 = fit_state_forest(state_data, "Great Recession", 12008),
  state_covid = fit_state_forest(state_data, "COVID-19", 12020)
)

extract_importance <- function(result) {
  values <- importance(result$model, type = 1, scale = FALSE)
  baseline_oob_mse <- tail(result$model$mse, 1)
  tibble(
    geography = result$geography,
    crisis = result$crisis,
    predictor = rownames(values),
    importance = 100 * values[, 1] / baseline_oob_mse
  )
}

importance_table <- map_dfr(rf_models, extract_importance)
prediction_table <- map_dfr(rf_models, "predictions")
metrics_table <- map_dfr(rf_models, function(x) {
  x$metrics %>%
    mutate(
      geography = x$geography,
      crisis = x$crisis,
      observations = nrow(x$validation),
      predictors = length(x$predictors),
      mtry = x$mtry,
      nodesize = x$nodesize
    )
}) %>%
  select(geography, crisis, validation, observations, predictors, mtry, everything())

pretty_predictor <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_replace("^pct ", "Percent ") %>%
    str_replace("^log ", "Log ") %>%
    str_to_sentence()
}

importance_table <- importance_table %>%
  mutate(predictor_label = pretty_predictor(predictor))

# Marginal prediction curves for the four most important numeric predictors.
partial_dependence <- function(result, importance_data, top_n = 4) {
  numeric_predictors <- result$predictors[
    vapply(result$training[result$predictors], is.numeric, logical(1))
  ]
  selected <- importance_data %>%
    filter(
      geography == result$geography,
      crisis == result$crisis,
      predictor %in% numeric_predictors
    ) %>%
    slice_max(importance, n = top_n, with_ties = FALSE) %>%
    pull(predictor)

  set.seed(44)
  reference <- result$training %>%
    slice_sample(n = min(500, nrow(result$training)))

  map_dfr(selected, function(variable) {
    grid <- unique(as.numeric(quantile(
      reference[[variable]], probs = seq(0.05, 0.95, length.out = 25),
      na.rm = TRUE
    )))
    map_dfr(grid, function(value) {
      new_data <- reference
      new_data[[variable]] <- value
      tibble(
        predictor = variable,
        predictor_value = value,
        predicted_recovery = mean(predict(result$model, newdata = new_data))
      )
    })
  }) %>%
    mutate(
      geography = result$geography,
      crisis = result$crisis,
      predictor_label = pretty_predictor(predictor)
    )
}

partial_table <- map_dfr(
  rf_models,
  ~ partial_dependence(.x, importance_table)
)

error_table <- map_dfr(rf_models, function(result) {
  tibble(
    geography = result$geography,
    crisis = result$crisis,
    trees = seq_along(result$model$mse),
    oob_mse = result$model$mse
  )
})

rf_tuning_table <- map_dfr(rf_models, function(result) {
  result$tuning %>%
    mutate(
      geography = result$geography,
      crisis = result$crisis
    )
})

# ---- Plots ------------------------------------------------------------------

crisis_colors <- c("Great Recession" = "#173F73", "COVID-19" = "#38BDF8")
theme_rf <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", color = "#173F73"),
      plot.subtitle = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

make_rf_plots <- function(geography_name) {
  predictions <- prediction_table %>% filter(geography == geography_name)
  importance_data <- importance_table %>%
    filter(geography == geography_name) %>%
    group_by(crisis) %>%
    slice_max(importance, n = 15, with_ties = FALSE) %>%
    ungroup()
  partial_data <- partial_table %>% filter(geography == geography_name)
  errors <- error_table %>% filter(geography == geography_name)
  tuning <- rf_tuning_table %>% filter(geography == geography_name)
  metrics <- metrics_table %>%
    filter(geography == geography_name) %>%
    select(crisis, RMSE, MAE, R_squared) %>%
    pivot_longer(-crisis, names_to = "metric", values_to = "value")

  observed_plot <-
    ggplot(predictions, aes(observed, predicted, color = crisis)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey45") +
    geom_point(alpha = 0.65, size = if (geography_name == "State") 2.2 else 1.3) +
    facet_wrap(~crisis, scales = "free") +
    scale_color_manual(values = crisis_colors) +
    labs(
      title = paste(geography_name, "random forest: observed versus predicted"),
      subtitle = if (geography_name == "County") {
        "Predictions are from a reproducible 20% holdout set"
      } else {
        "Predictions are out-of-bag estimates for the 51 state/DC observations"
      },
      x = "Observed recovery ratio", y = "Predicted recovery ratio",
      color = NULL
    ) +
    theme_rf() +
    theme(legend.position = "none")

  importance_plot <-
    ggplot(
      importance_data,
      aes(importance, reorder(predictor_label, importance), fill = crisis)
    ) +
    geom_col(width = 0.72) +
    facet_wrap(~crisis, scales = "free_y") +
    scale_fill_manual(values = crisis_colors) +
    labs(
      title = paste(geography_name, "random-forest variable importance"),
      subtitle = "Permutation importance: accuracy loss when each predictor is shuffled",
      x = "% increase in mean squared error", y = NULL, fill = NULL,
      caption = "Importance indicates predictive reliance, not direction or causality."
    ) +
    theme_rf() +
    theme(legend.position = "none")

  partial_plot <-
    ggplot(
      partial_data,
      aes(predictor_value, predicted_recovery, color = crisis)
    ) +
    geom_line(linewidth = 0.9) +
    facet_grid(crisis ~ predictor_label, scales = "free_x") +
    scale_color_manual(values = crisis_colors) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title = paste(geography_name, "random-forest response profiles"),
      subtitle = "Marginal prediction curves for the most important numeric predictors",
      x = "Predictor value", y = "Average predicted recovery ratio",
      color = NULL,
      caption = "Curves describe model behavior and should not be interpreted causally."
    ) +
    theme_rf(base_size = 8) +
    theme(legend.position = "none")

  error_plot <-
    ggplot(errors, aes(trees, oob_mse, color = crisis)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~crisis, scales = "free_y") +
    scale_color_manual(values = crisis_colors) +
    labs(
      title = paste(geography_name, "random-forest convergence"),
      subtitle = "Out-of-bag error should stabilize as trees are added",
      x = "Number of trees", y = "Out-of-bag mean squared error",
      color = NULL
    ) +
    theme_rf()

  performance_plot <-
    ggplot(metrics, aes(crisis, value, fill = crisis)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = number(value, accuracy = 0.001)), vjust = -0.3) +
    facet_wrap(~metric, scales = "free_y") +
    scale_fill_manual(values = crisis_colors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.17))) +
    labs(
      title = paste(geography_name, "random-forest performance"),
      subtitle = "Lower RMSE and MAE are better; higher R-squared is better",
      x = NULL, y = NULL, fill = NULL
    ) +
    theme_rf() +
    theme(legend.position = "none")

  tuning_plot <-
    ggplot(
      tuning,
      aes(factor(mtry), factor(nodesize), fill = oob_mse)
    ) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = number(oob_mse, accuracy = 0.0001)), size = 3) +
    facet_wrap(~crisis, scales = "free") +
    scale_fill_viridis_c(option = "C", direction = -1) +
    labs(
      title = paste(geography_name, "random-forest hyperparameter search"),
      subtitle = "Training-set out-of-bag MSE for combinations of mtry and node size",
      x = "Predictors sampled per split (mtry)",
      y = "Minimum terminal-node size",
      fill = "OOB MSE",
      caption = "The final model uses the lowest-OOB-error combination."
    ) +
    theme_rf()

  list(
    observed_vs_predicted = observed_plot,
    variable_importance = importance_plot,
    response_profiles = partial_plot,
    oob_convergence = error_plot,
    model_performance = performance_plot,
    hyperparameter_tuning = tuning_plot
  )
}

rf_plots <- list(
  county = make_rf_plots("County"),
  state = make_rf_plots("State")
)

save_plot_gallery <- function(plot_list, output_path, pdf_name) {
  file_names <- sprintf(
    "%02d_%s.png", seq_along(plot_list), names(plot_list)
  )
  walk2(plot_list, file_names, function(plot, file_name) {
    ggsave(
      file.path(output_path, file_name), plot,
      width = 10.5, height = 6.5, dpi = 300, bg = "white"
    )
  })
  grDevices::pdf(
    file.path(output_path, pdf_name),
    width = 10.5, height = 6.5, onefile = TRUE
  )
  walk(plot_list, print)
  grDevices::dev.off()
}

save_plot_gallery(rf_plots$county, county_output, "county_random_forest_graphs.pdf")
save_plot_gallery(rf_plots$state, state_output, "state_random_forest_graphs.pdf")

readr::write_csv(
  metrics_table %>% filter(geography == "County"),
  file.path(county_output, "county_random_forest_metrics.csv")
)
readr::write_csv(
  prediction_table %>% filter(geography == "County"),
  file.path(county_output, "county_random_forest_predictions.csv")
)
readr::write_csv(
  importance_table %>% filter(geography == "County"),
  file.path(county_output, "county_random_forest_importance.csv")
)
readr::write_csv(
  rf_tuning_table %>% filter(geography == "County"),
  file.path(county_output, "county_random_forest_tuning.csv")
)
readr::write_csv(
  metrics_table %>% filter(geography == "State"),
  file.path(state_output, "state_random_forest_metrics.csv")
)
readr::write_csv(
  prediction_table %>% filter(geography == "State"),
  file.path(state_output, "state_random_forest_predictions.csv")
)
readr::write_csv(
  importance_table %>% filter(geography == "State"),
  file.path(state_output, "state_random_forest_importance.csv")
)
readr::write_csv(
  rf_tuning_table %>% filter(geography == "State"),
  file.path(state_output, "state_random_forest_tuning.csv")
)

saveRDS(
  map(rf_models, "model"),
  file.path(project_dir, "figures", "random_forest", "fitted_random_forest_models.rds")
)

message(
  "Random-forest analysis complete.\nCounty outputs: ",
  normalizePath(county_output, winslash = "/", mustWork = FALSE),
  "\nState outputs: ",
  normalizePath(state_output, winslash = "/", mustWork = FALSE)
)
