# Keras 3 neural networks in R for county and state employment recovery
#
# Run from a clean R session:
#   source("keras_neural_network_models.R")
# or:
#   Rscript keras_neural_network_models.R
#
# The script uses Keras 3 with the PyTorch backend by default because PyTorch is
# already installed in this project environment. To use TensorFlow instead,
# install its Python backend and set KERAS_BACKEND=tensorflow before starting R.

# ---- Configure Keras before loading keras3/reticulate -----------------------

if (!nzchar(Sys.getenv("KERAS_BACKEND"))) {
  Sys.setenv(KERAS_BACKEND = "torch")
}
if (!nzchar(Sys.getenv("RETICULATE_PYTHON"))) {
  python_path <- Sys.which("python")
  if (nzchar(python_path)) {
    Sys.setenv(
      RETICULATE_PYTHON = normalizePath(
        python_path, winslash = "/", mustWork = TRUE
      )
    )
  }
}

required_packages <- c(
  "tidyverse", "scales", "keras3", "reticulate"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing R package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(keras3)
  library(reticulate)
})

if (!reticulate::py_module_available("keras")) {
  stop(
    "The Python 'keras' package is unavailable to reticulate.\n",
    "Install it with:\n",
    "reticulate::py_install('keras', pip = TRUE)\n",
    "Then restart R before rerunning this script."
  )
}
if (
  identical(keras3::config_backend(), "torch") &&
    !reticulate::py_module_available("torch")
) {
  stop(
    "KERAS_BACKEND=torch, but the Python 'torch' package is unavailable.\n",
    "Install PyTorch or set KERAS_BACKEND to another installed backend."
  )
}

message("Keras backend: ", keras3::config_backend())

# ---- Locate inputs and outputs ---------------------------------------------

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

county_output <- file.path(
  project_dir, "figures", "keras_r", "county"
)
state_output <- file.path(
  project_dir, "figures", "keras_r", "state"
)
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
  text <- tolower(as.character(x))
  result <- rep(NA, length(text))
  result[text %in% c("true", "t", "1", "yes")] <- TRUE
  result[text %in% c("false", "f", "0", "no")] <- FALSE
  result
}

prepare_task <- function(data, geography, crisis) {
  geography_name <- geography
  crisis_name <- crisis
  year <- if (crisis == "Great Recession") "2007" else "2019"
  outcome_suffix <- if (crisis == "Great Recession") "2008" else "covid"
  outcome_name <- paste0("recovery_ratio_", outcome_suffix)
  flag_name <- paste0("flag_implausible_", outcome_suffix)

  if (geography == "County") {
    id_columns <- c("fips", "county_name")
    rural_name <- if (year == "2007") "rucc_2003_2007" else "rucc_2013_2019"
  } else {
    id_columns <- c("state_fips", "state_abbr", "state_name")
    rural_name <- paste0("pct_rural_", year)
  }

  source_columns <- c(
    poverty_rate = paste0("poverty_rate_", year),
    median_hh_income = paste0("median_hh_income_", year),
    total_population = paste0("total_population_", year),
    rurality = rural_name,
    population_density = paste0("population_density_", year),
    proprietor_share = paste0("proprietor_share_", year),
    net_migration_rate = paste0("net_migration_rate_", year),
    birth_rate = paste0("birth_rate_", year)
  )

  if (tolower(Sys.getenv("INCLUDE_SPARSE_EDUCATION", "false")) == "true") {
    source_columns <- c(
      source_columns,
      pct_hs_plus = paste0("pct_hs_plus_", year),
      pct_bachelors_plus = paste0("pct_bachelors_plus_", year)
    )
  }

  missing <- setdiff(
    c(id_columns, outcome_name, source_columns), names(data)
  )
  if (length(missing)) {
    stop(
      geography, " ", crisis, " data are missing: ",
      paste(missing, collapse = ", ")
    )
  }

  result <- data %>%
    select(
      all_of(id_columns), all_of(outcome_name), any_of(flag_name),
      all_of(unname(source_columns))
    )
  names(result)[match(unname(source_columns), names(result))] <- names(source_columns)
  names(result)[names(result) == outcome_name] <- "recovery"
  if (flag_name %in% names(result)) {
    names(result)[names(result) == flag_name] <- "implausible"
  } else {
    result$implausible <- FALSE
  }

  result <- result %>%
    mutate(
      implausible = as_flag(implausible),
      recovery = as.numeric(recovery),
      across(all_of(names(source_columns)), as.numeric),
      log_median_income = if_else(
        median_hh_income > 0, log(median_hh_income), NA_real_
      ),
      log_population = if_else(
        total_population > 0, log(total_population), NA_real_
      ),
      log_density = if_else(
        population_density > 0, log(population_density), NA_real_
      )
    ) %>%
    filter(!is.na(recovery), !implausible | is.na(implausible)) %>%
    select(
      all_of(id_columns), recovery,
      poverty_rate, log_median_income, log_population, rurality,
      log_density, proprietor_share, net_migration_rate, birth_rate,
      any_of(c("pct_hs_plus", "pct_bachelors_plus"))
    ) %>%
    mutate(
      geography = geography_name,
      crisis = crisis_name,
      group = if (geography_name == "County") {
        str_sub(fips, 1, 2)
      } else {
        state_fips
      }
    )

  if (geography == "County") {
    result$rurality <- factor(result$rurality, ordered = FALSE)
  }
  result
}

county_tasks <- list(
  `Great Recession` = prepare_task(
    county_raw, "County", "Great Recession"
  ),
  `COVID-19` = prepare_task(
    county_raw, "County", "COVID-19"
  )
)
state_tasks <- list(
  `Great Recession` = prepare_task(
    state_raw, "State", "Great Recession"
  ),
  `COVID-19` = prepare_task(
    state_raw, "State", "COVID-19"
  )
)

identifier_columns <- c(
  "fips", "county_name", "state_fips", "state_abbr", "state_name",
  "recovery", "geography", "crisis", "group"
)

task_predictors <- function(data) {
  setdiff(names(data), identifier_columns)
}

# ---- Fold-specific preprocessing -------------------------------------------

fit_preprocessor <- function(training, predictors) {
  categorical <- predictors[
    vapply(training[predictors], is.factor, logical(1))
  ]
  numeric <- setdiff(predictors, categorical)

  medians <- vapply(
    training[numeric],
    function(x) {
      value <- median(x[is.finite(x)], na.rm = TRUE)
      if (is.finite(value)) value else 0
    },
    numeric(1)
  )
  modes <- vapply(
    training[categorical],
    function(x) {
      values <- as.character(x)
      values <- values[!is.na(values)]
      if (!length(values)) "Missing" else names(sort(table(values), decreasing = TRUE))[1]
    },
    character(1)
  )
  levels_list <- lapply(
    categorical,
    function(x) {
      values <- unique(c(as.character(training[[x]]), modes[[x]]))
      sort(values[!is.na(values)])
    }
  )
  names(levels_list) <- categorical

  raw <- as.data.frame(training[predictors])
  for (name in numeric) {
    raw[[name]][!is.finite(raw[[name]])] <- medians[[name]]
  }
  for (name in categorical) {
    values <- as.character(raw[[name]])
    values[is.na(values)] <- modes[[name]]
    raw[[name]] <- factor(values, levels = levels_list[[name]])
  }
  matrix <- stats::model.matrix(~ . - 1, data = raw)
  centers <- colMeans(matrix)
  scales <- apply(matrix, 2, stats::sd)
  scales[!is.finite(scales) | scales < 1e-12] <- 1

  list(
    predictors = predictors, numeric = numeric, categorical = categorical,
    medians = medians, modes = modes, levels = levels_list,
    matrix_columns = colnames(matrix), centers = centers, scales = scales
  )
}

apply_preprocessor <- function(data, prep) {
  raw <- as.data.frame(data[prep$predictors])
  for (name in prep$numeric) {
    raw[[name]][!is.finite(raw[[name]])] <- prep$medians[[name]]
  }
  for (name in prep$categorical) {
    values <- as.character(raw[[name]])
    invalid <- is.na(values) | !values %in% prep$levels[[name]]
    values[invalid] <- prep$modes[[name]]
    raw[[name]] <- factor(values, levels = prep$levels[[name]])
  }
  matrix <- stats::model.matrix(~ . - 1, data = raw)

  missing_columns <- setdiff(prep$matrix_columns, colnames(matrix))
  if (length(missing_columns)) {
    additions <- matrix(
      0, nrow = nrow(matrix), ncol = length(missing_columns),
      dimnames = list(NULL, missing_columns)
    )
    matrix <- cbind(matrix, additions)
  }
  matrix <- matrix[, prep$matrix_columns, drop = FALSE]
  matrix <- sweep(matrix, 2, prep$centers, "-")
  matrix <- sweep(matrix, 2, prep$scales, "/")
  storage.mode(matrix) <- "double"
  matrix
}

# ---- Network and training ---------------------------------------------------

build_network <- function(input_features) {
  width_1 <- max(32L, min(64L, as.integer(input_features * 4L)))
  width_2 <- max(16L, as.integer(width_1 / 2L))
  width_3 <- max(8L, as.integer(width_2 / 2L))

  keras_model_sequential(input_shape = input_features, name = "recovery_network") |>
    layer_dense(
      units = width_1,
      kernel_regularizer = regularizer_l2(1e-4)
    ) |>
    layer_batch_normalization() |>
    layer_activation("relu") |>
    layer_dropout(rate = 0.30) |>
    layer_dense(
      units = width_2,
      kernel_regularizer = regularizer_l2(1e-4)
    ) |>
    layer_batch_normalization() |>
    layer_activation("relu") |>
    layer_dropout(rate = 0.20) |>
    layer_dense(
      units = width_3,
      activation = "relu",
      kernel_regularizer = regularizer_l2(1e-4)
    ) |>
    layer_dropout(rate = 0.10) |>
    layer_dense(units = 1, activation = "linear") |>
    compile(
      optimizer = optimizer_adam_w(
        learning_rate = 0.002,
        weight_decay = 1e-4,
        clipnorm = 5
      ),
      loss = "mse",
      metrics = list(metric_mean_absolute_error(name = "mae")),
      jit_compile = FALSE
    )
}

history_table <- function(history, geography, crisis, fold) {
  metrics <- history$metrics
  tibble(
    geography = geography,
    crisis = crisis,
    fold = fold,
    epoch = seq_along(metrics$loss),
    training_loss = as.numeric(metrics$loss),
    validation_loss = as.numeric(metrics$val_loss),
    training_mae = as.numeric(metrics$mae),
    validation_mae = as.numeric(metrics$val_mae),
    learning_rate = if ("learning_rate" %in% names(metrics)) {
      as.numeric(metrics$learning_rate)
    } else {
      NA_real_
    }
  )
}

train_network <- function(
    training, validation, predictors, seed,
    geography, crisis, fold
) {
  keras3::clear_session()
  set.seed(seed)
  keras3::set_random_seed(seed)

  prep <- fit_preprocessor(training, predictors)
  x_train <- apply_preprocessor(training, prep)
  x_validation <- apply_preprocessor(validation, prep)
  target_mean <- mean(training$recovery)
  target_sd <- stats::sd(training$recovery)
  if (!is.finite(target_sd) || target_sd < 1e-12) target_sd <- 1
  y_train <- (training$recovery - target_mean) / target_sd
  y_validation <- (validation$recovery - target_mean) / target_sd

  model <- build_network(ncol(x_train))
  callbacks <- list(
    callback_early_stopping(
      monitor = "val_loss", patience = 20,
      min_delta = 1e-5, restore_best_weights = TRUE
    ),
    callback_reduce_lr_on_plateau(
      monitor = "val_loss", factor = 0.5,
      patience = 8, min_lr = 1e-5
    ),
    callback_terminate_on_nan()
  )

  batch_size <- min(128L, max(8L, as.integer(nrow(x_train) / 4L)))
  history <- model |>
    fit(
      x = x_train, y = y_train,
      validation_data = list(x_validation, y_validation),
      epochs = 200, batch_size = batch_size,
      callbacks = callbacks, verbose = 0,
      shuffle = TRUE
    )

  list(
    model = model, preprocessor = prep,
    target_mean = target_mean, target_sd = target_sd,
    history = history_table(
      history, geography, crisis, fold
    ),
    batch_size = batch_size
  )
}

predict_network <- function(fit, data) {
  x <- apply_preprocessor(data, fit$preprocessor)
  standardized <- as.numeric(
    predict(fit$model, x, verbose = 0)
  )
  standardized * fit$target_sd + fit$target_mean
}

regression_metrics <- function(data, validation_method) {
  tibble(
    validation = validation_method,
    observations = nrow(data),
    RMSE = sqrt(mean((data$observed - data$predicted)^2)),
    MAE = mean(abs(data$observed - data$predicted)),
    R_squared = 1 -
      sum((data$observed - data$predicted)^2) /
      sum((data$observed - mean(data$observed))^2)
  )
}

permutation_importance <- function(
    fit, assessment, predictors, seed
) {
  set.seed(seed)
  baseline <- mean(
    (assessment$recovery - predict_network(fit, assessment))^2
  )
  map_dfr(predictors, function(predictor) {
    shuffled <- assessment
    shuffled[[predictor]] <- sample(shuffled[[predictor]])
    shuffled_mse <- mean(
      (assessment$recovery - predict_network(fit, shuffled))^2
    )
    tibble(
      predictor = predictor,
      importance_pct_mse = 100 * (shuffled_mse - baseline) / baseline
    )
  })
}

# ---- County 70/15/15 assessment --------------------------------------------

fit_county_task <- function(data, crisis, seed) {
  message("Training County ", crisis, " Keras model...")
  predictors <- task_predictors(data)
  set.seed(seed)
  test_rows <- sample(
    seq_len(nrow(data)), size = floor(0.15 * nrow(data))
  )
  development <- data[-test_rows, ]
  test <- data[test_rows, ]
  set.seed(seed + 1)
  validation_rows <- sample(
    seq_len(nrow(development)),
    size = floor(0.1764706 * nrow(development))
  )
  validation <- development[validation_rows, ]
  training <- development[-validation_rows, ]

  fit <- train_network(
    training, validation, predictors, seed,
    "County", crisis, "single split"
  )
  predicted <- predict_network(fit, test)
  predictions <- test %>%
    transmute(
      geography = "County", crisis,
      fips, county_name,
      observed = recovery, predicted,
      residual = observed - predicted
    )
  importance <- permutation_importance(
    fit, test, predictors, seed + 10
  ) %>%
    mutate(geography = "County", crisis)

  model_file <- file.path(
    county_output,
    if (crisis == "Great Recession") {
      "county_2008_keras_model.keras"
    } else {
      "county_covid_keras_model.keras"
    }
  )
  save_model(fit$model, model_file, overwrite = TRUE)
  saveRDS(
    list(
      preprocessor = fit$preprocessor,
      target_mean = fit$target_mean,
      target_sd = fit$target_sd,
      predictors = predictors,
      batch_size = fit$batch_size
    ),
    sub("\\.keras$", "_preprocessing.rds", model_file)
  )
  message("Finished County ", crisis, " Keras model.")

  list(
    fit = fit, predictions = predictions, importance = importance,
    metrics = regression_metrics(predictions, "15% holdout") %>%
      mutate(
        geography = "County", crisis,
        predictors = length(predictors)
      )
  )
}

# ---- State five-fold assessment --------------------------------------------

make_folds <- function(n, k, seed) {
  set.seed(seed)
  sample(rep(seq_len(k), length.out = n))
}

fit_state_task <- function(data, crisis, seed) {
  predictors <- task_predictors(data)
  outer_folds <- make_folds(nrow(data), 5, seed)
  predictions <- list()
  histories <- list()
  importances <- list()
  fits <- list()

  for (fold in seq_len(5)) {
    message("Training State ", crisis, " fold ", fold, " of 5...")
    development <- data[outer_folds != fold, ]
    assessment <- data[outer_folds == fold, ]
    inner_validation <- make_folds(
      nrow(development), 5, seed + fold
    ) == 1
    training <- development[!inner_validation, ]
    validation <- development[inner_validation, ]

    fit <- train_network(
      training, validation, predictors, seed + fold,
      "State", crisis, fold
    )
    predicted <- predict_network(fit, assessment)
    predictions[[fold]] <- assessment %>%
      transmute(
        geography = "State", crisis, fold,
        state_fips, state_abbr, state_name,
        observed = recovery, predicted,
        residual = observed - predicted
      )
    histories[[fold]] <- fit$history
    importances[[fold]] <- permutation_importance(
      fit, assessment, predictors, seed + 100 + fold
    ) %>%
      mutate(fold = fold)
    fits[[fold]] <- fit

    label <- if (crisis == "Great Recession") "2008" else "covid"
    model_file <- file.path(
      state_output,
      paste0("state_", label, "_fold_", fold, "_keras_model.keras")
    )
    save_model(fit$model, model_file, overwrite = TRUE)
    saveRDS(
      list(
        preprocessor = fit$preprocessor,
        target_mean = fit$target_mean,
        target_sd = fit$target_sd,
        predictors = predictors,
        batch_size = fit$batch_size
      ),
      sub("\\.keras$", "_preprocessing.rds", model_file)
    )
    message("Finished State ", crisis, " fold ", fold, " of 5.")
  }

  prediction_table <- bind_rows(predictions)
  importance_table <- bind_rows(importances) %>%
    group_by(predictor) %>%
    summarise(
      importance_pct_mse = mean(importance_pct_mse),
      importance_sd = sd(importance_pct_mse),
      .groups = "drop"
    ) %>%
    mutate(geography = "State", crisis)

  list(
    fits = fits,
    predictions = prediction_table,
    importance = importance_table,
    history = bind_rows(histories),
    metrics = regression_metrics(
      prediction_table, "5-fold cross-validation"
    ) %>%
      mutate(
        geography = "State", crisis,
        predictors = length(predictors)
      )
  )
}

# ---- Run models -------------------------------------------------------------

county_models <- list(
  `Great Recession` = fit_county_task(
    county_tasks[["Great Recession"]], "Great Recession", 2008
  ),
  `COVID-19` = fit_county_task(
    county_tasks[["COVID-19"]], "COVID-19", 2020
  )
)
state_models <- list(
  `Great Recession` = fit_state_task(
    state_tasks[["Great Recession"]], "Great Recession", 12008
  ),
  `COVID-19` = fit_state_task(
    state_tasks[["COVID-19"]], "COVID-19", 12020
  )
)

county_predictions <- bind_rows(map(county_models, "predictions"))
county_importance <- bind_rows(map(county_models, "importance"))
county_history <- bind_rows(map(county_models, ~ .x$fit$history))
county_metrics <- bind_rows(map(county_models, "metrics"))

state_predictions <- bind_rows(map(state_models, "predictions"))
state_importance <- bind_rows(map(state_models, "importance"))
state_history <- bind_rows(map(state_models, "history"))
state_metrics <- bind_rows(map(state_models, "metrics"))

# ---- Graph gallery ----------------------------------------------------------

crisis_colors <- c(
  "Great Recession" = "#173F73",
  "COVID-19" = "#38BDF8"
)
theme_keras <- function(base_size = 11) {
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

make_graphs <- function(
    geography, predictions, importance, history, metrics
) {
  observed <- ggplot(
    predictions, aes(observed, predicted, color = crisis)
  ) +
    geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", color = "grey40"
    ) +
    geom_point(
      alpha = 0.55,
      size = if (geography == "State") 2.2 else 1.2
    ) +
    facet_wrap(~crisis, scales = "free") +
    scale_color_manual(values = crisis_colors) +
    labs(
      title = paste(geography, "Keras network: observed versus predicted"),
      subtitle = if (geography == "County") {
        "Predictions from a held-out 15% assessment set"
      } else {
        "Predictions from five-fold cross-validation"
      },
      x = "Observed recovery ratio",
      y = "Predicted recovery ratio",
      color = NULL
    ) +
    theme_keras() +
    theme(legend.position = "none")

  learning <- history %>%
    select(
      geography, crisis, fold, epoch,
      Training = training_loss, Validation = validation_loss
    ) %>%
    pivot_longer(
      c(Training, Validation), names_to = "series", values_to = "loss"
    ) %>%
    ggplot(aes(epoch, loss, color = series, group = interaction(fold, series))) +
    geom_line(
      alpha = if (geography == "State") 0.45 else 0.9,
      linewidth = 0.7
    ) +
    facet_wrap(~crisis, scales = "free_y") +
    scale_y_log10() +
    scale_color_manual(
      values = c(Training = "#173F73", Validation = "#F59E0B")
    ) +
    labs(
      title = paste(geography, "Keras learning curves"),
      subtitle = "Early stopping restores the epoch with minimum validation loss",
      x = "Epoch", y = "Standardized MSE (log scale)", color = NULL
    ) +
    theme_keras()

  importance_plot <- importance %>%
    mutate(
      predictor_label = predictor %>%
        str_replace_all("_", " ") %>%
        str_to_sentence()
    ) %>%
    ggplot(
      aes(
        importance_pct_mse,
        reorder(predictor_label, importance_pct_mse),
        fill = crisis
      )
    ) +
    geom_col(width = 0.72) +
    facet_wrap(~crisis, scales = "free_y") +
    scale_fill_manual(values = crisis_colors) +
    labs(
      title = paste(geography, "Keras permutation importance"),
      subtitle = "Increase in assessment MSE after shuffling each predictor",
      x = "% increase in mean squared error",
      y = NULL, fill = NULL,
      caption = "Importance measures predictive reliance, not causality."
    ) +
    theme_keras() +
    theme(legend.position = "none")

  metric_long <- metrics %>%
    select(crisis, RMSE, MAE, R_squared) %>%
    pivot_longer(-crisis, names_to = "metric", values_to = "value")
  performance <- ggplot(
    metric_long, aes(crisis, value, fill = crisis)
  ) +
    geom_col(width = 0.65) +
    geom_text(
      aes(label = number(value, accuracy = 0.001)),
      vjust = -0.3
    ) +
    facet_wrap(~metric, scales = "free_y") +
    scale_fill_manual(values = crisis_colors) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.17))) +
    labs(
      title = paste(geography, "Keras model performance"),
      subtitle = "Lower RMSE and MAE are better; higher R-squared is better",
      x = NULL, y = NULL, fill = NULL
    ) +
    theme_keras() +
    theme(legend.position = "none")

  residuals <- ggplot(
    predictions, aes(residual, fill = crisis, color = crisis)
  ) +
    geom_histogram(
      bins = 35, alpha = 0.35, position = "identity"
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~crisis, scales = "free_y") +
    scale_fill_manual(values = crisis_colors) +
    scale_color_manual(values = crisis_colors) +
    labs(
      title = paste(geography, "Keras residual distributions"),
      subtitle = "Residual = observed minus predicted recovery ratio",
      x = "Residual", y = "Count", fill = NULL, color = NULL
    ) +
    theme_keras() +
    theme(legend.position = "none")

  list(
    observed_vs_predicted = observed,
    learning_curves = learning,
    permutation_importance = importance_plot,
    model_performance = performance,
    residual_distributions = residuals
  )
}

county_graphs <- make_graphs(
  "County", county_predictions, county_importance,
  county_history, county_metrics
)
state_graphs <- make_graphs(
  "State", state_predictions, state_importance,
  state_history, state_metrics
)

save_gallery <- function(graphs, output, pdf_name) {
  file_names <- sprintf(
    "%02d_%s.png", seq_along(graphs), names(graphs)
  )
  walk2(graphs, file_names, function(graph, file_name) {
    ggsave(
      file.path(output, file_name), graph,
      width = 10.5, height = 6.5, dpi = 300, bg = "white"
    )
  })
  grDevices::pdf(
    file.path(output, pdf_name),
    width = 10.5, height = 6.5, onefile = TRUE
  )
  walk(graphs, print)
  grDevices::dev.off()
}

save_gallery(
  county_graphs, county_output, "county_keras_r_graphs.pdf"
)
save_gallery(
  state_graphs, state_output, "state_keras_r_graphs.pdf"
)

# ---- Export reusable results ------------------------------------------------

readr::write_csv(
  county_metrics,
  file.path(county_output, "county_keras_r_metrics.csv")
)
readr::write_csv(
  county_predictions,
  file.path(county_output, "county_keras_r_predictions.csv")
)
readr::write_csv(
  county_importance,
  file.path(county_output, "county_keras_r_importance.csv")
)
readr::write_csv(
  county_history,
  file.path(county_output, "county_keras_r_history.csv")
)
readr::write_csv(
  state_metrics,
  file.path(state_output, "state_keras_r_metrics.csv")
)
readr::write_csv(
  state_predictions,
  file.path(state_output, "state_keras_r_predictions.csv")
)
readr::write_csv(
  state_importance,
  file.path(state_output, "state_keras_r_importance.csv")
)
readr::write_csv(
  state_history,
  file.path(state_output, "state_keras_r_history.csv")
)

architecture <- list(
  interface = paste0(
    "R keras3 ", as.character(packageVersion("keras3"))
  ),
  backend = keras3::config_backend(),
  hidden_layers = "Adaptive 4x, 2x, and 1x encoded input width",
  batch_normalization = "After first and second dense layers",
  activation = "ReLU",
  dropout_rates = c(0.30, 0.20, 0.10),
  optimizer = "AdamW",
  initial_learning_rate = 0.002,
  weight_decay = 1e-4,
  gradient_clipnorm = 5,
  loss = "MSE on standardized recovery ratio",
  early_stopping_patience = 20,
  reduce_lr_patience = 8,
  maximum_epochs = 200,
  county_validation = "70% train / 15% validation / 15% assessment",
  state_validation = "5-fold outer CV with fold-specific validation split"
)
saveRDS(
  architecture,
  file.path(project_dir, "figures", "keras_r", "architecture.rds")
)
capture.output(
  str(architecture),
  file = file.path(project_dir, "figures", "keras_r", "architecture.txt")
)

message(
  "R/Keras neural-network analysis complete.\nCounty outputs: ",
  normalizePath(county_output, winslash = "/", mustWork = FALSE),
  "\nState outputs: ",
  normalizePath(state_output, winslash = "/", mustWork = FALSE)
)
print(county_metrics)
print(state_metrics)
