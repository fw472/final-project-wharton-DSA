# Comprehensive graph gallery for "Bouncing Back"
#
# Run from the project directory with either:
#   source("graphs.R")
#   Rscript graphs.R
#
# Outputs:
#   figures/*.png
#   figures/all_graphs.pdf
#
# The named `plots` list is intentionally left in the calling environment so
# individual figures can be viewed or reused after source("graphs.R").

required_packages <- c("tidyverse", "scales", "usmap")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    "\nInstall them with:\n  install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(usmap)
})

if (!"data_year" %in% names(formals(usmap::us_map)) ||
    !"data_year" %in% names(formals(usmap::plot_usmap))) {
  stop(
    "The installed usmap package is too old for reproducible county boundaries.",
    "\nUpdate it with:\n  install.packages(\"usmap\")",
    call. = FALSE
  )
}

# ---- Locate inputs and prepare output directory ----------------------------

command_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
command_dir <- if (length(command_file) > 0) {
  dirname(normalizePath(sub("^--file=", "", command_file[[1]]), mustWork = FALSE))
} else {
  character()
}

source_files <- unlist(
  lapply(
    sys.frames(),
    function(frame) {
      if (!is.null(frame$ofile)) as.character(frame$ofile) else NULL
    }
  ),
  use.names = FALSE
)
source_dirs <- if (length(source_files) > 0) {
  dirname(normalizePath(source_files, mustWork = FALSE))
} else {
  character()
}

candidate_dirs <- unique(c(command_dir, rev(source_dirs), getwd()))
matching_dirs <- candidate_dirs[
  vapply(
    candidate_dirs,
    function(path) {
      file.exists(file.path(path, "spine_data.csv")) &&
        file.exists(file.path(path, "state_spine.csv"))
    },
    logical(1)
  )
]

if (length(matching_dirs) == 0) {
  stop(
    "Could not find spine_data.csv and state_spine.csv. ",
    "Run this script from the project directory or keep it beside both files.",
    call. = FALSE
  )
}

project_dir <- matching_dirs[[1]]

county_path <- file.path(project_dir, "spine_data.csv")
state_path <- file.path(project_dir, "state_spine.csv")
figure_dir <- file.path(project_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Read and validate data -------------------------------------------------

county <- readr::read_csv(
  county_path,
  col_types = cols(.default = col_guess(), fips = col_character()),
  show_col_types = FALSE
)

state <- readr::read_csv(
  state_path,
  col_types = cols(.default = col_guess(), state_fips = col_character()),
  show_col_types = FALSE
)

county_required <- c(
  "fips", "county_name",
  "peak_year_2008", "peak_employment_2008", "trough_year_2008",
  "trough_employment_2008", "recovery_ratio_2008",
  "flag_tiny_base_2008", "flag_implausible_2008", "never_recovered_2008",
  "peak_year_covid", "peak_employment_covid", "trough_year_covid",
  "trough_employment_covid", "recovery_ratio_covid",
  "flag_tiny_base_covid", "flag_implausible_covid", "never_recovered_covid",
  "poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
  "rucc_2003_2007", "population_density_2007", "proprietor_share_2007",
  "net_migration_rate_2007", "pct_hs_plus_2007",
  "pct_bachelors_plus_2007", "birth_rate_2007", "wealth_band_2008",
  "poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
  "rucc_2013_2019", "population_density_2019", "proprietor_share_2019",
  "net_migration_rate_2019", "pct_hs_plus_2019",
  "pct_bachelors_plus_2019", "birth_rate_2019", "wealth_band_covid"
)

state_required <- c(
  "state_fips", "state_abbr", "state_name",
  "peak_year_2008", "peak_employment_2008", "trough_year_2008",
  "trough_employment_2008", "recovery_ratio_2008",
  "flag_implausible_2008", "never_recovered_2008",
  "peak_year_covid", "peak_employment_covid", "trough_year_covid",
  "trough_employment_covid", "recovery_ratio_covid",
  "flag_implausible_covid", "never_recovered_covid",
  "poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
  "pct_rural_2007", "population_density_2007", "proprietor_share_2007",
  "net_migration_rate_2007", "pct_hs_plus_2007",
  "pct_bachelors_plus_2007", "birth_rate_2007",
  "poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
  "pct_rural_2019", "population_density_2019", "proprietor_share_2019",
  "net_migration_rate_2019", "pct_hs_plus_2019",
  "pct_bachelors_plus_2019", "birth_rate_2019"
)

validate_columns <- function(data, required, data_name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      data_name, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

validate_columns(county, county_required, "spine_data.csv")
validate_columns(state, state_required, "state_spine.csv")

county <- county %>%
  mutate(fips = str_pad(str_extract(as.character(fips), "\\d+"), 5, pad = "0"))

state <- state %>%
  mutate(state_fips = str_pad(
    str_extract(as.character(state_fips), "\\d+"), 2, pad = "0"
  ))

if (anyNA(county$fips) || any(nchar(county$fips) != 5) ||
    anyDuplicated(county$fips)) {
  stop("County FIPS must be unique, nonmissing five-character codes.", call. = FALSE)
}

if (anyNA(state$state_fips) || any(nchar(state$state_fips) != 2) ||
    anyDuplicated(state$state_fips)) {
  stop("State FIPS must be unique, nonmissing two-character codes.", call. = FALSE)
}

# ---- Align the two crisis periods ------------------------------------------

rucc_labels <- c(
  "1" = "Metro: 1M+",
  "2" = "Metro: 250K–1M",
  "3" = "Metro: <250K",
  "4" = "Nonmetro: 20K+, adjacent",
  "5" = "Nonmetro: 20K+, remote",
  "6" = "Nonmetro: 2.5K–20K, adjacent",
  "7" = "Nonmetro: 2.5K–20K, remote",
  "8" = "Rural: adjacent",
  "9" = "Rural: remote"
)

county_long <- bind_rows(
  county %>% transmute(
    fips, county_name, crisis = "Great Recession",
    peak_year = peak_year_2008, peak_employment = peak_employment_2008,
    trough_year = trough_year_2008, trough_employment = trough_employment_2008,
    recovery_ratio = recovery_ratio_2008,
    flag_tiny_base = flag_tiny_base_2008,
    flag_implausible = flag_implausible_2008,
    never_recovered = never_recovered_2008,
    poverty_rate = poverty_rate_2007,
    median_hh_income = median_hh_income_2007,
    total_population = total_population_2007,
    rural_urban_code = rucc_2003_2007,
    population_density = population_density_2007,
    proprietor_share = proprietor_share_2007,
    net_migration_rate = net_migration_rate_2007,
    pct_hs_plus = pct_hs_plus_2007,
    pct_bachelors_plus = pct_bachelors_plus_2007,
    birth_rate = birth_rate_2007,
    wealth_band = wealth_band_2008
  ),
  county %>% transmute(
    fips, county_name, crisis = "COVID-19",
    peak_year = peak_year_covid, peak_employment = peak_employment_covid,
    trough_year = trough_year_covid, trough_employment = trough_employment_covid,
    recovery_ratio = recovery_ratio_covid,
    flag_tiny_base = flag_tiny_base_covid,
    flag_implausible = flag_implausible_covid,
    never_recovered = never_recovered_covid,
    poverty_rate = poverty_rate_2019,
    median_hh_income = median_hh_income_2019,
    total_population = total_population_2019,
    rural_urban_code = rucc_2013_2019,
    population_density = population_density_2019,
    proprietor_share = proprietor_share_2019,
    net_migration_rate = net_migration_rate_2019,
    pct_hs_plus = pct_hs_plus_2019,
    pct_bachelors_plus = pct_bachelors_plus_2019,
    birth_rate = birth_rate_2019,
    wealth_band = wealth_band_covid
  )
) %>%
  mutate(
    crisis = factor(crisis, levels = c("Great Recession", "COVID-19")),
    wealth_band = factor(
      str_to_lower(wealth_band),
      levels = c("lower", "middle", "higher"),
      labels = c("Lower income", "Middle income", "Higher income")
    ),
    rural_urban_code = factor(
      as.character(rural_urban_code),
      levels = names(rucc_labels),
      labels = unname(rucc_labels)
    ),
    peak_to_trough_loss = 1 - trough_employment / peak_employment
  )

state_long <- bind_rows(
  state %>% transmute(
    fips = state_fips, state_abbr, state_name, crisis = "Great Recession",
    peak_year = peak_year_2008, peak_employment = peak_employment_2008,
    trough_year = trough_year_2008, trough_employment = trough_employment_2008,
    recovery_ratio = recovery_ratio_2008,
    flag_implausible = flag_implausible_2008,
    never_recovered = never_recovered_2008,
    poverty_rate = poverty_rate_2007,
    median_hh_income = median_hh_income_2007,
    total_population = total_population_2007,
    pct_rural = pct_rural_2007,
    population_density = population_density_2007,
    proprietor_share = proprietor_share_2007,
    net_migration_rate = net_migration_rate_2007,
    pct_hs_plus = pct_hs_plus_2007,
    pct_bachelors_plus = pct_bachelors_plus_2007,
    birth_rate = birth_rate_2007
  ),
  state %>% transmute(
    fips = state_fips, state_abbr, state_name, crisis = "COVID-19",
    peak_year = peak_year_covid, peak_employment = peak_employment_covid,
    trough_year = trough_year_covid, trough_employment = trough_employment_covid,
    recovery_ratio = recovery_ratio_covid,
    flag_implausible = flag_implausible_covid,
    never_recovered = never_recovered_covid,
    poverty_rate = poverty_rate_2019,
    median_hh_income = median_hh_income_2019,
    total_population = total_population_2019,
    pct_rural = pct_rural_2019,
    population_density = population_density_2019,
    proprietor_share = proprietor_share_2019,
    net_migration_rate = net_migration_rate_2019,
    pct_hs_plus = pct_hs_plus_2019,
    pct_bachelors_plus = pct_bachelors_plus_2019,
    birth_rate = birth_rate_2019
  )
) %>%
  mutate(
    crisis = factor(crisis, levels = c("Great Recession", "COVID-19")),
    peak_to_trough_loss = 1 - trough_employment / peak_employment
  )

valid_county <- county_long %>%
  filter(
    !is.na(recovery_ratio),
    is.na(flag_implausible) | !flag_implausible
  )

valid_state <- state_long %>%
  filter(
    !is.na(recovery_ratio),
    is.na(flag_implausible) | !flag_implausible
  )

message("Loaded ", nrow(county), " counties and ", nrow(state), " states/DC.")
message(
  "Recovery plots exclude ",
  nrow(county_long) - nrow(valid_county),
  " of ", nrow(county_long),
  " county-period rows with missing or implausible recovery ratios."
)

# ---- Shared style -----------------------------------------------------------

crisis_colors <- c("Great Recession" = "#355C7D", "COVID-19" = "#C44E52")

theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = rel(1.25)),
      plot.subtitle = element_text(color = "grey30"),
      plot.caption = element_text(color = "grey40", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

recovery_reference <- geom_vline(
  xintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6
)

recovery_h_reference <- geom_hline(
  yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6
)

plots <- list()
add_plot <- function(name, plot) {
  plots[[name]] <<- plot
}

# ---- Outcome distributions and comparisons ---------------------------------

add_plot(
  "01_recovery_histograms",
  ggplot(valid_county, aes(recovery_ratio, fill = crisis)) +
    geom_histogram(bins = 45, color = "white", linewidth = 0.15) +
    recovery_reference +
    facet_wrap(~crisis, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = crisis_colors, guide = "none") +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "County recovery ratios after two economic shocks",
      subtitle = "The dashed line marks a return to pre-crisis peak employment",
      x = "Recovery employment as a share of peak employment", y = "Counties",
      caption = "Source: project spine_data.csv; missing and implausible ratios excluded."
    ) +
    theme_report()
)

add_plot(
  "02_recovery_density",
  ggplot(valid_county, aes(recovery_ratio, color = crisis, fill = crisis)) +
    geom_density(alpha = 0.18, linewidth = 1, adjust = 1.1) +
    recovery_reference +
    scale_color_manual(values = crisis_colors) +
    scale_fill_manual(values = crisis_colors) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Recovery distributions shifted between crises",
      x = "Recovery employment as a share of peak employment", y = "Density",
      color = "Crisis", fill = "Crisis",
      caption = "Density curves are normalized within each crisis."
    ) +
    theme_report()
)

add_plot(
  "03_recovery_boxplots",
  ggplot(valid_county, aes(crisis, recovery_ratio, fill = crisis)) +
    geom_hline(
      yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6
    ) +
    geom_boxplot(width = 0.58, outlier.alpha = 0.25, outlier.size = 1) +
    scale_fill_manual(values = crisis_colors, guide = "none") +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "County recovery ratios by crisis",
      x = NULL, y = "Recovery employment as a share of peak employment",
      caption = "Boxes show the median and interquartile range."
    ) +
    theme_report()
)

add_plot(
  "04_recovery_ecdf",
  ggplot(valid_county, aes(recovery_ratio, color = crisis)) +
    stat_ecdf(linewidth = 1) +
    recovery_reference +
    scale_color_manual(values = crisis_colors) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Cumulative distribution of county recovery",
      x = "Recovery employment as a share of peak employment",
      y = "Share of counties at or below ratio", color = "Crisis"
    ) +
    theme_report()
)

paired_county <- county %>%
  filter(
    !is.na(recovery_ratio_2008), !is.na(recovery_ratio_covid),
    is.na(flag_implausible_2008) | !flag_implausible_2008,
    is.na(flag_implausible_covid) | !flag_implausible_covid
  ) %>%
  mutate(
    tiny_base = if_else(
      coalesce(flag_tiny_base_2008, FALSE) |
        coalesce(flag_tiny_base_covid, FALSE),
      "Peak employment below 1,000", "Peak employment at least 1,000"
    )
  )

paired_limits <- range(
  c(paired_county$recovery_ratio_2008, paired_county$recovery_ratio_covid),
  finite = TRUE
)

add_plot(
  "05_county_crisis_comparison",
  ggplot(
    paired_county,
    aes(recovery_ratio_2008, recovery_ratio_covid, color = tiny_base)
  ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey35") +
    geom_point(alpha = 0.5, size = 1.5) +
    coord_equal(xlim = paired_limits, ylim = paired_limits) +
    scale_color_manual(values = c(
      "Peak employment at least 1,000" = "#355C7D",
      "Peak employment below 1,000" = "#D98C3F"
    )) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Did the same counties recover after both crises?",
      subtitle = "Points above the diagonal recovered more strongly after COVID-19",
      x = "Great Recession recovery ratio", y = "COVID-19 recovery ratio",
      color = "County size diagnostic",
      caption = paste(comma(nrow(paired_county)), "counties with both outcomes.")
    ) +
    theme_report()
)

add_plot(
  "06_peak_to_trough_loss",
  ggplot(
    valid_county %>% filter(is.finite(peak_to_trough_loss)),
    aes(peak_to_trough_loss, fill = crisis, color = crisis)
  ) +
    geom_density(alpha = 0.18, linewidth = 0.9) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey35") +
    scale_fill_manual(values = crisis_colors) +
    scale_color_manual(values = crisis_colors) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Employment losses from peak to trough",
      x = "Peak-to-trough employment loss", y = "Density",
      fill = "Crisis", color = "Crisis"
    ) +
    theme_report()
)

trough_counts <- county_long %>%
  filter(!is.na(trough_year)) %>%
  count(crisis, trough_year)

add_plot(
  "07_trough_years",
  ggplot(trough_counts, aes(factor(trough_year), n, fill = crisis)) +
    geom_col() +
    facet_wrap(~crisis, scales = "free_x") +
    scale_fill_manual(values = crisis_colors, guide = "none") +
    labs(
      title = "When county employment reached its post-peak trough",
      x = "Trough year", y = "Counties"
    ) +
    theme_report()
)

never_summary <- bind_rows(
  county_long %>%
    filter(!is.na(never_recovered)) %>%
    group_by(crisis) %>%
    summarise(share = mean(never_recovered), .groups = "drop") %>%
    mutate(level = "County"),
  state_long %>%
    filter(!is.na(never_recovered)) %>%
    group_by(crisis) %>%
    summarise(share = mean(never_recovered), .groups = "drop") %>%
    mutate(level = "State/DC")
)

add_plot(
  "08_never_recovered_share",
  ggplot(never_summary, aes(level, share, fill = crisis)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.65) +
    geom_text(
      aes(label = label_percent(accuracy = 0.1)(share)),
      position = position_dodge(width = 0.72), vjust = -0.35, size = 3.5
    ) +
    scale_fill_manual(values = crisis_colors) +
    scale_y_continuous(labels = label_percent(), expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Long-run non-recovery remained common",
      subtitle = "Never recovered means employment never exceeded its pre-crisis peak through 2025",
      x = NULL, y = "Share never recovered", fill = "Crisis"
    ) +
    theme_report()
)

# ---- Group comparisons ------------------------------------------------------

add_plot(
  "09_recovery_by_wealth_band",
  ggplot(
    valid_county %>% filter(!is.na(wealth_band)),
    aes(wealth_band, recovery_ratio, fill = crisis)
  ) +
    geom_hline(
      yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6
    ) +
    geom_violin(
      position = position_dodge(width = 0.8), alpha = 0.55,
      trim = TRUE, scale = "width"
    ) +
    geom_boxplot(
      position = position_dodge(width = 0.8), width = 0.16,
      outlier.shape = NA, alpha = 0.8
    ) +
    scale_fill_manual(values = crisis_colors) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Recovery by county income tercile",
      x = "Baseline household-income band",
      y = "Recovery employment as a share of peak employment",
      fill = "Crisis"
    ) +
    theme_report() +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))
)

add_plot(
  "10_recovery_by_rural_urban_code",
  ggplot(
    valid_county %>% filter(!is.na(rural_urban_code)),
    aes(rural_urban_code, recovery_ratio, fill = crisis)
  ) +
    geom_hline(
      yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6
    ) +
    geom_boxplot(
      position = position_dodge(width = 0.75), width = 0.65,
      outlier.alpha = 0.12, outlier.size = 0.6
    ) +
    scale_fill_manual(values = crisis_colors) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Recovery differed across the rural–urban continuum",
      x = "USDA rural–urban continuum category",
      y = "Recovery employment as a share of peak employment",
      fill = "Crisis"
    ) +
    theme_report() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 8))
)

# ---- Predictor relationships ------------------------------------------------

make_predictor_plot <- function(
    data, variable, title, x_label, x_labels = waiver(), log_x = FALSE) {
  plot_data <- data %>%
    filter(!is.na(.data[[variable]]), is.finite(.data[[variable]]))

  if (log_x) {
    removed_nonpositive <- sum(plot_data[[variable]] <= 0)
    if (removed_nonpositive > 0) {
      message(
        title, ": excluded ", removed_nonpositive,
        " nonpositive value(s) from the logarithmic x-axis."
      )
    }
    plot_data <- plot_data %>% filter(.data[[variable]] > 0)
  }

  p <- ggplot(
    plot_data,
    aes(x = .data[[variable]], y = recovery_ratio, color = crisis)
  ) +
    geom_hline(
      yintercept = 1, linetype = "dashed", color = "grey35", linewidth = 0.5
    ) +
    geom_point(alpha = 0.28, size = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.9) +
    facet_wrap(~crisis) +
    scale_color_manual(values = crisis_colors, guide = "none") +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = title, x = x_label,
      y = "Recovery employment as a share of peak employment",
      caption = paste(
        comma(nrow(plot_data)),
        "county-period observations; line is an OLS trend with 95% confidence interval."
      )
    ) +
    theme_report()

  if (log_x) {
    p <- p + scale_x_log10(labels = x_labels)
  } else {
    p <- p + scale_x_continuous(labels = x_labels)
  }
  p
}

add_plot(
  "11_predictor_poverty",
  make_predictor_plot(
    valid_county, "poverty_rate",
    "Recovery and baseline poverty", "Population below poverty line",
    label_percent(scale = 1, accuracy = 1)
  )
)

add_plot(
  "12_predictor_income",
  make_predictor_plot(
    valid_county, "median_hh_income",
    "Recovery and baseline household income",
    "Median household income (log scale)", label_dollar(accuracy = 1000), TRUE
  )
)

add_plot(
  "13_predictor_population",
  make_predictor_plot(
    valid_county, "total_population",
    "Recovery and county population",
    "Total population (log scale)", label_number(scale_cut = cut_short_scale()), TRUE
  )
)

add_plot(
  "14_predictor_density",
  make_predictor_plot(
    valid_county, "population_density",
    "Recovery and population density",
    "Residents per square mile (log scale)", label_number(), TRUE
  )
)

add_plot(
  "15_predictor_proprietor_share",
  make_predictor_plot(
    valid_county, "proprietor_share",
    "Recovery and proprietor employment",
    "Proprietors as a share of employment", label_percent(accuracy = 1)
  )
)

add_plot(
  "16_predictor_migration",
  make_predictor_plot(
    valid_county, "net_migration_rate",
    "Recovery and baseline net migration",
    "Net migration per 1,000 residents", label_number(accuracy = 1)
  )
)

add_plot(
  "17_predictor_education",
  make_predictor_plot(
    valid_county, "pct_bachelors_plus",
    "Recovery and college attainment",
    "Adults with a bachelor's degree or higher",
    label_percent(scale = 1, accuracy = 1)
  )
)

add_plot(
  "18_predictor_birth_rate",
  make_predictor_plot(
    valid_county, "birth_rate",
    "Recovery and baseline birth rate",
    "Births per 1,000 residents", label_number(accuracy = 1)
  )
)

# ---- Correlation heatmap ----------------------------------------------------

correlation_data <- valid_county %>%
  transmute(
    crisis,
    Recovery = recovery_ratio,
    Poverty = poverty_rate,
    `Log income` = if_else(median_hh_income > 0, log10(median_hh_income), NA_real_),
    `Log population` = if_else(total_population > 0, log10(total_population), NA_real_),
    `Log density` = if_else(population_density > 0, log10(population_density), NA_real_),
    Proprietors = proprietor_share,
    Migration = net_migration_rate,
    `High school+` = pct_hs_plus,
    `Bachelor's+` = pct_bachelors_plus,
    `Birth rate` = birth_rate
  )

correlation_long <- correlation_data %>%
  group_split(crisis) %>%
  map_dfr(function(group) {
    crisis_name <- as.character(group$crisis[[1]])
    matrix_data <- group %>% select(-crisis)
    correlation <- cor(matrix_data, use = "pairwise.complete.obs")
    as.data.frame(as.table(correlation), stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      transmute(
        crisis = crisis_name,
        predictor_x = Var1,
        predictor_y = Var2,
        correlation = Freq
      )
  }) %>%
  mutate(crisis = factor(crisis, levels = levels(county_long$crisis)))

add_plot(
  "19_predictor_correlations",
  ggplot(
    correlation_long,
    aes(predictor_x, predictor_y, fill = correlation)
  ) +
    geom_tile(color = "white", linewidth = 0.25) +
    geom_text(
      aes(label = number(correlation, accuracy = 0.01)),
      size = 2.35, color = "grey15"
    ) +
    facet_wrap(~crisis) +
    scale_fill_gradient2(
      low = "#3B6FB6", mid = "white", high = "#B8423F",
      midpoint = 0, limits = c(-1, 1), name = "Pearson\ncorrelation"
    ) +
    coord_equal() +
    labs(
      title = "County predictor correlation matrices",
      subtitle = "Income, population, and density are log-transformed",
      x = NULL, y = NULL,
      caption = "Correlations use all available pairs of observations."
    ) +
    theme_report(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
)

# ---- Highest- and lowest-recovery rankings ---------------------------------

make_ranking_plot <- function(data, crisis_name, entity, level_label) {
  ranked <- data %>%
    filter(crisis == crisis_name) %>%
    arrange(recovery_ratio)

  extremes <- bind_rows(
    ranked %>% slice_head(n = 10) %>% mutate(group = "Lowest recovery"),
    ranked %>% slice_tail(n = 10) %>% mutate(group = "Highest recovery")
  ) %>%
    mutate(
      display_name = .data[[entity]],
      display_name = factor(
        display_name,
        levels = unique(display_name[order(recovery_ratio)])
      )
    )

  ggplot(extremes, aes(recovery_ratio, display_name, color = group)) +
    geom_vline(
      xintercept = 1, linetype = "dashed", color = "grey35", linewidth = 0.5
    ) +
    geom_segment(
      aes(x = 0, xend = recovery_ratio, yend = display_name),
      linewidth = 0.65, alpha = 0.75
    ) +
    geom_point(size = 2.5) +
    facet_grid(group ~ ., scales = "free_y", space = "free_y") +
    scale_color_manual(values = c(
      "Lowest recovery" = "#C44E52", "Highest recovery" = "#2A7F62"
    ), guide = "none") +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = paste(level_label, "recovery extremes:", crisis_name),
      x = "Recovery employment as a share of peak employment", y = NULL,
      caption = "Missing and implausible recovery ratios excluded."
    ) +
    theme_report()
}

add_plot(
  "20_county_rankings_2008",
  make_ranking_plot(
    valid_county, "Great Recession", "county_name", "County"
  )
)
add_plot(
  "21_county_rankings_covid",
  make_ranking_plot(valid_county, "COVID-19", "county_name", "County")
)
add_plot(
  "22_state_rankings_2008",
  make_ranking_plot(valid_state, "Great Recession", "state_name", "State")
)
add_plot(
  "23_state_rankings_covid",
  make_ranking_plot(valid_state, "COVID-19", "state_name", "State")
)

# ---- County and state maps --------------------------------------------------

# The 2021 county vintage retains Connecticut's historical counties, matching
# the FIPS codes used in this project's county data.
county_map_fips <- usmap::us_map(regions = "counties", data_year = 2021) %>%
  distinct(fips) %>%
  pull(fips) %>%
  as.character() %>%
  str_pad(5, pad = "0")
state_map_fips <- usmap::us_map(regions = "states") %>%
  distinct(fips) %>%
  pull(fips) %>%
  as.character() %>%
  str_pad(2, pad = "0")

unmatched_counties <- county %>%
  filter(!fips %in% county_map_fips) %>%
  distinct(fips, county_name)
unmatched_states <- state %>%
  filter(!state_fips %in% state_map_fips) %>%
  distinct(state_fips, state_name)

if (nrow(unmatched_counties) > 0) {
  message(
    "County maps omit ", nrow(unmatched_counties),
    " record(s) not represented by usmap's 2021 geometry: ",
    paste(unmatched_counties$county_name, collapse = "; ")
  )
} else {
  message("All county FIPS matched usmap's 2021 county geometry.")
}

if (nrow(unmatched_states) > 0) {
  message(
    "State maps omit ", nrow(unmatched_states),
    " record(s) not represented by usmap geometry: ",
    paste(unmatched_states$state_name, collapse = "; ")
  )
} else {
  message("All state/DC FIPS matched usmap geometry.")
}

make_recovery_map <- function(data, crisis_name, regions, level_label) {
  allowed_fips <- if (regions == "counties") {
    county_map_fips
  } else {
    state_map_fips
  }

  map_data <- data %>%
    filter(crisis == crisis_name, fips %in% allowed_fips) %>%
    transmute(fips, value = recovery_ratio)

  map_args <- list(
    regions = regions,
    data = map_data,
    values = "value"
  )
  if (regions == "counties") {
    map_args$data_year <- 2021
  }

  do.call(usmap::plot_usmap, map_args) +
    scale_fill_viridis_c(
      option = "C", labels = label_percent(accuracy = 1),
      na.value = "grey88", name = "Recovery\nratio"
    ) +
    labs(
      title = paste(level_label, "recovery:", crisis_name),
      subtitle = "Recovery employment divided by pre-crisis peak employment",
      caption = "Grey areas have no matched, valid recovery ratio."
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey30"),
      plot.caption = element_text(color = "grey40", hjust = 0),
      legend.position = "right"
    )
}

make_never_map <- function(data, crisis_name, regions, level_label) {
  allowed_fips <- if (regions == "counties") {
    county_map_fips
  } else {
    state_map_fips
  }

  map_data <- data %>%
    filter(crisis == crisis_name, fips %in% allowed_fips) %>%
    transmute(
      fips,
      value = factor(
        case_when(
          never_recovered ~ "Never recovered",
          !never_recovered ~ "Recovered at some point",
          TRUE ~ NA_character_
        ),
        levels = c("Recovered at some point", "Never recovered")
      )
    )

  map_args <- list(
    regions = regions,
    data = map_data,
    values = "value"
  )
  if (regions == "counties") {
    map_args$data_year <- 2021
  }

  do.call(usmap::plot_usmap, map_args) +
    scale_fill_manual(
      values = c(
        "Recovered at some point" = "#B9D8C2",
        "Never recovered" = "#B8423F"
      ),
      na.value = "grey88", name = NULL
    ) +
    labs(
      title = paste(level_label, "long-run recovery status:", crisis_name),
      subtitle = "Whether employment ever exceeded its pre-crisis peak through 2025",
      caption = "Grey areas have no matched recovery-status observation."
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey30"),
      plot.caption = element_text(color = "grey40", hjust = 0),
      legend.position = "bottom"
    )
}

add_plot(
  "24_county_recovery_map_2008",
  make_recovery_map(valid_county, "Great Recession", "counties", "County")
)
add_plot(
  "25_county_recovery_map_covid",
  make_recovery_map(valid_county, "COVID-19", "counties", "County")
)
add_plot(
  "26_state_recovery_map_2008",
  make_recovery_map(valid_state, "Great Recession", "states", "State")
)
add_plot(
  "27_state_recovery_map_covid",
  make_recovery_map(valid_state, "COVID-19", "states", "State")
)
add_plot(
  "28_county_never_recovered_map_2008",
  make_never_map(county_long, "Great Recession", "counties", "County")
)
add_plot(
  "29_county_never_recovered_map_covid",
  make_never_map(county_long, "COVID-19", "counties", "County")
)
add_plot(
  "30_state_never_recovered_map_2008",
  make_never_map(state_long, "Great Recession", "states", "State")
)
add_plot(
  "31_state_never_recovered_map_covid",
  make_never_map(state_long, "COVID-19", "states", "State")
)

# ---- Advanced processed-data views -----------------------------------------

processed_county <- county %>%
  filter(
    !is.na(recovery_ratio_2008), !is.na(recovery_ratio_covid),
    is.na(flag_implausible_2008) | !flag_implausible_2008,
    is.na(flag_implausible_covid) | !flag_implausible_covid
  ) %>%
  transmute(
    fips,
    county_name,
    recovery_ratio_2008,
    recovery_ratio_covid,
    recovery_gap = recovery_ratio_covid - recovery_ratio_2008,
    peak_to_trough_loss_2008 =
      1 - trough_employment_2008 / peak_employment_2008,
    peak_to_trough_loss_covid =
      1 - trough_employment_covid / peak_employment_covid,
    poverty_change_pp = poverty_rate_2019 - poverty_rate_2007,
    income_change_pct =
      median_hh_income_2019 / median_hh_income_2007 - 1,
    population_change_pct =
      total_population_2019 / total_population_2007 - 1,
    density_change_pct =
      population_density_2019 / population_density_2007 - 1,
    proprietor_share_change_pp =
      100 * (proprietor_share_2019 - proprietor_share_2007),
    migration_rate_change =
      net_migration_rate_2019 - net_migration_rate_2007,
    high_school_change_pp = pct_hs_plus_2019 - pct_hs_plus_2007,
    bachelors_change_pp =
      pct_bachelors_plus_2019 - pct_bachelors_plus_2007,
    birth_rate_change = birth_rate_2019 - birth_rate_2007,
    recovery_quartile_2008 = ntile(recovery_ratio_2008, 4),
    recovery_quartile_covid = ntile(recovery_ratio_covid, 4),
    never_recovered_2008,
    never_recovered_covid
  ) %>%
  mutate(
    across(
      c(
        recovery_gap, starts_with("peak_to_trough_loss_"),
        ends_with("_change_pp"), ends_with("_change_pct"),
        migration_rate_change, birth_rate_change
      ),
      ~ if_else(is.finite(.x), .x, NA_real_)
    ),
    recovery_quartile_2008 = factor(
      recovery_quartile_2008, levels = 1:4,
      labels = c("Q1: Lowest", "Q2", "Q3", "Q4: Highest")
    ),
    recovery_quartile_covid = factor(
      recovery_quartile_covid, levels = 1:4,
      labels = c("Q1: Lowest", "Q2", "Q3", "Q4: Highest")
    )
  )

processed_path <- file.path(figure_dir, "processed_county_metrics.csv")
readr::write_csv(processed_county, processed_path, na = "")
message("Wrote derived county metrics to ", processed_path, ".")

shock_medians <- valid_county %>%
  filter(is.finite(peak_to_trough_loss)) %>%
  group_by(crisis) %>%
  summarise(median_loss = median(peak_to_trough_loss), .groups = "drop")

add_plot(
  "32_resilience_quadrants",
  ggplot(
    valid_county %>%
      filter(
        is.finite(peak_to_trough_loss),
        !is.na(total_population), total_population > 0,
        !is.na(never_recovered)
      ),
    aes(
      peak_to_trough_loss, recovery_ratio,
      size = total_population,
      color = factor(
        never_recovered,
        levels = c(FALSE, TRUE),
        labels = c("Eventually recovered", "Never recovered")
      )
    )
  ) +
    geom_hline(
      yintercept = 1, linetype = "dashed", color = "grey30", linewidth = 0.6
    ) +
    geom_vline(
      data = shock_medians,
      aes(xintercept = median_loss),
      linetype = "dotted", color = "grey30", linewidth = 0.6,
      inherit.aes = FALSE
    ) +
    geom_point(alpha = 0.42) +
    facet_wrap(~crisis, scales = "free_x") +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    scale_size_area(
      max_size = 9,
      labels = label_number(scale_cut = cut_short_scale()),
      name = "Baseline\npopulation"
    ) +
    scale_color_manual(
      values = c(
        "Eventually recovered" = "#2A7F62",
        "Never recovered" = "#B8423F"
      ),
      name = "Long-run status"
    ) +
    labs(
      title = "Shock severity and economic resilience",
      subtitle = "Dotted lines mark each crisis's median peak-to-trough loss",
      x = "Peak-to-trough employment loss",
      y = "Recovery employment as a share of peak employment",
      caption = "Bubble area represents baseline county population."
    ) +
    theme_report()
)

transition_table <- processed_county %>%
  count(recovery_quartile_2008, recovery_quartile_covid, name = "count") %>%
  group_by(recovery_quartile_2008) %>%
  mutate(row_share = count / sum(count)) %>%
  ungroup()

add_plot(
  "33_recovery_quartile_transitions",
  ggplot(
    transition_table,
    aes(recovery_quartile_2008, recovery_quartile_covid, fill = row_share)
  ) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(
      aes(
        label = paste0(
          comma(count), "\n", label_percent(accuracy = 0.1)(row_share)
        )
      ),
      size = 3.4, lineheight = 0.9
    ) +
    scale_fill_viridis_c(
      option = "C", labels = label_percent(accuracy = 1),
      name = "Within-row\nshare"
    ) +
    coord_equal() +
    labs(
      title = "How counties moved between recovery quartiles",
      subtitle = "Percentages sum to 100% within each Great Recession quartile",
      x = "Great Recession recovery quartile",
      y = "COVID-19 recovery quartile",
      caption = paste(comma(nrow(processed_county)), "counties with both outcomes.")
    ) +
    theme_report()
)

add_plot(
  "34_recovery_gap_distribution",
  ggplot(processed_county, aes(recovery_gap)) +
    geom_histogram(
      bins = 50, fill = "#6C5B7B", color = "white", linewidth = 0.2
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.7) +
    geom_vline(
      xintercept = median(processed_county$recovery_gap, na.rm = TRUE),
      color = "#C44E52", linewidth = 0.8
    ) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Change in county recovery performance between crises",
      subtitle = "Positive values indicate stronger recovery after COVID-19",
      x = "COVID-19 recovery ratio minus Great Recession recovery ratio",
      y = "Counties",
      caption = "The dashed line is no change; the red line is the county median."
    ) +
    theme_report()
)

change_labels <- c(
  poverty_change_pp = "Poverty rate change (percentage points)",
  income_change_pct = "Household income change",
  population_change_pct = "Population change",
  density_change_pct = "Population density change",
  proprietor_share_change_pp = "Proprietor share change (percentage points)",
  migration_rate_change = "Net migration rate change",
  high_school_change_pp = "High school attainment change (percentage points)",
  bachelors_change_pp = "Bachelor's attainment change (percentage points)",
  birth_rate_change = "Birth rate change"
)

change_long <- processed_county %>%
  select(fips, recovery_gap, all_of(names(change_labels))) %>%
  pivot_longer(
    cols = all_of(names(change_labels)),
    names_to = "metric", values_to = "change"
  ) %>%
  filter(!is.na(change), is.finite(change)) %>%
  group_by(metric) %>%
  mutate(change_z = as.numeric(scale(change))) %>%
  ungroup() %>%
  mutate(metric = factor(metric, levels = names(change_labels), labels = change_labels))

add_plot(
  "35_structural_change_and_recovery_gap",
  ggplot(change_long, aes(change_z, recovery_gap)) +
    geom_hline(
      yintercept = 0, linetype = "dashed", color = "grey35", linewidth = 0.5
    ) +
    geom_point(alpha = 0.18, size = 0.75, color = "#355C7D") +
    geom_smooth(
      method = "lm", formula = y ~ x, se = TRUE,
      color = "#C44E52", linewidth = 0.8
    ) +
    facet_wrap(~metric, scales = "free_y", ncol = 3) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Did long-run county changes predict improved recovery?",
      subtitle = "Each x-axis is standardized within its metric",
      x = "Change from 2007 to 2019 (standard deviations)",
      y = "COVID-19 recovery ratio minus Great Recession ratio",
      caption = "Lines are separate bivariate OLS trends with 95% confidence intervals."
    ) +
    theme_report(base_size = 9)
)

profile_variables <- c(
  "poverty_rate", "median_hh_income", "total_population",
  "population_density", "proprietor_share", "net_migration_rate",
  "pct_hs_plus", "pct_bachelors_plus", "birth_rate"
)

profile_labels <- c(
  poverty_rate = "Poverty",
  median_hh_income = "Income",
  total_population = "Population",
  population_density = "Density",
  proprietor_share = "Proprietor share",
  net_migration_rate = "Migration",
  pct_hs_plus = "High school+",
  pct_bachelors_plus = "Bachelor's+",
  birth_rate = "Birth rate"
)

quartile_profiles <- valid_county %>%
  group_by(crisis) %>%
  mutate(
    recovery_quartile = ntile(recovery_ratio, 4),
    across(
      all_of(profile_variables),
      ~ as.numeric(scale(.x)),
      .names = "z_{.col}"
    )
  ) %>%
  group_by(crisis, recovery_quartile) %>%
  summarise(
    across(starts_with("z_"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    starts_with("z_"),
    names_to = "predictor", values_to = "mean_z"
  ) %>%
  mutate(
    predictor = str_remove(predictor, "^z_"),
    predictor = factor(
      predictor,
      levels = names(profile_labels),
      labels = profile_labels
    ),
    recovery_quartile = factor(
      recovery_quartile, levels = 1:4,
      labels = c("Q1: Lowest", "Q2", "Q3", "Q4: Highest")
    )
  )

add_plot(
  "36_standardized_quartile_profiles",
  ggplot(
    quartile_profiles,
    aes(predictor, recovery_quartile, fill = mean_z)
  ) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = number(mean_z, accuracy = 0.01)), size = 2.8) +
    facet_wrap(~crisis, ncol = 1) +
    scale_fill_gradient2(
      low = "#3B6FB6", mid = "white", high = "#B8423F",
      midpoint = 0, name = "Mean\nz-score"
    ) +
    labs(
      title = "Economic profiles of low- and high-recovery counties",
      subtitle = "Each predictor is standardized within crisis before averaging",
      x = NULL, y = "Recovery quartile"
    ) +
    theme_report(base_size = 9) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
)

binned_variables <- c(
  "poverty_rate", "median_hh_income", "population_density",
  "proprietor_share", "net_migration_rate",
  "pct_bachelors_plus", "birth_rate"
)

binned_labels <- c(
  poverty_rate = "Poverty",
  median_hh_income = "Household income",
  population_density = "Population density",
  proprietor_share = "Proprietor share",
  net_migration_rate = "Net migration",
  pct_bachelors_plus = "Bachelor's attainment",
  birth_rate = "Birth rate"
)

binned_relationships <- valid_county %>%
  select(crisis, recovery_ratio, all_of(binned_variables)) %>%
  pivot_longer(
    all_of(binned_variables),
    names_to = "predictor", values_to = "value"
  ) %>%
  filter(!is.na(value), is.finite(value)) %>%
  group_by(crisis, predictor) %>%
  mutate(
    predictor_z = as.numeric(scale(value)),
    decile = ntile(value, 10)
  ) %>%
  group_by(crisis, predictor, decile) %>%
  summarise(
    predictor_z = median(predictor_z, na.rm = TRUE),
    mean_recovery = mean(recovery_ratio, na.rm = TRUE),
    se = sd(recovery_ratio, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    predictor = factor(
      predictor, levels = names(binned_labels), labels = binned_labels
    )
  )

add_plot(
  "37_binned_predictor_relationships",
  ggplot(
    binned_relationships,
    aes(predictor_z, mean_recovery, color = crisis, group = crisis)
  ) +
    geom_hline(
      yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.45
    ) +
    geom_errorbar(
      aes(ymin = mean_recovery - 1.96 * se, ymax = mean_recovery + 1.96 * se),
      width = 0.08, alpha = 0.6
    ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    facet_wrap(~predictor, scales = "free_y") +
    scale_color_manual(values = crisis_colors) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "Recovery across predictor deciles",
      subtitle = "Points are decile means; bars are approximate 95% confidence intervals",
      x = "Median standardized predictor value within decile",
      y = "Mean recovery ratio", color = "Crisis"
    ) +
    theme_report(base_size = 9)
)

make_standardized_model <- function(data, crisis_name) {
  model_data <- data %>%
    filter(
      crisis == crisis_name,
      median_hh_income > 0,
      total_population > 0,
      population_density > 0
    ) %>%
    transmute(
      recovery = recovery_ratio,
      poverty = poverty_rate,
      log_income = log(median_hh_income),
      log_population = log(total_population),
      log_density = log(population_density),
      proprietor_share,
      migration = net_migration_rate,
      bachelors = pct_bachelors_plus,
      birth_rate
    ) %>%
    drop_na() %>%
    mutate(across(everything(), ~ as.numeric(scale(.x))))

  fit <- lm(recovery ~ ., data = model_data)
  coefficient_table <- coef(summary(fit))
  intervals <- confint(fit)

  tibble(
    term = rownames(coefficient_table),
    estimate = coefficient_table[, "Estimate"],
    lower = intervals[, 1],
    upper = intervals[, 2],
    crisis = crisis_name,
    observations = nrow(model_data)
  ) %>%
    filter(term != "(Intercept)")
}

standardized_coefficients <- bind_rows(
  make_standardized_model(valid_county, "Great Recession"),
  make_standardized_model(valid_county, "COVID-19")
) %>%
  mutate(
    crisis = factor(crisis, levels = levels(county_long$crisis)),
    term = recode(
      term,
      poverty = "Poverty",
      log_income = "Log household income",
      log_population = "Log population",
      log_density = "Log population density",
      proprietor_share = "Proprietor share",
      migration = "Net migration",
      bachelors = "Bachelor's attainment",
      birth_rate = "Birth rate"
    )
  )

add_plot(
  "38_standardized_model_coefficients",
  ggplot(
    standardized_coefficients,
    aes(estimate, reorder(term, estimate), color = crisis)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey35") +
    geom_errorbar(
      aes(xmin = lower, xmax = upper),
      position = position_dodge(width = 0.55),
      width = 0.2, orientation = "y"
    ) +
    geom_point(position = position_dodge(width = 0.55), size = 2.3) +
    scale_color_manual(values = crisis_colors) +
    labs(
      title = "Multivariate predictors of county recovery",
      subtitle = "Standardized OLS coefficients with 95% confidence intervals",
      x = "Change in recovery (SD) per one-SD predictor increase",
      y = NULL, color = "Crisis",
      caption = paste(
        unique(standardized_coefficients$observations),
        collapse = " and "
      ) %>% paste("complete county-period observations.")
    ) +
    theme_report()
)

make_pca_scores <- function(data, crisis_name) {
  pca_data <- data %>%
    filter(
      crisis == crisis_name,
      median_hh_income > 0,
      total_population > 0,
      population_density > 0
    ) %>%
    transmute(
      fips, county_name, recovery_ratio,
      poverty = poverty_rate,
      log_income = log(median_hh_income),
      log_population = log(total_population),
      log_density = log(population_density),
      proprietor_share,
      migration = net_migration_rate,
      bachelors = pct_bachelors_plus,
      birth_rate
    ) %>%
    drop_na()

  pca <- prcomp(
    pca_data %>% select(-fips, -county_name, -recovery_ratio),
    center = TRUE, scale. = TRUE
  )
  explained <- 100 * pca$sdev^2 / sum(pca$sdev^2)

  bind_cols(
    pca_data %>% select(fips, county_name, recovery_ratio),
    as_tibble(pca$x[, 1:2])
  ) %>%
    mutate(
      crisis = crisis_name,
      recovery_quartile = ntile(recovery_ratio, 4),
      pc1_explained = explained[[1]],
      pc2_explained = explained[[2]]
    )
}

pca_scores <- bind_rows(
  make_pca_scores(valid_county, "Great Recession"),
  make_pca_scores(valid_county, "COVID-19")
) %>%
  mutate(
    crisis = factor(crisis, levels = levels(county_long$crisis)),
    recovery_quartile = factor(
      recovery_quartile, levels = 1:4,
      labels = c("Q1: Lowest", "Q2", "Q3", "Q4: Highest")
    )
  )

pca_centroids <- pca_scores %>%
  group_by(crisis, recovery_quartile) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")

add_plot(
  "39_county_predictor_pca",
  ggplot(pca_scores, aes(PC1, PC2, color = recovery_quartile)) +
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "grey80", linewidth = 0.4) +
    geom_point(alpha = 0.22, size = 0.85) +
    geom_point(
      data = pca_centroids, size = 4, shape = 21,
      fill = "white", stroke = 1.2
    ) +
    geom_text(
      data = pca_centroids,
      aes(label = recovery_quartile),
      color = "black", size = 2.5, vjust = -1.3
    ) +
    facet_wrap(~crisis, scales = "free") +
    scale_color_manual(
      values = c("#B8423F", "#D98C3F", "#5F9E8C", "#355C7D"),
      name = "Recovery quartile"
    ) +
    labs(
      title = "County economic structure in principal-component space",
      subtitle = "Large outlined points mark recovery-quartile centroids",
      x = "Principal component 1", y = "Principal component 2",
      caption = "PCA uses eight centered and scaled baseline predictors within each crisis."
    ) +
    theme_report()
)

state_recovery_gap <- state %>%
  filter(
    !is.na(recovery_ratio_2008), !is.na(recovery_ratio_covid),
    is.na(flag_implausible_2008) | !flag_implausible_2008,
    is.na(flag_implausible_covid) | !flag_implausible_covid
  ) %>%
  mutate(
    recovery_gap = recovery_ratio_covid - recovery_ratio_2008,
    state_name = factor(
      state_name,
      levels = state_name[order(recovery_gap)]
    )
  )

add_plot(
  "40_state_recovery_change",
  ggplot(
    state_recovery_gap,
    aes(recovery_gap, state_name, color = recovery_gap > 0)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey35") +
    geom_segment(
      aes(x = 0, xend = recovery_gap, yend = state_name),
      linewidth = 0.65
    ) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("FALSE" = "#C44E52", "TRUE" = "#2A7F62"),
      labels = c("Weaker after COVID-19", "Stronger after COVID-19"),
      name = NULL
    ) +
    scale_x_continuous(labels = label_percent(accuracy = 1)) +
    labs(
      title = "How state recovery changed between crises",
      x = "COVID-19 recovery ratio minus Great Recession ratio",
      y = NULL
    ) +
    theme_report(base_size = 8) +
    theme(panel.grid.major.y = element_blank())
)

rural_wealth <- valid_county %>%
  filter(!is.na(wealth_band), !is.na(rural_urban_code)) %>%
  mutate(
    rural_group = case_when(
      as.integer(rural_urban_code) <= 3 ~ "Metro",
      as.integer(rural_urban_code) <= 7 ~ "Nonmetro",
      TRUE ~ "Rural"
    ),
    rural_group = factor(rural_group, levels = c("Metro", "Nonmetro", "Rural"))
  ) %>%
  group_by(crisis, rural_group, wealth_band) %>%
  summarise(
    median_recovery = median(recovery_ratio),
    counties = n(),
    .groups = "drop"
  )

add_plot(
  "41_rural_wealth_interaction",
  ggplot(
    rural_wealth,
    aes(wealth_band, rural_group, fill = median_recovery)
  ) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(
      aes(
        label = paste0(
          label_percent(accuracy = 0.1)(median_recovery),
          "\nn=", comma(counties)
        )
      ),
      size = 3
    ) +
    facet_wrap(~crisis) +
    scale_fill_viridis_c(
      option = "C", labels = label_percent(accuracy = 1),
      name = "Median\nrecovery"
    ) +
    labs(
      title = "Recovery at the intersection of geography and wealth",
      x = "Baseline household-income band", y = "Rural–urban group"
    ) +
    theme_report()
)

# ---- Save individual PNGs and a combined PDF -------------------------------

invalid_plot_names <- names(plots)[
  !vapply(plots, inherits, logical(1), what = "ggplot")
]
if (length(invalid_plot_names) > 0) {
  stop(
    "These entries are not ggplot objects: ",
    paste(invalid_plot_names, collapse = ", "),
    call. = FALSE
  )
}

message("Saving ", length(plots), " high-resolution PNG files to ", figure_dir)

walk2(
  plots,
  names(plots),
  function(plot, plot_name) {
    is_map <- str_detect(plot_name, "_map_")
    ggsave(
      filename = file.path(figure_dir, paste0(plot_name, ".png")),
      plot = plot,
      width = if (is_map) 11 else 9,
      height = if (is_map) 7 else 6,
      units = "in",
      dpi = 300,
      bg = "white"
    )
  }
)

pdf_path <- file.path(figure_dir, "all_graphs.pdf")
grDevices::pdf(pdf_path, width = 11, height = 8.5, onefile = TRUE)
tryCatch(
  walk(plots, print),
  finally = grDevices::dev.off()
)

png_paths <- file.path(figure_dir, paste0(names(plots), ".png"))
bad_pngs <- png_paths[!file.exists(png_paths) | file.info(png_paths)$size <= 0]

if (length(bad_pngs) > 0 || !file.exists(pdf_path) ||
    file.info(pdf_path)$size <= 0 ||
    !file.exists(processed_path) || file.info(processed_path)$size <= 0) {
  stop(
    "One or more graph files were not written correctly: ",
    paste(basename(bad_pngs), collapse = ", "),
    call. = FALSE
  )
}

message(
  "Done: wrote ", length(png_paths), " PNG files and ",
  pdf_path, ", plus ", processed_path, "."
)
