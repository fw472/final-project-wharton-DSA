# 05_viability_check.R
#
# The "does this actually work" test. Run after 04_merge_report.R.
# Not the real analysis -- just proof the pipeline runs end to end and the
# outcome looks sane before four teammates start joining predictors onto it.

library(dplyr)
library(readr)

data <- read_csv("spine_data.csv", show_col_types = FALSE)

# ---- 1. distribution of both recovery outcomes ----
describe_distribution <- function(x, label) {
  x <- x[!is.na(x)]
  q <- quantile(x, c(0, .25, .5, .75, 1))
  message(sprintf("\n=== %s: n = %d ===", label, length(x)))
  message(sprintf("min = %.3f | Q1 = %.3f | median = %.3f | Q3 = %.3f | max = %.3f",
                   q[1], q[2], q[3], q[4], q[5]))
  message(sprintf("mean = %.3f | sd = %.3f", mean(x), sd(x)))
  message(sprintf("share below 0.7 (weak recovery): %.1f%% | share below 1.0 (not fully recovered): %.1f%%",
                   100 * mean(x < 0.7), 100 * mean(x < 1.0)))
}

describe_distribution(data$recovery_ratio_2008, "2008 RECOVERY RATIO")
describe_distribution(data$recovery_ratio_covid, "COVID RECOVERY RATIO")

# ---- 2. 10 best / 10 worst per crisis, with the raw numbers behind the ratio ----
show_extremes <- function(data, ratio_col, peak_year_col, peak_emp_col,
                           trough_year_col, trough_emp_col, label) {
  cols <- c("county_name", ratio_col, peak_year_col, peak_emp_col, trough_year_col, trough_emp_col)
  d <- data %>% filter(!is.na(.data[[ratio_col]])) %>% select(all_of(cols))

  message(sprintf("\n=== %s: 10 BEST RECOVERING COUNTIES ===", label))
  print(as.data.frame(d %>% arrange(desc(.data[[ratio_col]])) %>% slice_head(n = 10)), row.names = FALSE)

  message(sprintf("\n=== %s: 10 WORST RECOVERING COUNTIES ===", label))
  print(as.data.frame(d %>% arrange(.data[[ratio_col]]) %>% slice_head(n = 10)), row.names = FALSE)
}

show_extremes(data, "recovery_ratio_2008", "peak_year_2008", "peak_employment_2008",
              "trough_year_2008", "trough_employment_2008", "2008 CRISIS")
show_extremes(data, "recovery_ratio_covid", "peak_year_covid", "peak_employment_covid",
              "trough_year_covid", "trough_employment_covid", "COVID CRISIS")

# ---- 3. smoke-test OLS: recovery ~ controls, one per crisis ----
message("\n=== SMOKE-TEST OLS: 2008 recovery ~ pre-2008 controls ===")
model_2008 <- lm(recovery_ratio_2008 ~ poverty_rate_2007 + median_hh_income_2007 +
                    total_population_2007 + population_density_2007 + rucc_2003_2007,
                  data = data)
print(summary(model_2008))

message("\n=== SMOKE-TEST OLS: COVID recovery ~ pre-COVID controls ===")
model_covid <- lm(recovery_ratio_covid ~ poverty_rate_2019 + median_hh_income_2019 +
                     total_population_2019 + population_density_2019 + rucc_2013_2019,
                   data = data)
print(summary(model_covid))

# ---- 4. surviving-county count (the locked list for teammates) ----
control_cols_2008 <- c("poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
                        "population_density_2007", "rucc_2003_2007")
control_cols_covid <- c("poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
                         "population_density_2019", "rucc_2013_2019")

complete_2008 <- data %>% filter(!is.na(recovery_ratio_2008), if_all(all_of(control_cols_2008), ~ !is.na(.)))
complete_covid <- data %>% filter(!is.na(recovery_ratio_covid), if_all(all_of(control_cols_covid), ~ !is.na(.)))
complete_both <- data %>% filter(fips %in% complete_2008$fips, fips %in% complete_covid$fips)

message("\n=== SURVIVING COUNTIES (locked list candidates) ===")
message(sprintf("2008 crisis complete cases:  %d / %d", nrow(complete_2008), nrow(data)))
message(sprintf("COVID crisis complete cases: %d / %d", nrow(complete_covid), nrow(data)))
message(sprintf("Complete for BOTH crises:    %d / %d", nrow(complete_both), nrow(data)))
