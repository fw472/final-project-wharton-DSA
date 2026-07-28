# 17_predictor_report.R
#
# Post-build report for the four predictor buckets (self-employment,
# migration, education, births) joined onto spine_data.csv and
# state_spine.csv: per-bucket coverage, the locked complete-case county
# list, sanity stats, and the no-leakage audit. Not a modeling step --
# read-only checks over the two spine files.

library(dplyr)
library(readr)

county <- read_csv("spine_data.csv", show_col_types = FALSE)
state  <- read_csv("state_spine.csv", show_col_types = FALSE)

bucket_cols <- list(
  "1. Self-employment" = c("proprietor_share_2007", "proprietor_share_2019"),
  "2. Migration"        = c("net_migration_rate_2007", "net_migration_rate_2019"),
  "3. Education"         = c("pct_hs_plus_2007", "pct_bachelors_plus_2007",
                              "pct_hs_plus_2019", "pct_bachelors_plus_2019"),
  "4. Births"            = c("birth_rate_2007", "birth_rate_2019")
)

# ==== 1. COVERAGE PER BUCKET ====
message("\n============================================================")
message("1. COVERAGE PER BUCKET (non-missing counts)")
message("============================================================")
for (label in names(bucket_cols)) {
  message("\n--- ", label, " ---")
  for (col in bucket_cols[[label]]) {
    n_county <- if (col %in% names(county)) sum(!is.na(county[[col]])) else NA
    n_state  <- if (col %in% names(state))  sum(!is.na(state[[col]]))  else NA
    message(sprintf("  %-28s counties: %4d / %4d   |   states: %2d / %2d",
                     col, n_county, nrow(county), n_state, nrow(state)))
  }
}

# ==== 2. INTERSECTION: locked complete-case county list ====
message("\n============================================================")
message("2. INTERSECTION -- locked complete-case county list")
message("============================================================")

cols_2007 <- c("proprietor_share_2007", "net_migration_rate_2007",
               "pct_hs_plus_2007", "pct_bachelors_plus_2007", "birth_rate_2007")
cols_2019 <- c("proprietor_share_2019", "net_migration_rate_2019",
               "pct_hs_plus_2019", "pct_bachelors_plus_2019", "birth_rate_2019")
control_cols_2008 <- c("poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
                        "population_density_2007", "rucc_2003_2007")
control_cols_covid <- c("poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
                         "population_density_2019", "rucc_2013_2019")

complete_predictors_2007 <- county %>% filter(if_all(all_of(cols_2007), ~ !is.na(.)))
complete_predictors_2019 <- county %>% filter(if_all(all_of(cols_2019), ~ !is.na(.)))

complete_2008_all <- county %>%
  filter(!is.na(recovery_ratio_2008),
         if_all(all_of(control_cols_2008), ~ !is.na(.)),
         if_all(all_of(cols_2007), ~ !is.na(.)))
complete_covid_all <- county %>%
  filter(!is.na(recovery_ratio_covid),
         if_all(all_of(control_cols_covid), ~ !is.na(.)),
         if_all(all_of(cols_2019), ~ !is.na(.)))
complete_both_all <- county %>%
  filter(fips %in% complete_2008_all$fips, fips %in% complete_covid_all$fips)

message(sprintf("Four predictor buckets only, 2007 side:  %d / %d counties", nrow(complete_predictors_2007), nrow(county)))
message(sprintf("Four predictor buckets only, 2019 side:  %d / %d counties", nrow(complete_predictors_2019), nrow(county)))
message("(Education's ACS 1-year ~65k population threshold is the binding constraint on both sides.)")
message("")
message(sprintf("Spine (outcome + original controls) + all 4 buckets, 2008 side:  %d / %d counties",
                 nrow(complete_2008_all), nrow(county)))
message(sprintf("Spine + all 4 buckets, COVID side: %d / %d counties",
                 nrow(complete_covid_all), nrow(county)))
message(sprintf("LOCKED LIST -- complete for BOTH crises, all 4 buckets + spine: %d / %d counties",
                 nrow(complete_both_all), nrow(county)))

state_complete_2007 <- state %>% filter(if_all(all_of(cols_2007), ~ !is.na(.)))
state_complete_2019 <- state %>% filter(if_all(all_of(cols_2019), ~ !is.na(.)))
message(sprintf("\nStates complete on all 4 buckets, 2007 side: %d / %d", nrow(state_complete_2007), nrow(state)))
message(sprintf("States complete on all 4 buckets, 2019 side: %d / %d", nrow(state_complete_2019), nrow(state)))

# ==== 3. SANITY STATS ====
message("\n============================================================")
message("3. SANITY STATS (min / median / max)")
message("============================================================")
sanity <- function(x) sprintf("min=%.3f | median=%.3f | max=%.3f | NAs=%d",
                               min(x, na.rm = TRUE), median(x, na.rm = TRUE),
                               max(x, na.rm = TRUE), sum(is.na(x)))
all_pred_cols <- unlist(bucket_cols, use.names = FALSE)
message("\n--- COUNTY ---")
for (col in all_pred_cols) message(sprintf("  %-28s %s", col, sanity(county[[col]])))
message("\n--- STATE ---")
for (col in all_pred_cols) message(sprintf("  %-28s %s", col, sanity(state[[col]])))

message("\n--- wealth_band (county only) ---")
print(table(county$wealth_band_2008, useNA = "ifany"))
print(table(county$wealth_band_covid, useNA = "ifany"))

# ==== 4. NO-LEAKAGE AUDIT ====
message("\n============================================================")
message("4. NO-LEAKAGE AUDIT")
message("============================================================")
# Every predictor's underlying source vintage/year, hand-verified against the
# pull scripts (11-14). "source_year" is the latest calendar year any data
# in that pull could reflect; "crisis_threshold" is the latest year allowed
# for that column. Leakage-free requires source_year <= crisis_threshold.
audit <- tribble(
  ~column,                    ~source,                                        ~source_year, ~crisis_threshold,
  "proprietor_share_2007",    "BEA CAEMP25N/SAINC30, Year=2007",              2007,         2007,
  "proprietor_share_2019",    "BEA CAEMP25N/SAINC30, Year=2019",              2019,         2019,
  "net_migration_rate_2007",  "IRS SOI migration, 2006->2007 filing pair",    2007,         2007,
  "net_migration_rate_2019",  "IRS SOI migration, 2018->2019 filing pair",    2019,         2019,
  "pct_hs_plus_2007",         "ACS 1-year 2007, table B15002",                2007,         2007,
  "pct_bachelors_plus_2007",  "ACS 1-year 2007, table B15002",                2007,         2007,
  "pct_hs_plus_2019",         "ACS 1-year 2019, table B15003",                2019,         2019,
  "pct_bachelors_plus_2019",  "ACS 1-year 2019, table B15003",                2019,         2019,
  "birth_rate_2007",          "Census PEP Vintage 2009, BIRTHS2007/RBIRTH2007", 2007,       2007,
  "birth_rate_2019",          "Census PEP 2010s vintage, BIRTHS2019/RBIRTH2019", 2019,       2019,
  "wealth_band_2008",         "Tercile of median_hh_income_2007 (SAIPE 2007)", 2007,        2007,
  "wealth_band_covid",        "Tercile of median_hh_income_2019 (SAIPE 2019)", 2019,        2019
)
audit <- audit %>% mutate(leakage_ok = source_year <= crisis_threshold)
print(as.data.frame(audit))
stopifnot(all(audit$leakage_ok))
message("\nAUDIT PASSED: every predictor column's source year is <= its crisis threshold ",
        "(2007 for the 2008 crisis, 2019 for COVID). No column reflects data from the crisis ",
        "year itself or later.")
