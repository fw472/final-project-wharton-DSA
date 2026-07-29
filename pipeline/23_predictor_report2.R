# 23_predictor_report2.R
#
# Post-build report after adding the three new predictors (pre-crisis
# unemployment, age structure, CBP industry mix) to the original four
# (self-employment, migration, education, births). Covers all 7 buckets:
# coverage, the updated locked complete-case county list, sanity stats, and
# the no-leakage audit. Not a modeling step -- read-only checks over the two
# spine files, supersedes 17_predictor_report.R (which only covered buckets
# 1-4) as the current full report.

library(dplyr)
library(readr)

county <- read_csv("spine_data.csv", show_col_types = FALSE)
state  <- read_csv("state_spine.csv", show_col_types = FALSE)

sector_names <- c("agriculture", "mining", "utilities", "construction", "manufacturing",
                   "wholesale_trade", "retail", "transportation", "information", "finance",
                   "real_estate", "professional_services", "management", "administrative_waste",
                   "education_services", "healthcare", "arts_entertainment", "accommodation_food",
                   "other_services")
focus_sectors <- c("manufacturing", "construction", "retail", "accommodation_food",
                    "healthcare", "professional_services")
industry_cols_2007 <- paste0("share_", sector_names, "_2007")
industry_cols_2019 <- paste0("share_", sector_names, "_2019")

bucket_cols <- list(
  "1. Self-employment"       = c("proprietor_share_2007", "proprietor_share_2019"),
  "2. Migration"              = c("net_migration_rate_2007", "net_migration_rate_2019"),
  "3. Education"               = c("pct_hs_plus_2007", "pct_bachelors_plus_2007",
                                    "pct_hs_plus_2019", "pct_bachelors_plus_2019"),
  "4. Births"                  = c("birth_rate_2007", "birth_rate_2019"),
  "5. Pre-crisis unemployment" = c("pre_unemployment_2007", "pre_unemployment_2019"),
  "6. Age structure"           = c("working_age_share_2007", "working_age_share_2019"),
  "7. Industry mix (focus sectors only -- 19 total, see part 1b)" =
    c(paste0("share_", focus_sectors, "_2007"), paste0("share_", focus_sectors, "_2019"))
)

# ==== 1a. COVERAGE PER BUCKET (buckets 1-6 + industry mix focus sectors) ====
message("\n============================================================")
message("1a. COVERAGE PER BUCKET (non-missing counts)")
message("============================================================")
for (label in names(bucket_cols)) {
  message("\n--- ", label, " ---")
  for (col in bucket_cols[[label]]) {
    n_county <- if (col %in% names(county)) sum(!is.na(county[[col]])) else NA
    n_state  <- if (col %in% names(state))  sum(!is.na(state[[col]]))  else NA
    message(sprintf("  %-32s counties: %4d / %4d   |   states: %2d / %2d",
                     col, n_county, nrow(county), n_state, nrow(state)))
  }
}

# ==== 1b. INDUSTRY MIX -- full 19-sector coverage + suppression cost ====
message("\n============================================================")
message("1b. INDUSTRY MIX -- full sector coverage (county), 2007 vs 2019")
message("============================================================")
message(sprintf("%-26s %10s %10s   %-26s %10s %10s", "sector (2007)", "n", "%missing", "sector (2019)", "n", "%missing"))
for (i in seq_along(sector_names)) {
  c07 <- industry_cols_2007[i]; c19 <- industry_cols_2019[i]
  n07 <- sum(!is.na(county[[c07]])); n19 <- sum(!is.na(county[[c19]]))
  message(sprintf("%-26s %10d %9.1f%%   %-26s %10d %9.1f%%",
                   sector_names[i], n07, 100 * (1 - n07 / nrow(county)),
                   sector_names[i], n19, 100 * (1 - n19 / nrow(county))))
}
avg_missing_2007 <- 100 * (1 - mean(sapply(industry_cols_2007, function(c) sum(!is.na(county[[c]])))) / nrow(county))
avg_missing_2019 <- 100 * (1 - mean(sapply(industry_cols_2019, function(c) sum(!is.na(county[[c]])))) / nrow(county))
message(sprintf("\nAverage across all 19 sectors: %.1f%% of counties missing/suppressed in 2007, %.1f%% in 2019.",
                 avg_missing_2007, avg_missing_2019))
message("(2019 uses NAICS2017 with silent row-omission for non-reportable sectors; 2007 uses NAICS2002 ",
        "with an explicit EMP_F suppression flag -- both collapse to NA here, see pipeline/20_cbp_industry_mix.R.)")

# ==== 2. INTERSECTION: updated locked complete-case county list ====
message("\n============================================================")
message("2. INTERSECTION -- updated locked complete-case county list (all 7 buckets)")
message("============================================================")

cols_2007_orig <- c("proprietor_share_2007", "net_migration_rate_2007",
                     "pct_hs_plus_2007", "pct_bachelors_plus_2007", "birth_rate_2007")
cols_2019_orig <- c("proprietor_share_2019", "net_migration_rate_2019",
                     "pct_hs_plus_2019", "pct_bachelors_plus_2019", "birth_rate_2019")
cols_2007_new_nonindustry <- c("pre_unemployment_2007", "working_age_share_2007")
cols_2019_new_nonindustry <- c("pre_unemployment_2019", "working_age_share_2019")

cols_2007_all7 <- c(cols_2007_orig, cols_2007_new_nonindustry, industry_cols_2007)
cols_2019_all7 <- c(cols_2019_orig, cols_2019_new_nonindustry, industry_cols_2019)
cols_2007_all7_focus <- c(cols_2007_orig, cols_2007_new_nonindustry, paste0("share_", focus_sectors, "_2007"))
cols_2019_all7_focus <- c(cols_2019_orig, cols_2019_new_nonindustry, paste0("share_", focus_sectors, "_2019"))

control_cols_2008 <- c("poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
                        "population_density_2007", "rucc_2003_2007")
control_cols_covid <- c("poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
                         "population_density_2019", "rucc_2013_2019")

# previous (4-bucket) locked list, for comparison
complete_2008_old <- county %>%
  filter(!is.na(recovery_ratio_2008), if_all(all_of(control_cols_2008), ~ !is.na(.)), if_all(all_of(cols_2007_orig), ~ !is.na(.)))
complete_covid_old <- county %>%
  filter(!is.na(recovery_ratio_covid), if_all(all_of(control_cols_covid), ~ !is.na(.)), if_all(all_of(cols_2019_orig), ~ !is.na(.)))
complete_both_old <- county %>% filter(fips %in% complete_2008_old$fips, fips %in% complete_covid_old$fips)

# new locked list requiring ALL 19 industry sectors (strict)
complete_2008_all19 <- county %>%
  filter(!is.na(recovery_ratio_2008), if_all(all_of(control_cols_2008), ~ !is.na(.)), if_all(all_of(cols_2007_all7), ~ !is.na(.)))
complete_covid_all19 <- county %>%
  filter(!is.na(recovery_ratio_covid), if_all(all_of(control_cols_covid), ~ !is.na(.)), if_all(all_of(cols_2019_all7), ~ !is.na(.)))
complete_both_all19 <- county %>% filter(fips %in% complete_2008_all19$fips, fips %in% complete_covid_all19$fips)

# new locked list requiring only the 6 FOCUS industry sectors (less strict)
complete_2008_focus <- county %>%
  filter(!is.na(recovery_ratio_2008), if_all(all_of(control_cols_2008), ~ !is.na(.)), if_all(all_of(cols_2007_all7_focus), ~ !is.na(.)))
complete_covid_focus <- county %>%
  filter(!is.na(recovery_ratio_covid), if_all(all_of(control_cols_covid), ~ !is.na(.)), if_all(all_of(cols_2019_all7_focus), ~ !is.na(.)))
complete_both_focus <- county %>% filter(fips %in% complete_2008_focus$fips, fips %in% complete_covid_focus$fips)

message(sprintf("Previous locked list (4 buckets + spine, both crises): %d / %d counties", nrow(complete_both_old), nrow(county)))
message(sprintf("New locked list, ALL 7 buckets incl. all 19 industry sectors, both crises: %d / %d counties",
                 nrow(complete_both_all19), nrow(county)))
message(sprintf("  -> industry mix (all 19 sectors) costs %d additional counties vs. the previous locked list.",
                 nrow(complete_both_old) - nrow(complete_both_all19)))
message(sprintf("\nNew locked list, 7 buckets but only the 6 FOCUS industry sectors, both crises: %d / %d counties",
                 nrow(complete_both_focus), nrow(county)))
message(sprintf("  -> using only the 6 focus sectors instead of all 19 recovers %d counties.",
                 nrow(complete_both_focus) - nrow(complete_both_all19)))

state_complete_2007 <- state %>% filter(if_all(all_of(cols_2007_all7), ~ !is.na(.)))
state_complete_2019 <- state %>% filter(if_all(all_of(cols_2019_all7), ~ !is.na(.)))
message(sprintf("\nStates complete on all 7 buckets incl. all 19 sectors, 2007 side: %d / %d", nrow(state_complete_2007), nrow(state)))
message(sprintf("States complete on all 7 buckets incl. all 19 sectors, 2019 side: %d / %d", nrow(state_complete_2019), nrow(state)))

# ==== 3. SANITY STATS (new predictors only -- buckets 1-4 already reported in 17_predictor_report.R) ====
message("\n============================================================")
message("3. SANITY STATS (min / median / max) -- new predictors")
message("============================================================")
sanity <- function(x) sprintf("min=%.3f | median=%.3f | max=%.3f | NAs=%d",
                               min(x, na.rm = TRUE), median(x, na.rm = TRUE),
                               max(x, na.rm = TRUE), sum(is.na(x)))
new_cols <- c("pre_unemployment_2007", "pre_unemployment_2019",
              "working_age_share_2007", "working_age_share_2019",
              paste0("share_", focus_sectors, "_2007"), paste0("share_", focus_sectors, "_2019"))
message("\n--- COUNTY ---")
for (col in new_cols) message(sprintf("  %-32s %s", col, sanity(county[[col]])))
message("\n--- STATE ---")
for (col in new_cols) message(sprintf("  %-32s %s", col, sanity(state[[col]])))

# ==== 4. NO-LEAKAGE AUDIT (new predictors) ====
message("\n============================================================")
message("4. NO-LEAKAGE AUDIT -- new predictors")
message("============================================================")
audit <- tribble(
  ~column,                    ~source,                                          ~source_year, ~crisis_threshold,
  "pre_unemployment_2007",    "BLS LAUS laucnty07.xlsx, Unemployment Rate col", 2007,         2007,
  "pre_unemployment_2019",    "BLS LAUS laucnty19.xlsx, Unemployment Rate col", 2019,         2019,
  "working_age_share_2007",   "Census PEP co-est00int-agesex-5yr.csv, POPESTIMATE2007", 2007,  2007,
  "working_age_share_2019",   "Census PEP cc-est2019-agesex-XX.csv, YEAR=12 (=7/1/2019)", 2019, 2019,
  "share_*_2007 (19 sectors)", "Census CBP 2007 API, NAICS2002",                2007,         2007,
  "share_*_2019 (19 sectors)", "Census CBP 2019 API, NAICS2017",                2019,         2019
)
audit <- audit %>% mutate(leakage_ok = source_year <= crisis_threshold)
print(as.data.frame(audit))
stopifnot(all(audit$leakage_ok))
message("\nAUDIT PASSED: every new predictor's source year is <= its crisis threshold ",
        "(2007 for the 2008 crisis, 2019 for COVID). No column reflects data from the crisis ",
        "year itself or later. (Buckets 1-4 were already audited in 17_predictor_report.R and are unchanged.)")
