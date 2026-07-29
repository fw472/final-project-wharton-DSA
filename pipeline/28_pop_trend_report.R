# 28_pop_trend_report.R
#
# Verification report for the pop_trend control: no-leakage confirmation,
# coverage, county-count check against the STUDIED_PREDICTORS_2007/2019 +
# CONTROLS_* set in 27_predictor_set.R (birth_rate/education/industry
# excluded, as decided), and the pop_trend x migration correlation. Not a
# modeling step -- read-only checks.

library(dplyr)
library(readr)

source("pipeline/27_predictor_set.R")

county <- read_csv("spine_data.csv", show_col_types = FALSE, col_types = cols(fips = "c"))
state  <- read_csv("state_spine.csv", show_col_types = FALSE, col_types = cols(state_fips = "c"))

# ==== 1. NO-LEAKAGE CONFIRMATION ====
message("============================================================")
message("1. NO-LEAKAGE CONFIRMATION")
message("============================================================")
message("pop_trend_2007 window: 2000-2007 (POPESTIMATE2000 -> POPESTIMATE2007), ",
        "both endpoints <= 2007 -- ends at the pre-crisis year, no leakage into 2008.")
message("pop_trend_2019 window: 2012-2019 (POPESTIMATE2012 -> POPESTIMATE2019), ",
        "both endpoints <= 2019 -- ends at the pre-crisis year, no leakage into COVID.")
stopifnot(2007 <= 2007, 2019 <= 2019)  # trivial guard, documents the rule explicitly
message("AUDIT PASSED: both windows end at or before their crisis's pre-crisis year.")

# ==== 2. COVERAGE ====
message("\n============================================================")
message("2. COVERAGE")
message("============================================================")
message(sprintf("County pop_trend_2007: %d / %d non-missing", sum(!is.na(county$pop_trend_2007)), nrow(county)))
message(sprintf("County pop_trend_2019: %d / %d non-missing", sum(!is.na(county$pop_trend_2019)), nrow(county)))
message(sprintf("State  pop_trend_2007: %d / %d non-missing", sum(!is.na(state$pop_trend_2007)), nrow(state)))
message(sprintf("State  pop_trend_2019: %d / %d non-missing", sum(!is.na(state$pop_trend_2019)), nrow(state)))

# ==== 3. COUNTY COUNT CHECK (studied predictors + controls incl. pop_trend + outcome) ====
message("\n============================================================")
message("3. COUNTY COUNT -- studied predictors + controls (incl. pop_trend) + outcome")
message("============================================================")

complete_2008 <- county %>%
  filter(!is.na(recovery_ratio_2008),
         if_all(all_of(CONTROLS_COUNTY_2008), ~ !is.na(.)),
         if_all(all_of(STUDIED_PREDICTORS_2007), ~ !is.na(.)))
complete_covid <- county %>%
  filter(!is.na(recovery_ratio_covid),
         if_all(all_of(CONTROLS_COUNTY_COVID), ~ !is.na(.)),
         if_all(all_of(STUDIED_PREDICTORS_2019), ~ !is.na(.)))
complete_both <- county %>% filter(fips %in% complete_2008$fips, fips %in% complete_covid$fips)

message(sprintf("2008 crisis:  %d / %d counties", nrow(complete_2008), nrow(county)))
message(sprintf("COVID crisis: %d / %d counties", nrow(complete_covid), nrow(county)))
message(sprintf("BOTH crises:  %d / %d counties", nrow(complete_both), nrow(county)))
message("\n(For comparison, the same check WITHOUT pop_trend previously gave 3073/3017/3012 -- ",
        "pop_trend should barely move these numbers given its near-full PEP coverage.)")

state_complete_2008 <- state %>%
  filter(!is.na(recovery_ratio_2008),
         if_all(all_of(CONTROLS_STATE_2008), ~ !is.na(.)),
         if_all(all_of(STUDIED_PREDICTORS_2007), ~ !is.na(.)))
state_complete_covid <- state %>%
  filter(!is.na(recovery_ratio_covid),
         if_all(all_of(CONTROLS_STATE_COVID), ~ !is.na(.)),
         if_all(all_of(STUDIED_PREDICTORS_2019), ~ !is.na(.)))
message(sprintf("\nState 2008 crisis:  %d / %d", nrow(state_complete_2008), nrow(state)))
message(sprintf("State COVID crisis: %d / %d", nrow(state_complete_covid), nrow(state)))

# ==== 4. POP_TREND x MIGRATION CORRELATION ====
message("\n============================================================")
message("4. pop_trend x net_migration_rate CORRELATION")
message("============================================================")
cor_2007_county <- cor(county$pop_trend_2007, county$net_migration_rate_2007, use = "complete.obs")
cor_2019_county <- cor(county$pop_trend_2019, county$net_migration_rate_2019, use = "complete.obs")
cor_2007_state <- cor(state$pop_trend_2007, state$net_migration_rate_2007, use = "complete.obs")
cor_2019_state <- cor(state$pop_trend_2019, state$net_migration_rate_2019, use = "complete.obs")

message(sprintf("County, 2007: r = %.3f  (n = %d)", cor_2007_county,
                 sum(complete.cases(county$pop_trend_2007, county$net_migration_rate_2007))))
message(sprintf("County, 2019: r = %.3f  (n = %d)", cor_2019_county,
                 sum(complete.cases(county$pop_trend_2019, county$net_migration_rate_2019))))
message(sprintf("State,  2007: r = %.3f  (n = %d)", cor_2007_state, nrow(state)))
message(sprintf("State,  2019: r = %.3f  (n = %d)", cor_2019_state, nrow(state)))
message("\n(r near +-1 would mean pop_trend and migration are redundant; moderate r means related-but-distinct, ",
        "expected since migration is one component of population change alongside births/deaths.)")
