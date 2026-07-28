# 07_state_outcome.R
#
# State-level recovery outcome, mirroring 02_outcome.R exactly: same
# peak/trough search-window logic, same peak-window and trough-upper-bound
# years (already widened and locked on the county side), same
# trough+2/trough+3 averaged recovery window. Applied to the state
# employment panel from 06_state_panel.R instead of the county panel.
#
#   2008 crisis:  peak search 2006-2008 | trough search (peak_year+1)-2013
#   COVID crisis: peak search 2018-2019 | trough search (peak_year+1)-2022
#
# Flags:
#   flag_implausible_* : recovery ratio < 0.3 or > 2.5 (same rule as county)
#   never_recovered_*  : employment never exceeds the pre-crisis peak at any
#                        point in the panel after the peak year (through
#                        2025) -- same definition as the county spine.
#
# flag_tiny_base is DELIBERATELY DROPPED at the state level: it was a
# peak-employment < 1000 threshold calibrated for county-level small-
# denominator noise (a few dozen jobs swinging a tiny county's ratio). No
# state has employment anywhere near that scale, so the flag would just be
# FALSE for all 51 rows -- a meaningless column, not a meaningful check.

library(dplyr)

panel <- readRDS("raw_data/06_state_panel.rds")

state_names <- panel %>% distinct(state_fips, state_abbr, state_name)

compute_recovery <- function(panel, peak_window, trough_upper, suffix) {
  result <- panel %>%
    filter(year %in% peak_window) %>%
    filter(!is.na(employed)) %>%
    group_by(state_fips) %>%
    slice_max(employed, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(state_fips, peak_year = year, peak_employment = employed)

  result <- result %>%
    mutate(trough_window_upper = trough_upper)

  trough_lookup <- panel %>%
    inner_join(result %>% select(state_fips, peak_year, trough_window_upper), by = "state_fips") %>%
    filter(year > peak_year, year <= trough_window_upper) %>%
    filter(!is.na(employed)) %>%
    group_by(state_fips) %>%
    slice_min(employed, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(state_fips, trough_year = year, trough_employment = employed)

  result <- result %>%
    select(-trough_window_upper) %>%
    left_join(trough_lookup, by = "state_fips")

  recovery_lookup <- result %>%
    filter(!is.na(trough_year)) %>%
    select(state_fips, trough_year) %>%
    mutate(ry1 = trough_year + 2, ry2 = trough_year + 3)

  recovery_values <- panel %>%
    inner_join(recovery_lookup, by = "state_fips") %>%
    filter(year == ry1 | year == ry2) %>%
    filter(!is.na(employed)) %>%
    group_by(state_fips) %>%
    summarise(recovery_employment = mean(employed), n_recovery_years = n(), .groups = "drop")

  post_peak_max <- panel %>%
    inner_join(result %>% select(state_fips, peak_year), by = "state_fips") %>%
    filter(year > peak_year) %>%
    filter(!is.na(employed)) %>%
    group_by(state_fips) %>%
    summarise(max_employment_after_peak = max(employed), .groups = "drop")

  result <- result %>%
    left_join(recovery_values, by = "state_fips") %>%
    left_join(post_peak_max, by = "state_fips") %>%
    mutate(
      recovery_ratio = recovery_employment / peak_employment,
      flag_implausible = !is.na(recovery_ratio) & (recovery_ratio < 0.3 | recovery_ratio > 2.5),
      never_recovered = !is.na(max_employment_after_peak) & (max_employment_after_peak < peak_employment)
    ) %>%
    select(-max_employment_after_peak)

  names(result)[names(result) != "state_fips"] <- paste0(names(result)[names(result) != "state_fips"], "_", suffix)
  result
}

outcome_2008 <- compute_recovery(panel, peak_window = 2006:2008, trough_upper = 2013, suffix = "2008")
outcome_covid <- compute_recovery(panel, peak_window = 2018:2019, trough_upper = 2022, suffix = "covid")

outcome <- state_names %>%
  left_join(outcome_2008, by = "state_fips") %>%
  left_join(outcome_covid, by = "state_fips")

stopifnot(nrow(outcome) == 51, !any(duplicated(outcome$state_fips)))

message("\n=== STATE 2008 CRISIS: TROUGH YEAR DISTRIBUTION ===")
print(table(outcome$trough_year_2008, useNA = "ifany"))

message("\n=== STATE COVID CRISIS: TROUGH YEAR DISTRIBUTION ===")
print(table(outcome$trough_year_covid, useNA = "ifany"))

message("\n=== STATE FLAG COUNTS ===")
message(sprintf("flag_implausible_2008:  %d / 51", sum(outcome$flag_implausible_2008, na.rm = TRUE)))
message(sprintf("flag_implausible_covid: %d / 51", sum(outcome$flag_implausible_covid, na.rm = TRUE)))
message(sprintf("never_recovered_2008:   %d / 51", sum(outcome$never_recovered_2008, na.rm = TRUE)))
message(sprintf("never_recovered_covid:  %d / 51", sum(outcome$never_recovered_covid, na.rm = TRUE)))

message("\nStates/DC with a 2008 recovery ratio: ", sum(!is.na(outcome$recovery_ratio_2008)), " / 51")
message("States/DC with a COVID recovery ratio: ", sum(!is.na(outcome$recovery_ratio_covid)), " / 51")

saveRDS(outcome, "raw_data/07_state_outcome.rds")
