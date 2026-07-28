# 02_outcome.R
#
# Builds the recovery outcome, once for 2008 and once for COVID, from the
# BLS employment panel in 01_bls_employment.R.
#
# Definition (approved by project owner before this was written):
#   - Peak = each county's OWN max annual employment within a search window
#     (county timing varies -- a fixed calendar year for everyone would be
#     wrong). Trough = each county's OWN min annual employment in the years
#     after its peak, up to a crisis-specific upper bound.
#   - Recovery ratio = mean(employment at trough+2, employment at trough+3)
#     / peak employment. Averaging two years instead of one smooths one-year
#     flukes; the trough+2/trough+3 window is IDENTICAL for both crises.
#
#   2008 crisis:  peak search 2006-2008 | trough search (peak_year+1)-2013
#   COVID crisis: peak search 2018-2019 | trough search (peak_year+1)-2022
#
# (Trough windows widened from an initial 2011/2021 boundary after the first
# run showed 28.4%/17.3% of counties pinned at that edge -- see project
# owner's review.)
#
# Flags (never delete rows -- non-recovering counties are the point):
#   flag_tiny_base_*    : peak employment < 1000 (small-denominator noise)
#   flag_implausible_*  : recovery ratio < 0.3 or > 2.5 (near-certain data
#                         artifact rather than a real outcome)
#   never_recovered_*   : employment never exceeds the pre-crisis peak at ANY
#                         point in the panel after the peak year (through
#                         2025, the last available year). This is a DIFFERENT
#                         population than a low recovery_ratio: widening the
#                         trough-search window past 2013 (2008 crisis) barely
#                         moved the share pinned at the boundary (28.4% ->
#                         27.8%), and tracing those counties showed why -- 79%
#                         of them never get back above their 2006-08 peak
#                         through 2025 at all. That's long-run secular decline
#                         (mostly small rural counties), not a slow-motion
#                         2008 recovery still in progress. recovery_ratio
#                         already captures these correctly as low values; this
#                         flag just separates "structural never-recovery" from
#                         "slow but real recovery" for the writeup and for
#                         later checking whether predictors behave differently
#                         across the two groups.

library(dplyr)
library(tidyr)

panel <- readRDS("raw_data/01_bls_panel.rds")

# one row per fips, taking the county name from its most recent year on file
county_names <- panel %>%
  arrange(fips, desc(year)) %>%
  distinct(fips, .keep_all = TRUE) %>%
  select(fips, county_name)

compute_recovery <- function(panel, peak_window, trough_upper, suffix) {
  result <- panel %>%
    filter(year %in% peak_window) %>%
    filter(!is.na(employed)) %>%
    group_by(fips) %>%
    slice_max(employed, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(fips, peak_year = year, peak_employment = employed)

  result <- result %>%
    rowwise() %>%
    mutate(trough_window_upper = trough_upper) %>%
    ungroup()

  trough_lookup <- panel %>%
    inner_join(result %>% select(fips, peak_year, trough_window_upper), by = "fips") %>%
    filter(year > peak_year, year <= trough_window_upper) %>%
    filter(!is.na(employed)) %>%
    group_by(fips) %>%
    slice_min(employed, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(fips, trough_year = year, trough_employment = employed)

  result <- result %>%
    select(-trough_window_upper) %>%
    left_join(trough_lookup, by = "fips")

  recovery_lookup <- result %>%
    filter(!is.na(trough_year)) %>%
    select(fips, trough_year) %>%
    mutate(ry1 = trough_year + 2, ry2 = trough_year + 3)

  recovery_values <- panel %>%
    inner_join(recovery_lookup, by = "fips") %>%
    filter(year == ry1 | year == ry2) %>%
    filter(!is.na(employed)) %>%
    group_by(fips) %>%
    summarise(recovery_employment = mean(employed), n_recovery_years = n(), .groups = "drop")

  # never-recovered: does employment EVER exceed the peak again, at any
  # point in the full panel after the peak year (not bounded by trough_upper)
  post_peak_max <- panel %>%
    inner_join(result %>% select(fips, peak_year), by = "fips") %>%
    filter(year > peak_year) %>%
    filter(!is.na(employed)) %>%
    group_by(fips) %>%
    summarise(max_employment_after_peak = max(employed), .groups = "drop")

  result <- result %>%
    left_join(recovery_values, by = "fips") %>%
    left_join(post_peak_max, by = "fips") %>%
    mutate(
      recovery_ratio = recovery_employment / peak_employment,
      flag_tiny_base = peak_employment < 1000,
      flag_implausible = !is.na(recovery_ratio) & (recovery_ratio < 0.3 | recovery_ratio > 2.5),
      never_recovered = !is.na(max_employment_after_peak) & (max_employment_after_peak < peak_employment)
    ) %>%
    select(-max_employment_after_peak)

  names(result)[names(result) != "fips"] <- paste0(names(result)[names(result) != "fips"], "_", suffix)
  result
}

outcome_2008 <- compute_recovery(panel, peak_window = 2006:2008, trough_upper = 2013, suffix = "2008")
outcome_covid <- compute_recovery(panel, peak_window = 2018:2019, trough_upper = 2022, suffix = "covid")

outcome <- county_names %>%
  left_join(outcome_2008, by = "fips") %>%
  left_join(outcome_covid, by = "fips")

stopifnot(all(nchar(outcome$fips) == 5), !any(duplicated(outcome$fips)))

# ---- diagnostics the project owner asked to see before anything else ----
message("\n=== 2008 CRISIS: TROUGH YEAR DISTRIBUTION ===")
t2008 <- table(outcome$trough_year_2008, useNA = "ifany")
print(t2008)
pct_at_boundary_2008 <- 100 * sum(outcome$trough_year_2008 == 2013, na.rm = TRUE) / sum(!is.na(outcome$trough_year_2008))
message(sprintf("Counties pinned at the 2013 search-window boundary: %.1f%%", pct_at_boundary_2008))

message("\n=== COVID CRISIS: TROUGH YEAR DISTRIBUTION ===")
tcovid <- table(outcome$trough_year_covid, useNA = "ifany")
print(tcovid)
pct_at_boundary_covid <- 100 * sum(outcome$trough_year_covid == 2022, na.rm = TRUE) / sum(!is.na(outcome$trough_year_covid))
message(sprintf("Counties pinned at the 2022 search-window boundary: %.1f%%", pct_at_boundary_covid))

message("\n=== FLAG COUNTS ===")
message(sprintf("flag_tiny_base_2008:    %d / %d", sum(outcome$flag_tiny_base_2008, na.rm = TRUE), nrow(outcome)))
message(sprintf("flag_implausible_2008:  %d / %d", sum(outcome$flag_implausible_2008, na.rm = TRUE), nrow(outcome)))
message(sprintf("flag_tiny_base_covid:   %d / %d", sum(outcome$flag_tiny_base_covid, na.rm = TRUE), nrow(outcome)))
message(sprintf("flag_implausible_covid: %d / %d", sum(outcome$flag_implausible_covid, na.rm = TRUE), nrow(outcome)))
message(sprintf("never_recovered_2008:   %d / %d", sum(outcome$never_recovered_2008, na.rm = TRUE), nrow(outcome)))
message(sprintf("never_recovered_covid:  %d / %d", sum(outcome$never_recovered_covid, na.rm = TRUE), nrow(outcome)))

message("\nOutcome built for ", nrow(outcome), " counties (",
        sum(!is.na(outcome$recovery_ratio_2008)), " with a 2008 recovery ratio, ",
        sum(!is.na(outcome$recovery_ratio_covid)), " with a COVID recovery ratio).")

saveRDS(outcome, "raw_data/02_outcome.rds")
