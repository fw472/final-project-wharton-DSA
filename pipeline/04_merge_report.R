# 04_merge_report.R
#
# Joins the outcome (both crises) with the pre-crisis controls, reports
# match/drop counts at each step, and writes the spine CSV that teammates
# will join their predictors onto. Every county with a BLS employment
# record is kept -- controls that don't match just come in as NA, they are
# NOT used to drop counties (the outcome defines the analysis population).

library(dplyr)
library(readr)

outcome       <- readRDS("raw_data/02_outcome.rds")
controls_2008 <- readRDS("raw_data/03_controls_2008.rds")
controls_covid <- readRDS("raw_data/03_controls_covid.rds")

message("\n=== JOIN REPORT ===")
message("Starting population (counties with a BLS employment record): ", nrow(outcome))

join_and_report <- function(data, new_data, label) {
  n_matched <- sum(data$fips %in% new_data$fips)
  n_missing <- nrow(data) - n_matched
  message(sprintf("%-30s %d/%d counties matched (%d missing)",
                   label, n_matched, nrow(data), n_missing))
  left_join(data, new_data, by = "fips")
}

spine <- outcome
spine <- join_and_report(spine, controls_2008, "Controls 2007 (pre-2008)")
spine <- join_and_report(spine, controls_covid, "Controls 2019 (pre-COVID)")

message("\nFinal spine: ", nrow(spine), " counties, ", ncol(spine), " columns.")

# ---- surviving-county counts: the locked list for teammates ----
control_cols_2008 <- c("poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
                        "population_density_2007", "rucc_2003_2007")
control_cols_covid <- c("poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
                         "population_density_2019", "rucc_2013_2019")

complete_2008 <- spine %>%
  filter(!is.na(recovery_ratio_2008), if_all(all_of(control_cols_2008), ~ !is.na(.)))
complete_covid <- spine %>%
  filter(!is.na(recovery_ratio_covid), if_all(all_of(control_cols_covid), ~ !is.na(.)))
complete_both <- spine %>%
  filter(fips %in% complete_2008$fips, fips %in% complete_covid$fips)

message("\n=== SURVIVING COUNTIES WITH COMPLETE DATA ===")
message(sprintf("2008 crisis (outcome + all 5 controls):  %d / %d counties", nrow(complete_2008), nrow(spine)))
message(sprintf("COVID crisis (outcome + all 5 controls): %d / %d counties", nrow(complete_covid), nrow(spine)))
message(sprintf("Complete for BOTH crises (intersection):  %d / %d counties", nrow(complete_both), nrow(spine)))

write_csv(spine, "spine_data.csv")
message("\nWrote spine_data.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
