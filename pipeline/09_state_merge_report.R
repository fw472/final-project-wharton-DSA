# 09_state_merge_report.R
#
# Joins the state outcome with the state controls and writes state_spine.csv
# -- a SEPARATE file from county_spine.csv (formerly spine_data.csv). States
# and counties are never merged into one table: a state is its counties
# summed, so a model mixing both would double-count.

library(dplyr)
library(readr)

outcome <- readRDS("raw_data/07_state_outcome.rds")
controls_2008 <- readRDS("raw_data/08_state_controls_2008.rds")
controls_covid <- readRDS("raw_data/08_state_controls_covid.rds")

message("\n=== STATE JOIN REPORT ===")
message("Starting population (states/DC with a BLS-derived employment record): ", nrow(outcome))

join_and_report <- function(data, new_data, label) {
  n_matched <- sum(data$state_fips %in% new_data$state_fips)
  n_missing <- nrow(data) - n_matched
  message(sprintf("%-30s %d/%d states matched (%d missing)",
                   label, n_matched, nrow(data), n_missing))
  left_join(data, new_data, by = "state_fips")
}

spine <- outcome
spine <- join_and_report(spine, controls_2008, "Controls 2007 (pre-2008)")
spine <- join_and_report(spine, controls_covid, "Controls 2019 (pre-COVID)")

message("\nFinal state spine: ", nrow(spine), " states/DC, ", ncol(spine), " columns.")

write_csv(spine, "state_spine.csv")
message("Wrote state_spine.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
