# 22_merge_predictors2_state.R
#
# Joins the three new predictors (pre-crisis unemployment, age structure,
# CBP industry mix) onto the already-built state_spine.csv.

library(dplyr)
library(readr)

spine <- read_csv("state_spine.csv", show_col_types = FALSE, col_types = cols(state_fips = "c"))

unemployment <- readRDS("raw_data/18_unemployment_state.rds")
age          <- readRDS("raw_data/19_age_structure_state.rds")
industry     <- readRDS("raw_data/20_industry_mix_state.rds")

join_and_report <- function(data, new_data, label) {
  n_matched <- sum(data$state_fips %in% new_data$state_fips)
  message(sprintf("%-30s %d/%d states matched (%d missing)",
                   label, n_matched, nrow(data), nrow(data) - n_matched))
  left_join(data, new_data, by = "state_fips")
}

message("\n=== PREDICTOR JOIN REPORT (state, round 2) ===")
message("Starting population (state_spine.csv): ", nrow(spine))
spine <- join_and_report(spine, unemployment, "Pre-crisis unemployment (bucket 5)")
spine <- join_and_report(spine, age, "Age structure (bucket 6)")
spine <- join_and_report(spine, industry, "Industry mix (bucket 7)")

message("\nFinal state spine: ", nrow(spine), " states/DC, ", ncol(spine), " columns.")

write_csv(spine, "state_spine.csv")
message("Wrote state_spine.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
