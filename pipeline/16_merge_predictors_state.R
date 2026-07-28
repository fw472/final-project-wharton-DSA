# 16_merge_predictors_state.R
#
# Joins the four predictor buckets onto state_spine.csv. No wealth_band here
# -- that's a county-only column (banding 51 states into terciles isn't
# meaningful). Kept as a separate file from the county merge throughout --
# states and counties are never combined into one table.

library(dplyr)
library(readr)

spine <- read_csv("state_spine.csv", show_col_types = FALSE, col_types = cols(state_fips = "c"))

selfemp   <- readRDS("raw_data/11_selfemployment_state.rds")
migration <- readRDS("raw_data/12_migration_state.rds")
education <- readRDS("raw_data/13_education_state.rds")
births    <- readRDS("raw_data/14_births_state.rds")

join_and_report <- function(data, new_data, label) {
  n_matched <- sum(data$state_fips %in% new_data$state_fips)
  message(sprintf("%-30s %d/%d states matched (%d missing)",
                   label, n_matched, nrow(data), nrow(data) - n_matched))
  left_join(data, new_data, by = "state_fips")
}

message("\n=== PREDICTOR JOIN REPORT (state) ===")
message("Starting population (state_spine.csv): ", nrow(spine))
spine <- join_and_report(spine, selfemp, "Self-employment (bucket 1)")
spine <- join_and_report(spine, migration, "Migration (bucket 2)")
spine <- join_and_report(spine, education, "Education (bucket 3)")
spine <- join_and_report(spine, births, "Births (bucket 4)")

message("\nFinal state spine: ", nrow(spine), " states/DC, ", ncol(spine), " columns.")

write_csv(spine, "state_spine.csv")
message("Wrote state_spine.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
