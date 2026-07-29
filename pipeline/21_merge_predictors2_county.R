# 21_merge_predictors2_county.R
#
# Joins the three new predictors (pre-crisis unemployment, age structure,
# CBP industry mix) onto the already-built spine_data.csv (which already has
# the first four predictor buckets from 15_merge_predictors_county.R).

library(dplyr)
library(readr)

spine <- read_csv("spine_data.csv", show_col_types = FALSE, col_types = cols(fips = "c"))

unemployment <- readRDS("raw_data/18_unemployment_county.rds")
age          <- readRDS("raw_data/19_age_structure_county.rds")
industry     <- readRDS("raw_data/20_industry_mix_county.rds")

join_and_report <- function(data, new_data, label) {
  n_matched <- sum(data$fips %in% new_data$fips)
  message(sprintf("%-30s %d/%d counties matched (%d missing)",
                   label, n_matched, nrow(data), nrow(data) - n_matched))
  left_join(data, new_data, by = "fips")
}

message("\n=== PREDICTOR JOIN REPORT (county, round 2) ===")
message("Starting population (spine_data.csv): ", nrow(spine))
spine <- join_and_report(spine, unemployment, "Pre-crisis unemployment (bucket 5)")
spine <- join_and_report(spine, age, "Age structure (bucket 6)")
spine <- join_and_report(spine, industry, "Industry mix (bucket 7)")

message("\nFinal spine: ", nrow(spine), " counties, ", ncol(spine), " columns.")

write_csv(spine, "spine_data.csv")
message("Wrote spine_data.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
