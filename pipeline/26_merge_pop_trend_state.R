# 26_merge_pop_trend_state.R
#
# Joins the pre-crisis population-trend control onto state_spine.csv.

library(dplyr)
library(readr)

spine <- read_csv("state_spine.csv", show_col_types = FALSE, col_types = cols(state_fips = "c"))
pop_trend <- readRDS("raw_data/24_pop_trend_state.rds")

n_matched <- sum(spine$state_fips %in% pop_trend$state_fips)
message(sprintf("Population trend control:      %d/%d states matched (%d missing)",
                 n_matched, nrow(spine), nrow(spine) - n_matched))

spine <- left_join(spine, pop_trend, by = "state_fips")

message("Final state spine: ", nrow(spine), " states/DC, ", ncol(spine), " columns.")
write_csv(spine, "state_spine.csv")
message("Wrote state_spine.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
