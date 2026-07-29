# 25_merge_pop_trend_county.R
#
# Joins the pre-crisis population-trend control onto spine_data.csv.

library(dplyr)
library(readr)

spine <- read_csv("spine_data.csv", show_col_types = FALSE, col_types = cols(fips = "c"))
pop_trend <- readRDS("raw_data/24_pop_trend_county.rds")

n_matched <- sum(spine$fips %in% pop_trend$fips)
message(sprintf("Population trend control:      %d/%d counties matched (%d missing)",
                 n_matched, nrow(spine), nrow(spine) - n_matched))

spine <- left_join(spine, pop_trend, by = "fips")

message("Final spine: ", nrow(spine), " counties, ", ncol(spine), " columns.")
write_csv(spine, "spine_data.csv")
message("Wrote spine_data.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
