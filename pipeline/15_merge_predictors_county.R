# 15_merge_predictors_county.R
#
# Joins the four predictor buckets (self-employment, migration, education,
# births) onto spine_data.csv, and adds wealth_band -- a tercile split of
# median_hh_income computed SEPARATELY per crisis year (2007 income for the
# 2008 band, 2019 income for the COVID band), since median_hh_income itself
# isn't comparable in raw dollars across the two years. Counties only --
# states aren't banded (only 51 of them, terciles aren't meaningful).

library(dplyr)
library(readr)

spine <- read_csv("spine_data.csv", show_col_types = FALSE, col_types = cols(fips = "c"))

selfemp   <- readRDS("raw_data/11_selfemployment_county.rds")
migration <- readRDS("raw_data/12_migration_county.rds")
education <- readRDS("raw_data/13_education_county.rds")
births    <- readRDS("raw_data/14_births_county.rds")

join_and_report <- function(data, new_data, label) {
  n_matched <- sum(data$fips %in% new_data$fips)
  message(sprintf("%-30s %d/%d counties matched (%d missing)",
                   label, n_matched, nrow(data), nrow(data) - n_matched))
  left_join(data, new_data, by = "fips")
}

message("\n=== PREDICTOR JOIN REPORT (county) ===")
message("Starting population (spine_data.csv): ", nrow(spine))
spine <- join_and_report(spine, selfemp, "Self-employment (bucket 1)")
spine <- join_and_report(spine, migration, "Migration (bucket 2)")
spine <- join_and_report(spine, education, "Education (bucket 3)")
spine <- join_and_report(spine, births, "Births (bucket 4)")

# ---- wealth_band: tercile of median_hh_income, computed separately per crisis year ----
make_band <- function(x) {
  cuts <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  case_when(
    is.na(x) ~ NA_character_,
    x <= cuts[1] ~ "lower",
    x <= cuts[2] ~ "middle",
    TRUE ~ "higher"
  )
}

spine <- spine %>%
  mutate(
    wealth_band_2008 = make_band(median_hh_income_2007),
    wealth_band_covid = make_band(median_hh_income_2019)
  )

message("\nwealth_band_2008 distribution:")
print(table(spine$wealth_band_2008, useNA = "ifany"))
message("\nwealth_band_covid distribution:")
print(table(spine$wealth_band_covid, useNA = "ifany"))

message("\nFinal spine: ", nrow(spine), " counties, ", ncol(spine), " columns.")

write_csv(spine, "spine_data.csv")
message("Wrote spine_data.csv: ", nrow(spine), " rows, ", ncol(spine), " columns.")
