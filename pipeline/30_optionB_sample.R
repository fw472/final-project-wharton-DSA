# 30_optionB_sample.R
#
# Builds the Option B complete-case modeling samples (2008 side, COVID side)
# and reports the final sample size + per-sector industry-mix coverage, so
# it's clear this is the ~735-750 county sample (6 focus sectors), not the
# ~51-56 county collapse that requiring all 19 sectors produced earlier.
# Read-only relative to spine_data.csv -- writes new modeling-sample files
# under raw_data/ (gitignored, regenerable) for the modeling scripts to load.

library(dplyr)
library(readr)

source("pipeline/29_predictor_set_optionB.R")

county <- read_csv("spine_data.csv", show_col_types = FALSE, col_types = cols(fips = "c"))

message("============================================================")
message("OPTION B -- final sample size")
message("============================================================")

sample_2008 <- county %>%
  filter(!is.na(recovery_ratio_2008),
         if_all(all_of(CONTROLS_COUNTY_2008), ~ !is.na(.)),
         if_all(all_of(STUDIED_PREDICTORS_2007_OPTIONB), ~ !is.na(.)))
sample_covid <- county %>%
  filter(!is.na(recovery_ratio_covid),
         if_all(all_of(CONTROLS_COUNTY_COVID), ~ !is.na(.)),
         if_all(all_of(STUDIED_PREDICTORS_2019_OPTIONB), ~ !is.na(.)))
sample_both <- county %>% filter(fips %in% sample_2008$fips, fips %in% sample_covid$fips)

message(sprintf("2008 side:    %d / %d counties", nrow(sample_2008), nrow(county)))
message(sprintf("COVID side:   %d / %d counties", nrow(sample_covid), nrow(county)))
message(sprintf("BOTH crises:  %d / %d counties", nrow(sample_both), nrow(county)))

message("\n--- per-predictor non-missing counts (full spine, for reference) ---")
for (col in c(CONTROLS_COUNTY_2008, STUDIED_PREDICTORS_2007_OPTIONB)) {
  message(sprintf("  %-32s %4d / %4d non-missing", col, sum(!is.na(county[[col]])), nrow(county)))
}
for (col in c(CONTROLS_COUNTY_COVID, STUDIED_PREDICTORS_2019_OPTIONB)) {
  message(sprintf("  %-32s %4d / %4d non-missing", col, sum(!is.na(county[[col]])), nrow(county)))
}

message("\n--- industry-mix (6 focus sectors) coverage confirmation ---")
for (sec in FOCUS_SECTORS) {
  c07 <- paste0("share_", sec, "_2007"); c19 <- paste0("share_", sec, "_2019")
  message(sprintf("  %-24s 2007: %4d / %4d non-missing (%.1f%%)   2019: %4d / %4d non-missing (%.1f%%)",
                   sec, sum(!is.na(county[[c07]])), nrow(county), 100*mean(!is.na(county[[c07]])),
                   sum(!is.na(county[[c19]])), nrow(county), 100*mean(!is.na(county[[c19]]))))
}
message(sprintf("\nConfirmed: this is the 6-focus-sector sample (%d counties both crises), NOT the ~51-56",
                 nrow(sample_both)))
message("county collapse that requiring all 19 sectors produced in the earlier industry-mix test.")

# ---- wealth-band split within Option B's final sample (for the stress test) ----
message("\n--- wealth_band distribution within Option B samples ---")
message("2008 side:"); print(table(sample_2008$wealth_band_2008, useNA = "ifany"))
message("COVID side:"); print(table(sample_covid$wealth_band_covid, useNA = "ifany"))

saveRDS(sample_2008, "raw_data/30_optionB_sample_2008.rds")
saveRDS(sample_covid, "raw_data/30_optionB_sample_covid.rds")
saveRDS(sample_both, "raw_data/30_optionB_sample_both.rds")
message("\nSaved raw_data/30_optionB_sample_{2008,covid,both}.rds")
