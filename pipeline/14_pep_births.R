# 14_pep_births.R
#
# Predictor bucket 4: BIRTHS. Birth rate (per 1,000 population) at 2007 and
# 2019, for both counties and states.
#
# ORIGINAL PLAN was CDC WONDER natality -- confirmed unusable for this
# pipeline: WONDER's scripted API rejects ANY request that groups results by
# a location field (tested live against https://wonder.cdc.gov/controller/
# datarequest/D66 -- even a "clean" request with no location variable at all
# was rejected, because the Births measure forces a location check
# server-side). This is a documented, blanket CDC confidentiality policy on
# the API itself, not the county-suppression issue originally expected --
# state-level requests are blocked identically to county-level ones. WONDER
# natality is only reachable via the interactive web UI, which can't be
# scripted from here. User-approved substitute, chosen over a manual
# WONDER export:
#
#   - Census Population Estimates Program "components of change" files,
#     the same file family already used for total_population in
#     03_controls.R / 08_state_controls.R. These are Census's own vital-
#     statistics-derived birth counts (same underlying NCHS registrations
#     WONDER draws from) bundled directly into the population-estimates
#     pipeline, at both county (SUMLEV 050) and state (SUMLEV 040) level,
#     with a BIRTHS<year> count and a pre-computed RBIRTH<year> rate
#     (per 1,000 population) already included -- no separate denominator
#     needed.
#   - 2007: Vintage 2009 file (co-est2009-alldata.csv, covers 2000-2009),
#     a DIFFERENT vintage than the one used for total_population_2007 in
#     03_controls.R (which reads the later-revised co-est00int-tot.csv
#     intercensal file) -- the intercensal series only carries
#     age/sex/race breakdowns, not birth/death components, so this predictor
#     uses the original (non-intercensal-revised) Vintage 2009 postcensal
#     series instead. Population totals differ very slightly at the margin
#     from the controls' intercensal population as a result; the rate itself
#     (RBIRTH) is unaffected since Census computes it from its own
#     within-vintage population base.
#   - 2019: reuses the already-cached co-est2019-alldata.csv (same file
#     03_controls.R pulls total_population_2019 from).
#   - Full coverage, no suppression -- unlike raw WONDER, no small-county
#     values are dropped, so there's no "flag don't drop" caveat needed here.

library(dplyr)
library(readr)
library(httr)

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"

# ---- 2007: Vintage 2009 postcensal county/state file (has BIRTHS2007) ----
pep09_path <- "raw_data/co-est2009-alldata.csv"
if (!file.exists(pep09_path)) {
  message("Downloading Census PEP Vintage 2009 components-of-change file...")
  GET("https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/totals/co-est2009-alldata.csv",
      add_headers(`User-Agent` = ua), write_disk(pep09_path, overwrite = TRUE), timeout(60))
}
pep09 <- read_csv(pep09_path, show_col_types = FALSE, col_types = cols(.default = "c"))

births_county_2007 <- pep09 %>%
  filter(SUMLEV == "050") %>%
  transmute(fips = paste0(sprintf("%02s", STATE), sprintf("%03s", COUNTY)),
            birth_rate_2007 = as.numeric(RBIRTH2007))

births_state_2007 <- pep09 %>%
  filter(SUMLEV == "040") %>%
  transmute(state_fips = sprintf("%02s", STATE), birth_rate_2007 = as.numeric(RBIRTH2007))

message("Births 2007: ", nrow(births_county_2007), " counties, ", nrow(births_state_2007), " states.")

# ---- 2019: reuse cached Vintage 2019 file (already pulled by 03_controls.R) ----
pep19_path <- "raw_data/co-est2019-alldata.csv"
if (!file.exists(pep19_path)) {
  message("Downloading Census PEP 2010s components-of-change file...")
  GET("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
      add_headers(`User-Agent` = ua), write_disk(pep19_path, overwrite = TRUE), timeout(60))
}
pep19 <- read_csv(pep19_path, show_col_types = FALSE, col_types = cols(.default = "c"),
                   locale = locale(encoding = "latin1"))

births_county_2019 <- pep19 %>%
  filter(SUMLEV == "050") %>%
  transmute(fips = paste0(sprintf("%02s", STATE), sprintf("%03s", COUNTY)),
            birth_rate_2019 = as.numeric(RBIRTH2019))

births_state_2019 <- pep19 %>%
  filter(SUMLEV == "040") %>%
  transmute(state_fips = sprintf("%02s", STATE), birth_rate_2019 = as.numeric(RBIRTH2019))

message("Births 2019: ", nrow(births_county_2019), " counties, ", nrow(births_state_2019), " states.")

county_births <- births_county_2007 %>% full_join(births_county_2019, by = "fips")
state_births <- births_state_2007 %>% full_join(births_state_2019, by = "state_fips")

saveRDS(county_births, "raw_data/14_births_county.rds")
saveRDS(state_births, "raw_data/14_births_state.rds")
message("Saved 14_births_county.rds and 14_births_state.rds.")
