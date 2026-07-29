# 24_pop_trend.R
#
# CONTROL (not a studied predictor): pre-crisis POPULATION TREND, separating
# long-run structural decline (e.g. rural depopulation) from crisis response
# -- otherwise the migration predictor and the recovery outcome could both
# just be picking up "this county was already shrinking" rather than
# anything crisis-specific.
#
#   - Windows chosen to be symmetric (7 years each) and to end strictly
#     BEFORE their crisis: 2000-2007 for the 2008 side, 2012-2019 for the
#     COVID side. Both endpoints are pre-crisis years -- no leakage.
#   - Both windows reuse Census PEP files already cached by earlier scripts
#     in this pipeline (03_controls.R / 14_pep_births.R) -- no new download:
#       * 2000-2007: raw_data/co-est00int-tot.csv (2000-2010 intercensal),
#         POPESTIMATE2000 and POPESTIMATE2007.
#       * 2012-2019: raw_data/co-est2019-alldata.csv (2010s vintage),
#         POPESTIMATE2012 and POPESTIMATE2019.
#   - Metric: annualized growth rate (CAGR), not raw total % change, so the
#     control is on a consistent "% per year" scale -- pop_trend_2007 =
#     100 * ((POPESTIMATE2007/POPESTIMATE2000)^(1/7) - 1), same form for
#     pop_trend_2019 over 2012-2019.
#   - State values come from the SAME cached files' own state-level rows
#     (SUMLEV 040) -- these are Census's own state totals, not a sum of
#     counties, same source 08_state_controls.R already uses for state
#     population.

library(dplyr)
library(readr)

pep_2000s <- read_csv("raw_data/co-est00int-tot.csv", show_col_types = FALSE,
                       col_types = cols(.default = "c"))
pep_2010s <- read_csv("raw_data/co-est2019-alldata.csv", show_col_types = FALSE,
                       col_types = cols(.default = "c"), locale = locale(encoding = "latin1"))

# ---- COUNTY ----
county_pop_2000_2007 <- pep_2000s %>%
  filter(SUMLEV == "50") %>%
  transmute(
    fips = paste0(sprintf("%02d", as.integer(STATE)), sprintf("%03d", as.integer(COUNTY))),
    pop_2000 = as.numeric(POPESTIMATE2000),
    pop_2007 = as.numeric(POPESTIMATE2007)
  ) %>%
  mutate(pop_trend_2007 = 100 * ((pop_2007 / pop_2000)^(1/7) - 1)) %>%
  select(fips, pop_trend_2007)

county_pop_2012_2019 <- pep_2010s %>%
  filter(SUMLEV == "050") %>%
  transmute(
    fips = paste0(sprintf("%02s", STATE), sprintf("%03s", COUNTY)),
    pop_2012 = as.numeric(POPESTIMATE2012),
    pop_2019 = as.numeric(POPESTIMATE2019)
  ) %>%
  mutate(pop_trend_2019 = 100 * ((pop_2019 / pop_2012)^(1/7) - 1)) %>%
  select(fips, pop_trend_2019)

county_pop_trend <- county_pop_2000_2007 %>% full_join(county_pop_2012_2019, by = "fips")
message("Pop trend (county): ", sum(!is.na(county_pop_trend$pop_trend_2007)), " with 2007 trend, ",
        sum(!is.na(county_pop_trend$pop_trend_2019)), " with 2019 trend, out of ", nrow(county_pop_trend), " rows.")

# ---- STATE (own SUMLEV-040 rows in the same files, not summed from counties) ----
state_pop_2000_2007 <- pep_2000s %>%
  filter(SUMLEV == "40") %>%
  transmute(
    state_fips = sprintf("%02d", as.integer(STATE)),
    pop_2000 = as.numeric(POPESTIMATE2000),
    pop_2007 = as.numeric(POPESTIMATE2007)
  ) %>%
  mutate(pop_trend_2007 = 100 * ((pop_2007 / pop_2000)^(1/7) - 1)) %>%
  select(state_fips, pop_trend_2007)

state_pop_2012_2019 <- pep_2010s %>%
  filter(SUMLEV == "040") %>%
  transmute(
    state_fips = sprintf("%02d", as.integer(STATE)),
    pop_2012 = as.numeric(POPESTIMATE2012),
    pop_2019 = as.numeric(POPESTIMATE2019)
  ) %>%
  mutate(pop_trend_2019 = 100 * ((pop_2019 / pop_2012)^(1/7) - 1)) %>%
  select(state_fips, pop_trend_2019)

state_pop_trend <- state_pop_2000_2007 %>% full_join(state_pop_2012_2019, by = "state_fips")
message("Pop trend (state): ", nrow(state_pop_trend), " states/DC.")

saveRDS(county_pop_trend, "raw_data/24_pop_trend_county.rds")
saveRDS(state_pop_trend, "raw_data/24_pop_trend_state.rds")
message("Saved 24_pop_trend_county.rds and 24_pop_trend_state.rds.")
