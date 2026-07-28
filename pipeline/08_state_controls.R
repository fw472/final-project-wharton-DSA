# 08_state_controls.R
#
# Same five controls as the county spine, at state level, pre-crisis
# (2007 and 2019). Reuses files already downloaded for the county build
# wherever possible -- only SAIPE needs a fresh pull, since poverty rate
# and median household income are not properly additive across counties
# (they need their own directly-estimated state value, not a derived sum).
#
#   - poverty_rate, median_hh_income: Census SAIPE timeseries API, pulled
#     directly at state level (for=state:*) -- NOT aggregated from county
#     SAIPE, for the same reason state GDP isn't "sum of county GDP rates."
#   - total_population: Census PEP, same cached CSVs as the county build,
#     just reading the state-level rows (SUMLEV 040) instead of county
#     rows (SUMLEV 050) this time.
#   - population_density: state population / state land area, where land
#     area is SUMMED from the cached county Gazetteer file -- land area is
#     exactly additive (it's a geographic partition), unlike income/poverty.
#   - rucc-equivalent -> pct_rural: no state-level RUCC exists (RUCC is a
#     county classification), so this uses the standard OMB metro/nonmetro
#     split already embedded in the RUCC codes we already pulled for
#     counties: pct_rural = population-weighted share of the state's people
#     living in a NONMETRO county (RUCC 4-9) vs a metro county (RUCC 1-3).
#     Uses the SAME RUCC vintage already locked for county controls -- 2003
#     for the 2008 crisis, 2013 for COVID -- so state and county controls
#     stay on identical pre-crisis vintages.

library(dplyr)
library(readr)
library(httr)
library(jsonlite)

census_key <- Sys.getenv("CENSUS_API_KEY")

# ---- SAIPE: state-level poverty rate + median household income ----
pull_saipe_state <- function(year) {
  message("Pulling state-level SAIPE ", year, "...")
  url <- paste0(
    "https://api.census.gov/data/timeseries/poverty/saipe",
    "?get=NAME,SAEPOVRTALL_PT,SAEMHI_PT&for=state:*&YEAR=", year,
    "&key=", census_key
  )
  raw <- fromJSON(url)
  d <- as_tibble(raw[-1, ], .name_repair = "minimal")
  names(d) <- raw[1, ]
  d %>%
    transmute(
      state_fips = sprintf("%02s", state),
      poverty_rate = as.numeric(SAEPOVRTALL_PT),
      median_hh_income = as.numeric(SAEMHI_PT)
    )
}

saipe_state_2007 <- pull_saipe_state(2007)
saipe_state_2019 <- pull_saipe_state(2019)
message("State SAIPE 2007: ", nrow(saipe_state_2007), " states. State SAIPE 2019: ", nrow(saipe_state_2019), " states.")

# ---- Census PEP: state-level population (reusing cached county-build files) ----
message("Reading state-level rows from cached PEP files...")

pep_2000s <- read_csv("raw_data/co-est00int-tot.csv", show_col_types = FALSE)
pop_state_2007 <- pep_2000s %>%
  filter(as.integer(SUMLEV) == 40) %>%
  transmute(state_fips = sprintf("%02d", as.integer(STATE)), total_population = POPESTIMATE2007)

pep_2010s <- read_csv("raw_data/co-est2019-alldata.csv", show_col_types = FALSE, locale = locale(encoding = "latin1"))
pop_state_2019 <- pep_2010s %>%
  filter(as.integer(SUMLEV) == 40) %>%
  transmute(state_fips = sprintf("%02d", as.integer(STATE)), total_population = POPESTIMATE2019)

message("State PEP 2007: ", nrow(pop_state_2007), " states. State PEP 2019: ", nrow(pop_state_2019), " states.")

# ---- land area: sum county Gazetteer land area up to state ----
gaz_dir <- "raw_data/gaz2024"
gaz_file <- list.files(gaz_dir, pattern = "\\.txt$", full.names = TRUE)[1]
gazetteer <- read_tsv(gaz_file, show_col_types = FALSE,
                       col_types = cols(GEOID = col_character())) %>%
  transmute(fips = sprintf("%05s", GEOID), land_area_sqmi = ALAND_SQMI)

land_area_state <- gazetteer %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  group_by(state_fips) %>%
  summarise(land_area_sqmi = sum(land_area_sqmi), .groups = "drop")

# ---- pct_rural: population-weighted nonmetro share, from cached county controls ----
county_controls_2008 <- readRDS("raw_data/03_controls_2008.rds")
county_controls_covid <- readRDS("raw_data/03_controls_covid.rds")

pct_rural_2007 <- county_controls_2008 %>%
  filter(!is.na(total_population_2007), !is.na(rucc_2003_2007)) %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  group_by(state_fips) %>%
  summarise(
    pct_rural = 100 * sum(total_population_2007[rucc_2003_2007 >= 4]) / sum(total_population_2007),
    .groups = "drop"
  )

pct_rural_2019 <- county_controls_covid %>%
  filter(!is.na(total_population_2019), !is.na(rucc_2013_2019)) %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  group_by(state_fips) %>%
  summarise(
    pct_rural = 100 * sum(total_population_2019[rucc_2013_2019 >= 4]) / sum(total_population_2019),
    .groups = "drop"
  )

# ---- assemble ----
# LEFT joins from saipe_state (the authoritative 50-states-+-DC list from
# the Census API) on purpose -- the county-derived pieces (land area,
# pct_rural) are aggregated from a county table that also includes Puerto
# Rico municipios (a separate, already-locked county-spine decision). A
# full_join would let PR leak back in as an unwanted 52nd "state" row.
controls_state_2008 <- saipe_state_2007 %>%
  left_join(pop_state_2007, by = "state_fips") %>%
  left_join(land_area_state, by = "state_fips") %>%
  left_join(pct_rural_2007, by = "state_fips") %>%
  mutate(population_density = total_population / land_area_sqmi) %>%
  select(-land_area_sqmi) %>%
  rename_with(~ paste0(., "_2007"), -state_fips)

controls_state_covid <- saipe_state_2019 %>%
  left_join(pop_state_2019, by = "state_fips") %>%
  left_join(land_area_state, by = "state_fips") %>%
  left_join(pct_rural_2019, by = "state_fips") %>%
  mutate(population_density = total_population / land_area_sqmi) %>%
  select(-land_area_sqmi) %>%
  rename_with(~ paste0(., "_2019"), -state_fips)

message("\nState controls 2007 (pre-2008 crisis): ", nrow(controls_state_2008), " states.")
message("State controls 2019 (pre-COVID crisis): ", nrow(controls_state_covid), " states.")

saveRDS(controls_state_2008, "raw_data/08_state_controls_2008.rds")
saveRDS(controls_state_covid, "raw_data/08_state_controls_covid.rds")
