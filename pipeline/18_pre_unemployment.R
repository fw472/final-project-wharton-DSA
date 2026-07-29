# 18_pre_unemployment.R
#
# Predictor: PRE-CRISIS UNEMPLOYMENT RATE, 2007 and 2019. No new download --
# 01_bls_employment.R only ever extracted the "Employed" column from the
# cached LAUS county files (raw_data/laucnty07.xlsx, laucnty19.xlsx), but
# those same files also carry Labor Force / Unemployed / Unemployment Rate
# columns that were never parsed. This script re-reads just those two
# already-cached files and pulls the Unemployment Rate column directly
# (confirmed present, e.g. Autauga County AL 2007 = 3.4%, matching the
# Employed count already in the spine for consistency).
#
#   - COUNTY: Unemployment Rate column, used as-is (a rate, not a raw count
#     -- "level" is read here as "the rate value for that year", the
#     standard usable predictor; comparable across counties of different
#     size, unlike a raw unemployed-persons count).
#   - STATE: no separate state-level pull -- summed from the SAME cached
#     county files (Labor Force and Unemployed are both additive counts),
#     consistent with how 06_state_panel.R derives the state employment
#     panel by summing the county BLS panel rather than pulling a parallel
#     state series. state_rate = 100 * sum(unemployed) / sum(labor force).

library(dplyr)
library(readxl)

valid_state_fips <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17",
                       "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
                       "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
                       "47","48","49","50","51","53","54","55","56")

parse_lau_unemployment <- function(path, year) {
  preview <- read_excel(path, col_names = FALSE, n_max = 10)
  header_row <- which(apply(preview, 1, function(r) any(grepl("Unemployment Rate", r, ignore.case = TRUE))))
  stopifnot(length(header_row) == 1)
  raw <- read_excel(path, skip = header_row - 1)
  names(raw) <- make.unique(names(raw))

  raw %>%
    rename_with(~ "state_fips", matches("State FIPS", ignore.case = TRUE)) %>%
    rename_with(~ "county_fips", matches("County FIPS", ignore.case = TRUE)) %>%
    rename_with(~ "labor_force_raw", matches("^Labor Force$", ignore.case = TRUE)) %>%
    rename_with(~ "unemployed_raw", matches("^Unemployed$", ignore.case = TRUE)) %>%
    rename_with(~ "rate_raw", matches("Unemployment Rate", ignore.case = TRUE)) %>%
    filter(!is.na(state_fips), !is.na(county_fips)) %>%
    transmute(
      fips = paste0(sprintf("%02s", state_fips), sprintf("%03s", county_fips)),
      state_fips = sprintf("%02s", state_fips),
      labor_force = as.numeric(labor_force_raw),
      unemployed = as.numeric(unemployed_raw),
      unemployment_rate = as.numeric(rate_raw),
      year = year
    )
}

message("Re-parsing cached LAUS county files for unemployment (2007, 2019)...")
lau_2007 <- parse_lau_unemployment("raw_data/laucnty07.xlsx", 2007)
lau_2019 <- parse_lau_unemployment("raw_data/laucnty19.xlsx", 2019)
stopifnot(nrow(lau_2007) > 3000, nrow(lau_2019) > 3000)

# ---- COUNTY ----
county_unemployment <- lau_2007 %>%
  transmute(fips, pre_unemployment_2007 = unemployment_rate) %>%
  full_join(lau_2019 %>% transmute(fips, pre_unemployment_2019 = unemployment_rate), by = "fips")

message("County pre-crisis unemployment: ", nrow(county_unemployment), " counties.")

# ---- STATE (summed from county-level Labor Force / Unemployed) ----
state_unemployment_2007 <- lau_2007 %>%
  filter(state_fips %in% valid_state_fips) %>%
  group_by(state_fips) %>%
  summarise(pre_unemployment_2007 = 100 * sum(unemployed, na.rm = TRUE) / sum(labor_force, na.rm = TRUE), .groups = "drop")

state_unemployment_2019 <- lau_2019 %>%
  filter(state_fips %in% valid_state_fips) %>%
  group_by(state_fips) %>%
  summarise(pre_unemployment_2019 = 100 * sum(unemployed, na.rm = TRUE) / sum(labor_force, na.rm = TRUE), .groups = "drop")

state_unemployment <- state_unemployment_2007 %>% full_join(state_unemployment_2019, by = "state_fips")
message("State pre-crisis unemployment: ", nrow(state_unemployment), " states/DC.")

saveRDS(county_unemployment, "raw_data/18_unemployment_county.rds")
saveRDS(state_unemployment, "raw_data/18_unemployment_state.rds")
message("Saved 18_unemployment_county.rds and 18_unemployment_state.rds.")
