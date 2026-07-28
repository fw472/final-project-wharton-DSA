# 13_acs_education.R
#
# Predictor bucket 3: EDUCATION. % high-school-graduate-or-higher and %
# bachelor's-degree-or-higher among population 25+, at 2007 and 2019, for
# both counties and states. Single-year ACS (not 5-year) on purpose, so
# each value is a point-in-time read strictly before its crisis.
#
#   - The detailed table ID changes across the two years: 2007 uses B15002
#     ("Sex by Educational Attainment") because B15003 (the sex-combined
#     table) didn't exist yet -- confirmed live, B15003 first appears in the
#     2008 ACS 1-year vintage. B15002 requires summing the parallel
#     male (cells 011-018) and female (cells 028-035) attainment
#     categories; B15003 (2019) is already sex-combined (cells 017-025).
#   - ACS 1-year estimates are only published for geographies with
#     population >= 65,000 -- confirmed as a standing ACS design rule, not
#     something that changed between vintages. Counties below that
#     threshold simply won't appear in the API response (not an error).
#     States always clear the threshold, so state coverage is complete.
#   - Puerto Rico (state fips 72) is returned by both `for=state:*` and the
#     county pull (its larger municipios clear 65k) -- excluded here since
#     it isn't part of this project's US-states-and-counties scope.

library(dplyr)
library(readr)
library(jsonlite)

census_key <- Sys.getenv("CENSUS_API_KEY")

acs1_get <- function(year, vars, geo) {
  url <- paste0("https://api.census.gov/data/", year, "/acs/acs1?get=NAME,",
                 paste(vars, collapse = ","), "&", geo, "&key=", census_key)
  raw <- fromJSON(url)
  d <- as_tibble(raw[-1, , drop = FALSE], .name_repair = "minimal")
  names(d) <- raw[1, ]
  d
}

# ---- 2007: B15002 (male cells 011-018, female cells 028-035) ----
male_hs_plus   <- paste0("B15002_0", c(11:18), "E")
female_hs_plus <- paste0("B15002_0", c(28:35), "E")
male_ba_plus   <- paste0("B15002_0", c(15:18), "E")
female_ba_plus <- paste0("B15002_0", c(32:35), "E")
vars_2007 <- c("B15002_001E", male_hs_plus, female_hs_plus)

message("Pulling ACS 1-year 2007 (B15002) education, county...")
edu_county_2007_raw <- acs1_get(2007, vars_2007, "for=county:*&in=state:*")
message("Pulling ACS 1-year 2007 (B15002) education, state...")
edu_state_2007_raw <- acs1_get(2007, vars_2007, "for=state:*")

summarise_b15002 <- function(raw) {
  raw %>%
    mutate(across(all_of(vars_2007), as.numeric)) %>%
    rowwise() %>%
    mutate(
      total_25plus = B15002_001E,
      hs_plus = sum(c_across(all_of(c(male_hs_plus, female_hs_plus))), na.rm = TRUE),
      ba_plus = sum(c_across(all_of(c(male_ba_plus, female_ba_plus))), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    transmute(
      state, county = if ("county" %in% names(raw)) county else NA_character_,
      pct_hs_plus_2007 = 100 * hs_plus / total_25plus,
      pct_bachelors_plus_2007 = 100 * ba_plus / total_25plus
    )
}

edu_county_2007 <- summarise_b15002(edu_county_2007_raw) %>%
  filter(state != "72") %>%
  transmute(fips = paste0(state, county), pct_hs_plus_2007, pct_bachelors_plus_2007)

edu_state_2007 <- summarise_b15002(edu_state_2007_raw) %>%
  filter(state != "72") %>%
  transmute(state_fips = state, pct_hs_plus_2007, pct_bachelors_plus_2007)

message("Education 2007: ", nrow(edu_county_2007), " counties, ", nrow(edu_state_2007), " states.")

# ---- 2019: B15003 (single sex-combined sequence, cells 017-025) ----
hs_plus_2019 <- paste0("B15003_0", 17:25, "E")
ba_plus_2019 <- paste0("B15003_0", 22:25, "E")
vars_2019 <- c("B15003_001E", hs_plus_2019)

message("Pulling ACS 1-year 2019 (B15003) education, county...")
edu_county_2019_raw <- acs1_get(2019, vars_2019, "for=county:*&in=state:*")
message("Pulling ACS 1-year 2019 (B15003) education, state...")
edu_state_2019_raw <- acs1_get(2019, vars_2019, "for=state:*")

summarise_b15003 <- function(raw) {
  raw %>%
    mutate(across(all_of(vars_2019), as.numeric)) %>%
    rowwise() %>%
    mutate(
      total_25plus = B15003_001E,
      hs_plus = sum(c_across(all_of(hs_plus_2019)), na.rm = TRUE),
      ba_plus = sum(c_across(all_of(ba_plus_2019)), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    transmute(
      state, county = if ("county" %in% names(raw)) county else NA_character_,
      pct_hs_plus_2019 = 100 * hs_plus / total_25plus,
      pct_bachelors_plus_2019 = 100 * ba_plus / total_25plus
    )
}

edu_county_2019 <- summarise_b15003(edu_county_2019_raw) %>%
  filter(state != "72") %>%
  transmute(fips = paste0(state, county), pct_hs_plus_2019, pct_bachelors_plus_2019)

edu_state_2019 <- summarise_b15003(edu_state_2019_raw) %>%
  filter(state != "72") %>%
  transmute(state_fips = state, pct_hs_plus_2019, pct_bachelors_plus_2019)

message("Education 2019: ", nrow(edu_county_2019), " counties, ", nrow(edu_state_2019), " states.")

county_education <- edu_county_2007 %>% full_join(edu_county_2019, by = "fips")
state_education <- edu_state_2007 %>% full_join(edu_state_2019, by = "state_fips")

saveRDS(county_education, "raw_data/13_education_county.rds")
saveRDS(state_education, "raw_data/13_education_state.rds")
message("Saved 13_education_county.rds and 13_education_state.rds.")
