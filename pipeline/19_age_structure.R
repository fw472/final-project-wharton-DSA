# 19_age_structure.R
#
# Predictor: AGE STRUCTURE. Working-age (25-64) share of total population,
# 2007 and 2019, for both counties and states. Same Census PEP family
# already used for population/births in this pipeline, just the age-detail
# series instead of the totals series.
#
#   - 2007: co-est00int-agesex-5yr.csv, a single national county-level file
#     (2000-2010 intercensal), SEX=0 (both sexes), 5-year AGEGRP bins.
#     AGEGRP 0 = total; AGEGRP 6-13 = the eight 5-year bins covering
#     25-29 through 60-64, i.e. "roughly 25-64". No state rows in this file
#     -- county age counts are purely additive, so state values are summed
#     up from counties (same logic 08_state_controls.R already uses to sum
#     county land area to state land area).
#   - 2019: cc-est2019-agesex-<state>.csv, one file per state (no single
#     national file exists at manageable size for this series -- the
#     national "alldata" file is 176MB with a full race/Hispanic-origin
#     breakdown we don't need). These files use a coded YEAR variable, not
#     a calendar year column; empirically confirmed YEAR=12 reproduces
#     total_population_2019 exactly (55,869 for Autauga County, AL, matching
#     the population already cached from co-est2019-alldata.csv in
#     03_controls.R) -- confirming YEAR=12 is 7/1/2019. Already has combined
#     AGE2544_TOT and AGE4564_TOT bands, so working-age = their sum. Same
#     per-state files carry only county rows (SUMLEV 050), so state values
#     are again summed from counties, not read from a separate row.

library(dplyr)
library(readr)
library(httr)

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
valid_state_fips <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17",
                       "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
                       "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
                       "47","48","49","50","51","53","54","55","56")

# ---- 2007: single national file, SEX=0, AGEGRP bins 6-13 = 25-64 ----
agesex07_path <- "raw_data/co-est00int-agesex-5yr.csv"
if (!file.exists(agesex07_path)) {
  message("Downloading Census PEP 2000s age-sex file (county, 5-year bins)...")
  GET("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-agesex-5yr.csv",
      add_headers(`User-Agent` = ua), write_disk(agesex07_path, overwrite = TRUE), timeout(120))
}
agesex07 <- read_csv(agesex07_path, show_col_types = FALSE, col_types = cols(.default = "c"))

agesex07_county <- agesex07 %>%
  filter(SUMLEV == "050", SEX == "0") %>%
  mutate(fips = paste0(sprintf("%02s", STATE), sprintf("%03s", COUNTY)),
         AGEGRP = as.integer(AGEGRP), pop = as.numeric(POPESTIMATE2007))

age_county_2007 <- agesex07_county %>%
  group_by(fips) %>%
  summarise(
    total_pop = pop[AGEGRP == 0],
    working_age_pop = sum(pop[AGEGRP >= 6 & AGEGRP <= 13]),
    .groups = "drop"
  ) %>%
  mutate(working_age_share_2007 = 100 * working_age_pop / total_pop) %>%
  select(fips, working_age_share_2007)

message("Age structure 2007: ", nrow(age_county_2007), " counties.")

age_state_2007 <- agesex07_county %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  filter(state_fips %in% valid_state_fips) %>%
  group_by(state_fips) %>%
  summarise(
    total_pop = sum(pop[AGEGRP == 0]),
    working_age_pop = sum(pop[AGEGRP >= 6 & AGEGRP <= 13]),
    .groups = "drop"
  ) %>%
  mutate(working_age_share_2007 = 100 * working_age_pop / total_pop) %>%
  select(state_fips, working_age_share_2007)

message("Age structure 2007: ", nrow(age_state_2007), " states (summed from counties).")

# ---- 2019: per-state files, YEAR==12 (confirmed = 7/1/2019) ----
pull_agesex19 <- function(state_fips) {
  path <- sprintf("raw_data/cc-est2019-agesex-%s.csv", state_fips)
  if (!file.exists(path)) {
    GET(sprintf("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-agesex-%s.csv", state_fips),
        add_headers(`User-Agent` = ua), write_disk(path, overwrite = TRUE), timeout(60))
  }
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(SUMLEV == "050", YEAR == "12") %>%
    transmute(
      fips = paste0(sprintf("%02s", STATE), sprintf("%03s", COUNTY)),
      total_pop = as.numeric(POPESTIMATE),
      working_age_pop = as.numeric(AGE2544_TOT) + as.numeric(AGE4564_TOT)
    )
}

message("Pulling Census PEP 2019 age-sex files (51 state files)...")
agesex19_county <- bind_rows(lapply(valid_state_fips, pull_agesex19))

age_county_2019 <- agesex19_county %>%
  mutate(working_age_share_2019 = 100 * working_age_pop / total_pop) %>%
  select(fips, working_age_share_2019)

message("Age structure 2019: ", nrow(age_county_2019), " counties.")

age_state_2019 <- agesex19_county %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  group_by(state_fips) %>%
  summarise(total_pop = sum(total_pop), working_age_pop = sum(working_age_pop), .groups = "drop") %>%
  mutate(working_age_share_2019 = 100 * working_age_pop / total_pop) %>%
  select(state_fips, working_age_share_2019)

message("Age structure 2019: ", nrow(age_state_2019), " states (summed from counties).")

county_age <- age_county_2007 %>% full_join(age_county_2019, by = "fips")
state_age  <- age_state_2007  %>% full_join(age_state_2019, by = "state_fips")

saveRDS(county_age, "raw_data/19_age_structure_county.rds")
saveRDS(state_age, "raw_data/19_age_structure_state.rds")
message("Saved 19_age_structure_county.rds and 19_age_structure_state.rds.")
