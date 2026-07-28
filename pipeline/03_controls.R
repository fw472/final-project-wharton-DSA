# 03_controls.R
#
# Shared pre-crisis controls: poverty rate, median household income, total
# population, population density, USDA rural-urban continuum code. Pulled
# TWICE, once per crisis, using values from the year immediately before
# each crisis hit (2007 for 2008, 2019 for COVID) -- never a value that
# could already reflect the crisis itself.
#
# Sources (all direct CSV/XLS pulls, no ACS 5-year needed -- avoids the
# non-overlapping-years issue entirely for this spine):
#   - Poverty rate + median HH income: Census SAIPE timeseries API (annual,
#     point-in-time-by-design -- built for exactly this).
#   - Total population: Census Population Estimates Program (PEP) annual
#     county estimates. Two different vintage files are needed because
#     Census splits PEP into decade files: the 2000-2010 intercensal file
#     (has POPESTIMATE2007) and the 2010-2019 file (has POPESTIMATE2019).
#   - RUCC: USDA ERS rural-urban continuum codes, 2003 vintage for the 2008
#     crisis and 2013 vintage for COVID -- both PRE-date each crisis, and
#     both use the same (pre-2022) Connecticut county FIPS as the BLS LAUS
#     panel, so no CT geography mismatch here (unlike RUCC 2023).
#   - Land area (for density): Census Gazetteer file, any recent vintage --
#     land area doesn't meaningfully change year to year.

library(dplyr)
library(readr)
library(readxl)
library(httr)
library(jsonlite)

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
census_key <- Sys.getenv("CENSUS_API_KEY")

# ---- SAIPE: poverty rate + median household income, 2007 and 2019 ----
pull_saipe <- function(year) {
  message("Pulling SAIPE ", year, " poverty rate and median household income...")
  url <- paste0(
    "https://api.census.gov/data/timeseries/poverty/saipe",
    "?get=NAME,SAEPOVRTALL_PT,SAEMHI_PT&for=county:*&in=state:*&YEAR=", year,
    "&key=", census_key
  )
  raw <- fromJSON(url)
  d <- as_tibble(raw[-1, ], .name_repair = "minimal")
  names(d) <- raw[1, ]
  d %>%
    transmute(
      fips = paste0(state, county),
      poverty_rate = as.numeric(SAEPOVRTALL_PT),
      median_hh_income = as.numeric(SAEMHI_PT)
    )
}

saipe_2007 <- pull_saipe(2007)
saipe_2019 <- pull_saipe(2019)
stopifnot(all(nchar(saipe_2007$fips) == 5), all(nchar(saipe_2019$fips) == 5))
message("SAIPE 2007: ", nrow(saipe_2007), " counties. SAIPE 2019: ", nrow(saipe_2019), " counties.")

# ---- Census PEP: total population, 2007 and 2019 ----
message("Pulling Census PEP population estimates (2000s + 2010s vintage files)...")

pep_path_2000s <- "raw_data/co-est00int-tot.csv"
if (!file.exists(pep_path_2000s)) {
  GET("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv",
      add_headers(`User-Agent` = ua), write_disk(pep_path_2000s, overwrite = TRUE), timeout(60))
}
pep_2000s <- read_csv(pep_path_2000s, show_col_types = FALSE)

pop_2007 <- pep_2000s %>%
  filter(SUMLEV == 50) %>%
  transmute(
    fips = paste0(sprintf("%02d", STATE), sprintf("%03d", COUNTY)),
    total_population = POPESTIMATE2007
  )

pep_path_2010s <- "raw_data/co-est2019-alldata.csv"
if (!file.exists(pep_path_2010s)) {
  GET("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
      add_headers(`User-Agent` = ua), write_disk(pep_path_2010s, overwrite = TRUE), timeout(60))
}
pep_2010s <- read_csv(pep_path_2010s, show_col_types = FALSE, locale = locale(encoding = "latin1"))

pop_2019 <- pep_2010s %>%
  filter(SUMLEV == "050") %>%
  transmute(
    fips = paste0(sprintf("%02s", STATE), sprintf("%03s", COUNTY)),
    total_population = POPESTIMATE2019
  )

stopifnot(all(nchar(pop_2007$fips) == 5), all(nchar(pop_2019$fips) == 5))
message("PEP 2007: ", nrow(pop_2007), " counties. PEP 2019: ", nrow(pop_2019), " counties.")

# ---- USDA ERS RUCC: 2003 vintage and 2013 vintage ----
message("Pulling USDA ERS RUCC 2003 and 2013 vintages...")

rucc_2003_path <- "raw_data/rucc2003.xls"
if (!file.exists(rucc_2003_path)) {
  GET("https://www.ers.usda.gov/media/5770/2003-rural-urban-continuum-codes.xls?v=79742",
      add_headers(`User-Agent` = ua), write_disk(rucc_2003_path, overwrite = TRUE), timeout(30))
}
rucc_2003 <- read_excel(rucc_2003_path) %>%
  transmute(
    fips = sprintf("%05s", `FIPS Code`),
    rucc_2003 = as.integer(`2003 Rural-urban Continuum Code`)
  )

rucc_2013_path <- "raw_data/rucc2013.xls"
if (!file.exists(rucc_2013_path)) {
  GET("https://www.ers.usda.gov/media/5769/2013-rural-urban-continuum-codes.xls?v=55530",
      add_headers(`User-Agent` = ua), write_disk(rucc_2013_path, overwrite = TRUE), timeout(30))
}
rucc_2013 <- read_excel(rucc_2013_path) %>%
  transmute(
    fips = sprintf("%05s", FIPS),
    rucc_2013 = as.integer(RUCC_2013)
  )

stopifnot(all(nchar(rucc_2003$fips) == 5), all(nchar(rucc_2013$fips) == 5))
message("RUCC 2003: ", nrow(rucc_2003), " counties. RUCC 2013: ", nrow(rucc_2013), " counties.")

# ---- Census Gazetteer: land area for population density (static across both crises) ----
message("Pulling Census Gazetteer for land area...")

gaz_zip <- "raw_data/gaz2024.zip"
gaz_dir <- "raw_data/gaz2024"
if (!file.exists(gaz_zip)) {
  GET("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/2024_Gaz_counties_national.zip",
      add_headers(`User-Agent` = ua), write_disk(gaz_zip, overwrite = TRUE), timeout(30))
}
if (!dir.exists(gaz_dir)) unzip(gaz_zip, exdir = gaz_dir)
gaz_file <- list.files(gaz_dir, pattern = "\\.txt$", full.names = TRUE)[1]

gazetteer <- read_tsv(gaz_file, show_col_types = FALSE,
                       col_types = cols(GEOID = col_character())) %>%
  transmute(fips = sprintf("%05s", GEOID), land_area_sqmi = ALAND_SQMI)

stopifnot(all(nchar(gazetteer$fips) == 5))
message("Gazetteer: ", nrow(gazetteer), " counties.")

# ---- assemble the two control tables ----
controls_2008 <- saipe_2007 %>%
  full_join(pop_2007, by = "fips") %>%
  full_join(rucc_2003, by = "fips") %>%
  full_join(gazetteer, by = "fips") %>%
  mutate(population_density = total_population / land_area_sqmi) %>%
  select(-land_area_sqmi) %>%
  rename_with(~ paste0(., "_2007"), -fips)

controls_covid <- saipe_2019 %>%
  full_join(pop_2019, by = "fips") %>%
  full_join(rucc_2013, by = "fips") %>%
  full_join(gazetteer, by = "fips") %>%
  mutate(population_density = total_population / land_area_sqmi) %>%
  select(-land_area_sqmi) %>%
  rename_with(~ paste0(., "_2019"), -fips)

message("\nControls 2007 (pre-2008 crisis): ", nrow(controls_2008), " counties.")
message("Controls 2019 (pre-COVID crisis): ", nrow(controls_covid), " counties.")

saveRDS(controls_2008, "raw_data/03_controls_2008.rds")
saveRDS(controls_covid, "raw_data/03_controls_covid.rds")
