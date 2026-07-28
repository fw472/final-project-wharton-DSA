# 11_bea_selfemployment.R
#
# Predictor bucket 1: SELF-EMPLOYMENT. Proprietor employment share (proprietor
# employment / total employment) at 2007 and 2019, for both counties and
# states. Leakage-free and full-coverage by construction -- BEA employment
# tables cover every county and state every year.
#
#   - STATE: BEA Regional API, dataset "Regional", table SAINC30 ("Economic
#     Profile"), LineCode 240 = total employment, LineCode 260 = proprietors
#     employment (farm + nonfarm combined). Confirmed live: the API rejects
#     multiple LineCodes in one call ("Multiple parameter values were
#     supplied for a parameter that only allows single values"), so 240 and
#     260 are pulled in separate calls.
#   - COUNTY: as of the BEA's Nov 2024 revision, CAINC30 (the county
#     equivalent table) dropped all employment line items -- it now only
#     has income-dollar lines. County employment by industry/class only
#     survives in the legacy, no-longer-API-queryable table CAEMP25N, whose
#     bulk CSV is still hosted as a static file (confirmed reachable,
#     2001-2022 vintage, covers both 2007 and 2019). LineCode 10 = total
#     employment, LineCode 40 = proprietors employment (farm + nonfarm).
#     GeoFIPS in that file has a stray leading space baked into the field
#     and includes state/region/CSA/MSA aggregate rows mixed in with real
#     counties, so real counties are identified as 5-digit numeric FIPS
#     whose last 3 digits aren't "000".

library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
bea_key <- Sys.getenv("BEA_API_KEY")

# ---- STATE: SAINC30 via live BEA API ----
pull_sainc30 <- function(line_code) {
  message("Pulling SAINC30 LineCode ", line_code, " (state, 2007 & 2019)...")
  url <- paste0(
    "https://apps.bea.gov/api/data/?UserID=", bea_key,
    "&method=GetData&datasetname=Regional&TableName=SAINC30&LineCode=", line_code,
    "&GeoFips=STATE&Year=2007,2019&ResultFormat=JSON"
  )
  raw <- fromJSON(content(GET(url), as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  d <- raw$BEAAPI$Results$Data
  stopifnot(!is.null(d))
  as_tibble(d) %>%
    transmute(
      state_fips = substr(GeoFips, 1, 2),
      year = as.integer(TimePeriod),
      value = as.numeric(DataValue)
    )
}

state_total_emp <- pull_sainc30(240) %>% rename(total_employment = value)
state_prop_emp  <- pull_sainc30(260) %>% rename(proprietor_employment = value)

# SAINC30 GeoFips=STATE returns the 50 states + DC (fips 01-56) plus BEA's 8
# aggregate BEA-region codes (91-98, e.g. "New England", "Far West") mixed
# in -- those aren't states and must be dropped, same 51-code list used in
# 06_state_panel.R.
valid_state_fips <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17",
                       "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
                       "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
                       "47","48","49","50","51","53","54","55","56")
stopifnot(length(valid_state_fips) == 51)

state_selfemp <- state_total_emp %>%
  inner_join(state_prop_emp, by = c("state_fips", "year")) %>%
  filter(state_fips %in% valid_state_fips) %>%
  mutate(proprietor_share = proprietor_employment / total_employment) %>%
  select(state_fips, year, proprietor_share) %>%
  pivot_wider(names_from = year, values_from = proprietor_share,
              names_prefix = "proprietor_share_")

message("State proprietor share: ", nrow(state_selfemp), " states/DC.")

# ---- COUNTY: CAEMP25N legacy bulk file ----
caemp_zip <- "raw_data/CAEMP25N.zip"
caemp_dir <- "raw_data/CAEMP25N_extracted"
if (!file.exists(caemp_zip)) {
  message("Downloading CAEMP25N legacy bulk file (county employment, 2001-2022)...")
  GET("https://apps.bea.gov/regional/zip/CAEMP25N.zip",
      add_headers(`User-Agent` = ua), write_disk(caemp_zip, overwrite = TRUE), timeout(120))
}
if (!dir.exists(caemp_dir)) {
  unzip(caemp_zip, files = "CAEMP25N__ALL_AREAS_2001_2022.csv", exdir = caemp_dir)
}
caemp_file <- list.files(caemp_dir, pattern = "ALL_AREAS", full.names = TRUE)[1]

caemp_raw <- read_csv(caemp_file, col_types = cols(.default = "c"))

caemp_county <- caemp_raw %>%
  mutate(fips = trimws(GeoFIPS)) %>%
  filter(nchar(fips) == 5, substr(fips, 3, 5) != "000", LineCode %in% c("10", "40")) %>%
  transmute(fips, LineCode,
            emp_2007 = as.numeric(`2007`),
            emp_2019 = as.numeric(`2019`))

county_selfemp <- caemp_county %>%
  select(fips, LineCode, emp_2007, emp_2019) %>%
  pivot_longer(c(emp_2007, emp_2019), names_to = "year", values_to = "value") %>%
  mutate(year = sub("emp_", "", year)) %>%
  pivot_wider(names_from = LineCode, values_from = value, names_prefix = "line_") %>%
  mutate(proprietor_share = line_40 / line_10) %>%
  select(fips, year, proprietor_share) %>%
  pivot_wider(names_from = year, values_from = proprietor_share,
              names_prefix = "proprietor_share_")

message("County proprietor share: ", nrow(county_selfemp), " counties.")

saveRDS(county_selfemp, "raw_data/11_selfemployment_county.rds")
saveRDS(state_selfemp, "raw_data/11_selfemployment_state.rds")
message("Saved 11_selfemployment_county.rds and 11_selfemployment_state.rds.")
