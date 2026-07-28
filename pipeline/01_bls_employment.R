# 01_bls_employment.R
#
# County-level annual-average EMPLOYMENT (not unemployment rate) from BLS
# LAUS, one laucnty##.xlsx file per year. LAUS is used instead of QCEW
# because LAUS reports every county every year with no small-county
# disclosure suppression -- QCEW suppresses small counties, which would
# force us to delete exactly the counties this project cares most about
# keeping.
#
# Years needed (see 02_outcome.R for why):
#   2008 crisis: peak search 2006-2008, trough search up to 2013 (widened
#                from an initial 2011 -- housing-bust states like NV/FL/GA
#                documented to bottom that late),
#                recovery = trough+2/trough+3 -> as late as 2013+3 = 2016.
#                Union: 2006-2016.
#   COVID crisis: peak search 2018-2019, trough search up to 2022 (widened
#                from an initial 2021),
#                recovery -> as late as 2022+3 = 2025.
#                Union: 2018-2025.
#
# HONESTY NOTE: bls.gov returns HTTP 403 to every automated request tried
# from this environment (curl, R httr, WebFetch -- all blocked, looks like a
# WAF blocking this network, not a real outage). This is untested from your
# own laptop/school network; try it first. If it fails here too, this script
# prints the exact URL + target filepath for every missing year in one batch
# so you can download them all manually, then re-run.

library(dplyr)
library(readxl)
library(httr)

years_needed <- sort(unique(c(2006:2016, 2018:2025)))

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"

bls_path_for <- function(year) sprintf("raw_data/laucnty%02d.xlsx", year %% 100)
bls_url_for  <- function(year) sprintf("https://www.bls.gov/lau/laucnty%02d.xlsx", year %% 100)

# ---- download pass ----
missing_years <- c()
for (yr in years_needed) {
  path <- bls_path_for(yr)
  if (file.exists(path)) next
  message("Attempting live download for ", yr, "...")
  resp <- tryCatch(
    GET(bls_url_for(yr), add_headers(`User-Agent` = ua),
        write_disk(path, overwrite = TRUE), timeout(30)),
    error = function(e) e
  )
  live_ok <- !inherits(resp, "error") && status_code(resp) == 200
  if (!live_ok) {
    if (file.exists(path)) file.remove(path)
    missing_years <- c(missing_years, yr)
  }
}

if (length(missing_years) > 0) {
  message("\nCould not download ", length(missing_years), " year(s) automatically. ",
          "Download each of these manually, then re-run this script:\n")
  for (yr in missing_years) {
    message(sprintf("  YEAR %d\n    URL:  %s\n    Save to:  %s\n",
                     yr, bls_url_for(yr), normalizePath(bls_path_for(yr), mustWork = FALSE)))
  }
  stop("Missing ", length(missing_years), " BLS year file(s) -- see paths above. ",
       "Peak/trough detection needs every year in the window, so the pipeline ",
       "stops here rather than silently using an incomplete window.")
}

# ---- parse pass ----
parse_one_year <- function(path, year) {
  preview <- read_excel(path, col_names = FALSE, n_max = 10)
  header_row <- which(apply(preview, 1, function(r) any(grepl("Unemployment Rate", r, ignore.case = TRUE))))
  if (length(header_row) == 0) {
    stop("Couldn't find a header row containing 'Unemployment Rate' in the first ",
         "10 rows of ", path, ". The file format may differ from what this ",
         "script expects for year ", year, " -- open it manually and check.")
  }
  raw <- read_excel(path, skip = header_row - 1)
  names(raw) <- make.unique(names(raw))

  out <- raw %>%
    rename_with(~ "state_fips", matches("State FIPS", ignore.case = TRUE)) %>%
    rename_with(~ "county_fips", matches("County FIPS", ignore.case = TRUE)) %>%
    rename_with(~ "county_name_raw", matches("County Name", ignore.case = TRUE)) %>%
    rename_with(~ "employed_raw", matches("^Employed$", ignore.case = TRUE)) %>%
    filter(!is.na(state_fips), !is.na(county_fips)) %>%
    transmute(
      fips = paste0(sprintf("%02s", state_fips), sprintf("%03s", county_fips)),
      county_name = county_name_raw,
      year = year,
      employed = as.numeric(employed_raw)
    )

  if (nrow(out) < 3000) {
    stop("Parsed BLS data for ", year, " failed a sanity check (fewer than ",
         "3000 counties). The column layout probably isn't what this script ",
         "assumed -- open ", path, " manually and check.")
  }
  out
}

message("\nParsing ", length(years_needed), " year(s) of BLS LAUS employment...")
panel <- bind_rows(lapply(years_needed, function(yr) parse_one_year(bls_path_for(yr), yr)))

stopifnot(all(nchar(panel$fips) == 5))
message("BLS employment panel: ", nrow(panel), " county-year rows, ",
        n_distinct(panel$fips), " counties, years ", min(panel$year), "-", max(panel$year), ".")

saveRDS(panel, "raw_data/01_bls_panel.rds")
