# 20_cbp_industry_mix.R
#
# Predictor: INDUSTRY MIX. Each 2-digit NAICS sector's share of total county
# (or state) employment, 2007 and 2019, from Census County Business Patterns
# -- not BEA, since BEA's county employment-by-industry table (CAEMP25N) was
# discontinued and doesn't cover a full sector breakdown for these years
# anyway (see [[11_bea_selfemployment.R]] for the CAEMP25N/CAINC30 story).
#
#   - The sector-level NAICS variable name differs by CBP vintage: 2019 uses
#     NAICS2017 codes; 2007 uses NAICS2002 codes (confirmed live against
#     each year's own /variables.json -- 2007 CBP was NOT yet on a NAICS2007
#     classification). Sector-level (2-digit-equivalent) codes are stable
#     across NAICS revisions -- same 19 sector codes both years, incl. the
#     combined codes NAICS uses at sector level: 31-33 (manufacturing),
#     44-45 (retail), 48-49 (transportation/warehousing). "00" = total
#     across all sectors, used as the share denominator.
#   - 2019: the API has an INDLEVEL variable (2 = sector level) and plain
#     `state`/`county` geography params, so one filtered call per geography
#     level (for=county:*&in=state:* / for=state:*) gets exactly the sector
#     rows, confirmed working nationally in a single call each.
#   - 2007: no INDLEVEL variable exists in that vintage's schema, and the
#     API rejects comma-separated multi-value NAICS filters (HTTP 204) --
#     so this loops one API call per sector code (20 calls: "00" + 19
#     sectors), each already a national county-or-state wildcard pull.
#   - SUPPRESSION -- the two vintages behave differently, confirmed live:
#     2019 silently OMITS a sector's row entirely for a county with no
#     reportable establishments in that sector (small counties often have
#     as few as ~11 of 19 possible sector rows) -- becomes NA after
#     pivoting, nothing to detect. 2007 instead returns EMP=0 WITH a
#     non-null EMP_F suppression-flag letter (a/b/c/g/...) when the true
#     value is withheld for disclosure -- treating that as a real zero would
#     be wrong (e.g. Autauga County AL 2007 manufacturing: EMP_F="g", the
#     true count is some nonzero suppressed value, not 0) -- so 2007 rows
#     with any EMP_F flag are forced to NA before computing shares.
#   - Puerto Rico (state fips 72) appears in the state-level wildcard pull
#     for both years -- excluded, consistent with every other bucket.

library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)

census_key <- Sys.getenv("CENSUS_API_KEY")
valid_state_fips <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17",
                       "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
                       "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
                       "47","48","49","50","51","53","54","55","56")

sector_lookup <- tribble(
  ~code,     ~sector_name,
  "11",      "agriculture",
  "21",      "mining",
  "22",      "utilities",
  "23",      "construction",
  "31-33",   "manufacturing",
  "42",      "wholesale_trade",
  "44-45",   "retail",
  "48-49",   "transportation",
  "51",      "information",
  "52",      "finance",
  "53",      "real_estate",
  "54",      "professional_services",
  "55",      "management",
  "56",      "administrative_waste",
  "61",      "education_services",
  "62",      "healthcare",
  "71",      "arts_entertainment",
  "72",      "accommodation_food",
  "81",      "other_services"
)

cbp_get <- function(url) {
  raw <- tryCatch(fromJSON(content(GET(url), as = "text", encoding = "UTF-8"), simplifyVector = TRUE),
                   error = function(e) NULL)
  if (is.null(raw) || is.null(dim(raw))) return(NULL)
  d <- as_tibble(raw[-1, , drop = FALSE], .name_repair = "minimal")
  names(d) <- raw[1, ]
  # the NAICS exact-match filter clause (e.g. NAICS2002=31-33) gets echoed
  # back as a SECOND column with the same name as the normal data column --
  # keep only the first (the real data column).
  d[, !duplicated(names(d)), drop = FALSE]
}

# ==== 2019 (NAICS2017), one call per geography level ====
message("Pulling CBP 2019 (NAICS2017) sector employment, county...")
cbp19_county_raw <- cbp_get(paste0(
  "https://api.census.gov/data/2019/cbp?get=NAICS2017,EMP&INDLEVEL=2&for=county:*&in=state:*&key=", census_key
))
message("Pulling CBP 2019 (NAICS2017) sector employment, state...")
cbp19_state_raw <- cbp_get(paste0(
  "https://api.census.gov/data/2019/cbp?get=NAICS2017,EMP&INDLEVEL=2&for=state:*&key=", census_key
))

cbp19_county <- cbp19_county_raw %>%
  transmute(fips = paste0(state, county), code = trimws(NAICS2017), EMP = as.numeric(EMP))
cbp19_state <- cbp19_state_raw %>%
  filter(state %in% valid_state_fips) %>%
  transmute(state_fips = state, code = trimws(NAICS2017), EMP = as.numeric(EMP))

message("CBP 2019: ", n_distinct(cbp19_county$fips), " counties, ", n_distinct(cbp19_state$state_fips), " states with at least one sector row.")

# ==== 2007 (NAICS2002), one call per sector code (no INDLEVEL filter available) ====
message("Pulling CBP 2007 (NAICS2002) sector employment (20 calls: total + 19 sectors)...")
codes_2007 <- c("00", sector_lookup$code)

pull_2007_code <- function(code, geo) {
  geo_clause <- if (geo == "county") "for=county:*&in=state:*" else "for=state:*"
  url <- paste0("https://api.census.gov/data/2007/cbp?get=NAICS2002,EMP,EMP_F&NAICS2002=", code,
                "&", geo_clause, "&key=", census_key)
  d <- cbp_get(url)
  Sys.sleep(0.15)
  d
}

cbp07_county_raw <- bind_rows(lapply(codes_2007, pull_2007_code, geo = "county"))
cbp07_state_raw  <- bind_rows(lapply(codes_2007, pull_2007_code, geo = "state"))

# EMP_F non-null/non-blank means the EMP=0 shown is a suppression placeholder,
# not a real zero -- force those to NA before any share is computed.
cbp07_county <- cbp07_county_raw %>%
  transmute(fips = paste0(state, county), code = trimws(NAICS2002),
            EMP = if_else(is.na(EMP_F) | EMP_F == "", as.numeric(EMP), NA_real_))
cbp07_state <- cbp07_state_raw %>%
  filter(state %in% valid_state_fips) %>%
  transmute(state_fips = state, code = trimws(NAICS2002),
            EMP = if_else(is.na(EMP_F) | EMP_F == "", as.numeric(EMP), NA_real_))

is_suppressed <- function(f) !is.na(f) & f != ""
n_suppressed_county <- sum(is_suppressed(cbp07_county_raw$EMP_F))
n_suppressed_state  <- sum(is_suppressed(cbp07_state_raw$EMP_F))
message("CBP 2007: ", n_distinct(cbp07_county$fips), " counties, ", n_distinct(cbp07_state$state_fips),
        " states with at least one sector row. ", n_suppressed_county,
        " county-sector cells suppressed (EMP_F flag), ", n_suppressed_state, " state-sector cells suppressed.")

# ==== build share tables ====
make_shares <- function(long, id_col, year_suffix) {
  wide <- long %>%
    filter(code %in% c("00", sector_lookup$code)) %>%
    pivot_wider(id_cols = all_of(id_col), names_from = code, values_from = EMP)
  total <- wide[["00"]]
  shares <- lapply(sector_lookup$code, function(cd) {
    if (!cd %in% names(wide)) return(rep(NA_real_, nrow(wide)))
    100 * wide[[cd]] / total
  })
  names(shares) <- paste0("share_", sector_lookup$sector_name, "_", year_suffix)
  bind_cols(wide[id_col], as_tibble(shares))
}

county_industry_2007 <- make_shares(cbp07_county, "fips", "2007")
county_industry_2019 <- make_shares(cbp19_county, "fips", "2019")
state_industry_2007  <- make_shares(cbp07_state, "state_fips", "2007")
state_industry_2019  <- make_shares(cbp19_state, "state_fips", "2019")

county_industry <- county_industry_2007 %>% full_join(county_industry_2019, by = "fips")
state_industry  <- state_industry_2007  %>% full_join(state_industry_2019, by = "state_fips")

message("\nIndustry mix county table: ", nrow(county_industry), " counties, ", ncol(county_industry) - 1, " share columns.")
message("Industry mix state table: ", nrow(state_industry), " states/DC, ", ncol(state_industry) - 1, " share columns.")

saveRDS(county_industry, "raw_data/20_industry_mix_county.rds")
saveRDS(state_industry, "raw_data/20_industry_mix_state.rds")
message("Saved 20_industry_mix_county.rds and 20_industry_mix_state.rds.")
