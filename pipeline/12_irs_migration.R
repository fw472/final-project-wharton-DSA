# 12_irs_migration.R
#
# Predictor bucket 2: MIGRATION. Net migration rate (per 1,000 population) at
# 2007 and 2019, for both counties and states. IRS SOI county-to-county /
# state-to-state migration flow files are used instead of Census PEP because
# PEP's pre-2010 vintage doesn't carry a migration component.
#
#   - "As of 2007" uses the 2006->2007 filing-year pair (no 2008 data
#     involved). "As of 2019" uses the 2018->2019 pair. Both predate their
#     respective crisis.
#   - Each SOI file (inflow or outflow) contains, for every home geography,
#     one row per real counterpart geography PLUS several aggregate rows
#     identified by special codes: "96" = Total Migration - US & Foreign,
#     "97" = Total Migration - US only, "98" = Foreign only. Taking the "96"
#     row directly gives the total in/out flow for that geography-year --
#     no need to sum individual county-pair rows (confirmed against live
#     files: 96 = 97 + 98).
#   - n2 (called Exmpt_Num pre-2011, n2 from 2011 on) = number of exemptions
#     on the migrating returns, IRS's standard population-proxy migrant
#     count. Column names switch from CamelCase (pre-2011 vintage, e.g.
#     Return_Num) to snake_case (2011+, e.g. n1) -- both eras confirmed live.
#   - Net migration rate = 1000 * (inflow_n2 - outflow_n2) / total_population,
#     where total_population reuses the pre-crisis PEP population already
#     cached by 03_controls.R / 08_state_controls.R (fully additive, safe to
#     reuse as the rate denominator).
#   - Column layouts (home geography field vs. counterpart field) differ
#     between inflow and outflow files, and between county and state files
#     -- each pull function below hard-codes the confirmed field names for
#     its specific file.

library(dplyr)
library(readr)
library(httr)

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"

download_soi <- function(fname) {
  path <- paste0("raw_data/", fname)
  if (!file.exists(path)) {
    message("Downloading ", fname, "...")
    GET(paste0("https://www.irs.gov/pub/irs-soi/", fname),
        add_headers(`User-Agent` = ua), write_disk(path, overwrite = TRUE), timeout(60))
  }
  path
}

# All SOI files are read with every column forced to character -- state and
# county code columns like "01"/"001" would otherwise get guessed as integer
# by readr and silently lose their leading zeros, breaking fips concatenation.
read_soi_csv <- function(path) read_csv(path, col_types = cols(.default = "c"))

# ---- COUNTY: 2006-07 vintage (CamelCase columns) ----
county_inflow_0607 <- read_soi_csv(download_soi("countyinflow0607.csv")) %>%
  filter(State_Code_Origin == "96", County_Code_Origin == "000") %>%
  transmute(fips = paste0(State_Code_Dest, County_Code_Dest), inflow_n2 = as.numeric(Exmpt_Num))

county_outflow_0607 <- read_soi_csv(download_soi("countyoutflow0607.csv")) %>%
  filter(State_Code_Dest == "96", County_Code_Dest == "000") %>%
  transmute(fips = paste0(State_Code_Origin, County_Code_Origin), outflow_n2 = as.numeric(Exmpt_Num))

# ---- COUNTY: 2018-19 vintage (snake_case columns) ----
county_inflow_1819 <- read_soi_csv(download_soi("countyinflow1819.csv")) %>%
  filter(y1_statefips == "96", y1_countyfips == "000") %>%
  transmute(fips = paste0(y2_statefips, y2_countyfips), inflow_n2 = as.numeric(n2))

county_outflow_1819 <- read_soi_csv(download_soi("countyoutflow1819.csv")) %>%
  filter(y2_statefips == "96", y2_countyfips == "000") %>%
  transmute(fips = paste0(y1_statefips, y1_countyfips), outflow_n2 = as.numeric(n2))

county_flows_2007 <- county_inflow_0607 %>% inner_join(county_outflow_0607, by = "fips")
county_flows_2019 <- county_inflow_1819 %>% inner_join(county_outflow_1819, by = "fips")
message("County migration flows: ", nrow(county_flows_2007), " counties (2007), ",
        nrow(county_flows_2019), " counties (2019).")

county_pop_2007 <- readRDS("raw_data/03_controls_2008.rds") %>% select(fips, total_population_2007)
county_pop_2019 <- readRDS("raw_data/03_controls_covid.rds") %>% select(fips, total_population_2019)

county_migration_2007 <- county_flows_2007 %>%
  inner_join(county_pop_2007, by = "fips") %>%
  mutate(net_migration_rate_2007 = 1000 * (inflow_n2 - outflow_n2) / total_population_2007) %>%
  select(fips, net_migration_rate_2007)

county_migration_2019 <- county_flows_2019 %>%
  inner_join(county_pop_2019, by = "fips") %>%
  mutate(net_migration_rate_2019 = 1000 * (inflow_n2 - outflow_n2) / total_population_2019) %>%
  select(fips, net_migration_rate_2019)

county_migration <- county_migration_2007 %>% full_join(county_migration_2019, by = "fips")
message("County net migration rate: ", nrow(county_migration), " counties.")

# ---- STATE: 2006-07 vintage ----
state_inflow_0607 <- read_soi_csv(download_soi("stateinflow0607.csv")) %>%
  filter(State_Code_Origin == "96") %>%
  transmute(state_fips = State_Code_Dest, inflow_n2 = as.numeric(Exmpt_Num))

state_outflow_0607 <- read_soi_csv(download_soi("stateoutflow0607.csv")) %>%
  filter(State_Code_Dest == "96") %>%
  transmute(state_fips = State_Code_Origin, outflow_n2 = as.numeric(Exmpt_Num))

# ---- STATE: 2018-19 vintage ----
state_inflow_1819 <- read_soi_csv(download_soi("stateinflow1819.csv")) %>%
  filter(y1_statefips == "96") %>%
  transmute(state_fips = y2_statefips, inflow_n2 = as.numeric(n2))

state_outflow_1819 <- read_soi_csv(download_soi("stateoutflow1819.csv")) %>%
  filter(y2_statefips == "96") %>%
  transmute(state_fips = y1_statefips, outflow_n2 = as.numeric(n2))

state_flows_2007 <- state_inflow_0607 %>% inner_join(state_outflow_0607, by = "state_fips") %>%
  filter(state_fips != "00")
state_flows_2019 <- state_inflow_1819 %>% inner_join(state_outflow_1819, by = "state_fips") %>%
  filter(state_fips != "00")
message("State migration flows: ", nrow(state_flows_2007), " states (2007), ",
        nrow(state_flows_2019), " states (2019).")

state_pop_2007 <- readRDS("raw_data/08_state_controls_2008.rds") %>% select(state_fips, total_population_2007)
state_pop_2019 <- readRDS("raw_data/08_state_controls_covid.rds") %>% select(state_fips, total_population_2019)

state_migration_2007 <- state_flows_2007 %>%
  inner_join(state_pop_2007, by = "state_fips") %>%
  mutate(net_migration_rate_2007 = 1000 * (inflow_n2 - outflow_n2) / total_population_2007) %>%
  select(state_fips, net_migration_rate_2007)

state_migration_2019 <- state_flows_2019 %>%
  inner_join(state_pop_2019, by = "state_fips") %>%
  mutate(net_migration_rate_2019 = 1000 * (inflow_n2 - outflow_n2) / total_population_2019) %>%
  select(state_fips, net_migration_rate_2019)

state_migration <- state_migration_2007 %>% full_join(state_migration_2019, by = "state_fips")
message("State net migration rate: ", nrow(state_migration), " states/DC.")

saveRDS(county_migration, "raw_data/12_migration_county.rds")
saveRDS(state_migration, "raw_data/12_migration_state.rds")
message("Saved 12_migration_county.rds and 12_migration_state.rds.")
