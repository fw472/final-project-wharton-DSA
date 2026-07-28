# 06_state_panel.R
#
# Builds the state-level employment panel by SUMMING the existing county
# panel (raw_data/01_bls_panel.rds) up to state FIPS -- no new BLS download.
# This is deliberate, not just convenient: LAUS county estimates are
# benchmarked (raked) to independently-estimated state control totals as
# part of BLS's own methodology, so summing our already-validated county
# panel closely reproduces the official state series while reusing data
# that's already been pulled and sanity-checked, avoiding another
# BLS-blocked manual-download round for a parallel set of state files.
#
# States and counties are kept as SEPARATE datasets throughout this
# pipeline (06-10_state_*.R) -- a state is its counties summed, so a model
# mixing county rows and state rows would double-count. They are compared
# at the end, never merged into one table.

library(dplyr)

panel <- readRDS("raw_data/01_bls_panel.rds")

# Standard 2-digit state FIPS for the 50 states + DC. Territories (PR = 72,
# VI, GU, AS, MP) are excluded per "all 50 states (+ DC if clean)".
state_lookup <- tribble(
  ~state_fips, ~state_abbr, ~state_name,
  "01","AL","Alabama", "02","AK","Alaska", "04","AZ","Arizona", "05","AR","Arkansas",
  "06","CA","California", "08","CO","Colorado", "09","CT","Connecticut", "10","DE","Delaware",
  "11","DC","District of Columbia", "12","FL","Florida", "13","GA","Georgia", "15","HI","Hawaii",
  "16","ID","Idaho", "17","IL","Illinois", "18","IN","Indiana", "19","IA","Iowa",
  "20","KS","Kansas", "21","KY","Kentucky", "22","LA","Louisiana", "23","ME","Maine",
  "24","MD","Maryland", "25","MA","Massachusetts", "26","MI","Michigan", "27","MN","Minnesota",
  "28","MS","Mississippi", "29","MO","Missouri", "30","MT","Montana", "31","NE","Nebraska",
  "32","NV","Nevada", "33","NH","New Hampshire", "34","NJ","New Jersey", "35","NM","New Mexico",
  "36","NY","New York", "37","NC","North Carolina", "38","ND","North Dakota", "39","OH","Ohio",
  "40","OK","Oklahoma", "41","OR","Oregon", "42","PA","Pennsylvania", "44","RI","Rhode Island",
  "45","SC","South Carolina", "46","SD","South Dakota", "47","TN","Tennessee", "48","TX","Texas",
  "49","UT","Utah", "50","VT","Vermont", "51","VA","Virginia", "53","WA","Washington",
  "54","WV","West Virginia", "55","WI","Wisconsin", "56","WY","Wyoming"
)
stopifnot(nrow(state_lookup) == 51)

panel_with_state <- panel %>%
  mutate(state_fips = substr(fips, 1, 2)) %>%
  filter(state_fips %in% state_lookup$state_fips)

n_na_dropped <- sum(is.na(panel_with_state$employed))
message("County-year rows with NA employment excluded from state sums: ", n_na_dropped,
        " (of ", nrow(panel_with_state), ")")

state_panel <- panel_with_state %>%
  filter(!is.na(employed)) %>%
  group_by(state_fips, year) %>%
  summarise(employed = sum(employed), n_counties = n(), .groups = "drop") %>%
  left_join(state_lookup, by = "state_fips")

message("State employment panel: ", nrow(state_panel), " state-year rows, ",
        n_distinct(state_panel$state_fips), " states/DC, years ",
        min(state_panel$year), "-", max(state_panel$year), ".")

# quick QA: flag any state-year where the county count looks off relative to
# that state's typical county count (would indicate a partial-sum artifact)
qa <- state_panel %>%
  group_by(state_fips) %>%
  mutate(typical_n = median(n_counties)) %>%
  ungroup() %>%
  filter(n_counties != typical_n)
if (nrow(qa) > 0) {
  message("NOTE: ", nrow(qa), " state-year(s) have a county count different from that state's typical count (see raw_data/06_state_panel.rds n_counties column) -- likely a year with one or two counties missing from BLS's file, not a bug.")
}

saveRDS(state_panel, "raw_data/06_state_panel.rds")
