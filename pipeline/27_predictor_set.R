# 27_predictor_set.R
#
# CANONICAL predictor-set definition -- source() this from any future
# modeling script instead of re-deriving the column lists by hand. Reflects
# the design review decision recorded here:
#
#   - birth_rate_2007 / birth_rate_2019 are RETAINED in spine_data.csv /
#     state_spine.csv (not deleted) but EXCLUDED from the studied-predictor
#     set: no theoretical link to employment recovery, and redundant with
#     age structure. Simply not referenced below.
#   - education (pct_hs_plus_*, pct_bachelors_plus_*) and industry mix
#     (share_*_*) were already dropped from both county and state analysis
#     in an earlier review (coverage collapsed to ~750/51 counties when
#     required) -- also not referenced below.
#   - pop_trend_2007 / pop_trend_2019 added as a CONTROL (not a studied
#     predictor): pre-crisis population trend, to separate long-run
#     structural decline from crisis response so migration and the outcome
#     aren't both just proxying "this county was already shrinking."
#
# Studied predictors (what the analysis is actually testing):
#   self-employment, migration, pre-crisis unemployment, age structure.
#
# Controls (context variables, not the object of study):
#   poverty rate, median household income, total population, population
#   density, rurality (RUCC for counties / pct_rural for states), and now
#   population trend.

STUDIED_PREDICTORS_2007 <- c(
  "proprietor_share_2007",      # self-employment
  "net_migration_rate_2007",    # migration
  "pre_unemployment_2007",      # pre-crisis unemployment
  "working_age_share_2007"      # age structure
)
STUDIED_PREDICTORS_2019 <- c(
  "proprietor_share_2019",
  "net_migration_rate_2019",
  "pre_unemployment_2019",
  "working_age_share_2019"
)

CONTROLS_COUNTY_2008 <- c(
  "poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
  "population_density_2007", "rucc_2003_2007", "pop_trend_2007"
)
CONTROLS_COUNTY_COVID <- c(
  "poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
  "population_density_2019", "rucc_2013_2019", "pop_trend_2019"
)
CONTROLS_STATE_2008 <- c(
  "poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
  "population_density_2007", "pct_rural_2007", "pop_trend_2007"
)
CONTROLS_STATE_COVID <- c(
  "poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
  "population_density_2019", "pct_rural_2019", "pop_trend_2019"
)

# county-only, not modeled at state level (only 51 states, terciles not meaningful)
WEALTH_BAND_COLS <- c("wealth_band_2008", "wealth_band_covid")

# explicitly excluded from modeling, columns retained in the files unused:
EXCLUDED_COLS <- c(
  "birth_rate_2007", "birth_rate_2019",                                  # no theoretical link, redundant w/ age structure
  "pct_hs_plus_2007", "pct_bachelors_plus_2007",                          # education -- coverage too low
  "pct_hs_plus_2019", "pct_bachelors_plus_2019",
  grep("^share_", c(                                                      # industry mix -- coverage too low
    "share_agriculture_2007","share_mining_2007","share_utilities_2007","share_construction_2007",
    "share_manufacturing_2007","share_wholesale_trade_2007","share_retail_2007","share_transportation_2007",
    "share_information_2007","share_finance_2007","share_real_estate_2007","share_professional_services_2007",
    "share_management_2007","share_administrative_waste_2007","share_education_services_2007",
    "share_healthcare_2007","share_arts_entertainment_2007","share_accommodation_food_2007","share_other_services_2007",
    "share_agriculture_2019","share_mining_2019","share_utilities_2019","share_construction_2019",
    "share_manufacturing_2019","share_wholesale_trade_2019","share_retail_2019","share_transportation_2019",
    "share_information_2019","share_finance_2019","share_real_estate_2019","share_professional_services_2019",
    "share_management_2019","share_administrative_waste_2019","share_education_services_2019",
    "share_healthcare_2019","share_arts_entertainment_2019","share_accommodation_food_2019","share_other_services_2019"
  ), value = TRUE)
)
