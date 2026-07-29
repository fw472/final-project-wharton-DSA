# 29_predictor_set_optionB.R
#
# OPTION B predictor-set definition, on the option-b-test branch only. Same
# base as Option A (pipeline/27_predictor_set.R: self-employment, migration,
# pre-crisis unemployment, age structure + controls incl. pop_trend), PLUS
# education and 6 focus industry sectors added back in as STUDIED predictors
# instead of being excluded. This trades sample size for richer predictors --
# see pipeline/30_optionB_sample.R for the resulting county count.
#
# Focus industry sectors chosen for their theorized differential relevance to
# the two crises: manufacturing, construction (goods-producing, expected to
# hurt 2008 recovery more), accommodation_food (tourism, expected to hurt
# COVID recovery more), retail, healthcare, professional_services (broad
# service-sector coverage / comparison sectors).

STUDIED_PREDICTORS_2007_OPTIONB <- c(
  "proprietor_share_2007", "net_migration_rate_2007", "pre_unemployment_2007",
  "working_age_share_2007",
  "pct_hs_plus_2007", "pct_bachelors_plus_2007",
  "share_manufacturing_2007", "share_construction_2007", "share_retail_2007",
  "share_accommodation_food_2007", "share_healthcare_2007", "share_professional_services_2007"
)
STUDIED_PREDICTORS_2019_OPTIONB <- c(
  "proprietor_share_2019", "net_migration_rate_2019", "pre_unemployment_2019",
  "working_age_share_2019",
  "pct_hs_plus_2019", "pct_bachelors_plus_2019",
  "share_manufacturing_2019", "share_construction_2019", "share_retail_2019",
  "share_accommodation_food_2019", "share_healthcare_2019", "share_professional_services_2019"
)

# same controls as Option A (income/poverty, population, density, rurality, pop_trend)
CONTROLS_COUNTY_2008 <- c("poverty_rate_2007", "median_hh_income_2007", "total_population_2007",
                           "population_density_2007", "rucc_2003_2007", "pop_trend_2007")
CONTROLS_COUNTY_COVID <- c("poverty_rate_2019", "median_hh_income_2019", "total_population_2019",
                            "population_density_2019", "rucc_2013_2019", "pop_trend_2019")

WEALTH_BAND_COLS <- c("wealth_band_2008", "wealth_band_covid")

FOCUS_SECTORS <- c("manufacturing", "construction", "retail", "accommodation_food",
                    "healthcare", "professional_services")
