# 34_optionB_report.R
#
# Consolidated Option B viability report -- reads the saved results from
# 30-33 and answers the 6 questions directly: final sample, LASSO stability,
# RF importance, wealth-band stress test, the industry-mix flip hypothesis,
# and education's added value. Read-only; no modeling here, just reporting.

library(dplyr)

lasso <- readRDS("raw_data/31_optionB_lasso_results.rds")
rf    <- readRDS("raw_data/32_optionB_rf_results.rds")
wb    <- readRDS("raw_data/33_optionB_wealthband_results.rds")
s2008 <- readRDS("raw_data/30_optionB_sample_2008.rds")
scovid <- readRDS("raw_data/30_optionB_sample_covid.rds")
sboth <- readRDS("raw_data/30_optionB_sample_both.rds")

message("############################################################")
message("OPTION B -- FINAL VIABILITY REPORT")
message("############################################################")

message("\n=== 5. INDUSTRY MIX -- does it flip between crises? ===")
flip_2008 <- rf$rf_2008$importance %>% filter(grepl("^share_", variable))
flip_covid <- rf$rf_covid$importance %>% filter(grepl("^share_", variable))
message("\nRF importance (%IncMSE), 2008:"); print(flip_2008, row.names = FALSE)
message("\nRF importance (%IncMSE), COVID:"); print(flip_covid, row.names = FALSE)
message("\nLASSO lambda.1se: NO industry sector was selected in EITHER crisis, any seed (0/5 for all 12 sector-year columns).")
message("LASSO lambda.min (weak-effect view): manufacturing_2007 & construction_2007 both negative ",
        "(coef ~ -0.0015, -0.0024) -- theory-consistent DIRECTION (goods-producing hurts 2008 recovery) but tiny magnitude. ",
        "accommodation_food_2019 also negative (~-0.0003) -- theory-consistent direction (tourism hurts COVID recovery) but even smaller.")
message("RF ranking: manufacturing/construction land MID-PACK for 2008 (ranks 9-10 of 18, ahead of healthcare/accommodation_food/age) ",
        "-- a real but modest signal, roughly consistent with theory.")
message("RF ranking: accommodation_food_2019 is DEAD LAST (lowest %IncMSE of all 18 variables) for COVID ",
        "-- this CONTRADICTS the tourism hypothesis; it's the least useful predictor in the COVID model, not the most.")
message("VERDICT: industry mix does NOT clearly flip between crises in a way that earns its coverage cost. ",
        "2008 shows a modest, theory-consistent signal for goods-producing sectors; COVID shows no support for ",
        "the accommodation/food hypothesis at all. Neither crisis has an industry sector survive LASSO's standard threshold.")

message("\n=== 6. EDUCATION -- meaningful signal or redundant? ===")
edu_2008 <- lasso$lasso_2008$freq_df %>% filter(grepl("^pct_", variable))
edu_covid <- lasso$lasso_covid$freq_df %>% filter(grepl("^pct_", variable))
message("\nLASSO lambda.1se selection (2008):"); print(edu_2008, row.names = FALSE)
message("\nLASSO lambda.1se selection (COVID):"); print(edu_covid, row.names = FALSE)
rf_edu_2008 <- rf$rf_2008$importance %>% filter(grepl("^pct_", variable))
rf_edu_covid <- rf$rf_covid$importance %>% filter(grepl("^pct_", variable))
message("\nRF importance (2008):"); print(rf_edu_2008, row.names = FALSE)
message("RF importance (COVID):"); print(rf_edu_covid, row.names = FALSE)
message("\nVERDICT: education carries REAL signal for 2008 -- pct_bachelors_plus_2007 selected in all 5 LASSO seeds ",
        "(positive coef, more bachelor's degrees -> better 2008 recovery) and ranks 3rd of 18 in RF importance. ",
        "For COVID it's much weaker -- pct_hs_plus_2019 only wobbles in 1/5 LASSO seeds, pct_bachelors_plus_2019 ",
        "never selected, though both still place mid-pack (4th, 9th) in RF importance. Education earns more of its ",
        "keep for the 2008 model than for COVID.")

message("\n############################################################")
message("SUMMARY TABLE")
message("############################################################")
message(sprintf("Final sample: %d counties (2008), %d counties (COVID), %d counties BOTH crises -- vs. Option A's ~3,012.",
                 nrow(s2008), nrow(scovid), nrow(sboth)))
message("LASSO stability: 2008 fully stable but sparse (4/18 vars, none industry/most controls); ",
        "COVID 3/18 wobbly seed-to-seed (pct_hs_plus, poverty_rate, rucc).")
message(sprintf("RF fit: 2008 OOB R^2 = %.1f%%; COVID OOB R^2 = %.1f%% -- COVID much more predictable than 2008 in this sample.",
                 rf$rf_2008$rf$rsq[length(rf$rf_2008$rf$rsq)]*100, rf$rf_covid$rf$rsq[length(rf$rf_covid$rf$rsq)]*100))
message("Wealth-band stress test: 'lower' band (n=57 in 2008, n=99 in COVID) is where it breaks down -- ",
        "high R^2 (0.65 / 0.49) driven by too few observations per parameter (ratio ~3-5.5:1), LASSO picks ",
        "just 1 near-arbitrary variable, NOT trustworthy. 'middle' band is borderline (2008 LASSO collapses to ",
        "null model entirely). 'higher' band (n=463-468) is the only band with stable, interpretable results.")
message("Industry mix: does not clearly flip between crises as theorized; weak/mixed support at best.")
message("Education: meaningfully useful for 2008, marginal for COVID.")
