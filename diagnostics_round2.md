# Diagnostics Round 2

All refits below are in-memory only — no `.rds`, `.qmd`, or pipeline file was
created or modified. Source data: `spine_data.csv` and the cached fit objects
in `raw_data/modeling_andrew_*.rds` (all read-only). This is the only file
written. Tables and verbatim code only, no interpretation.

`studied_z` = `proprietor_share_z, migration_rate_z, unemployment_rate_z, working_age_share_z`.
`controls` = `income, poverty_rate, log_population, log_density, rucc, pop_trend`.

---

## 1. Collinearity

**Pearson correlation, migration_rate_z vs. pop_trend:**

| Crisis | r |
|---|---|
| 2008 | 0.58262 |
| COVID | 0.57306 |

**VIF, primary OLS, 2008** (`lm(log_outcome ~ studied_z + controls, data = res_2008$df)`):

| Term | VIF |
|---|---|
| proprietor_share_z | 2.008 |
| migration_rate_z | 1.629 |
| unemployment_rate_z | 1.415 |
| working_age_share_z | 1.279 |
| income | 4.295 |
| poverty_rate | 2.985 |
| log_population | 6.454 |
| log_density | 5.062 |
| rucc (GVIF^(1/2Df))^2 | 1.193 |
| pop_trend | 2.052 |

**VIF, primary OLS, COVID:**

| Term | VIF |
|---|---|
| proprietor_share_z | 1.827 |
| migration_rate_z | 1.676 |
| unemployment_rate_z | 1.422 |
| working_age_share_z | 1.321 |
| income | 4.038 |
| poverty_rate | 3.226 |
| log_population | 6.396 |
| log_density | 5.072 |
| rucc (GVIF^(1/2Df))^2 | 1.187 |
| pop_trend | 2.181 |

---

## 2. pop_trend drop test

Refit: `lm_robust(log_outcome ~ studied_z + controls[-pop_trend], data = df, se_type = "HC1")`,
identical data/seed otherwise.

**2008:**

| Term | Est. (with) | SE (with) | p (with) | Est. (without) | SE (without) | p (without) |
|---|---|---|---|---|---|---|
| proprietor_share_z | 0.00748 | 0.00376 | 0.04677 | 0.00877 | 0.00373 | 0.01872 |
| migration_rate_z | 0.00142 | 0.00438 | 0.74641 | 0.00814 | 0.00289 | **0.00493** |
| unemployment_rate_z | 0.00369 | 0.00262 | 0.15855 | 0.00353 | 0.00264 | 0.18056 |
| working_age_share_z | -0.01635 | 0.00360 | 0.00001 | -0.01618 | 0.00365 | 0.00001 |

**Flag: migration_rate_z crosses p = 0.05** (0.74641 → 0.00493) when pop_trend is dropped, 2008 only.

**COVID:**

| Term | Est. (with) | SE (with) | p (with) | Est. (without) | SE (without) | p (without) |
|---|---|---|---|---|---|---|
| proprietor_share_z | 0.01094 | 0.00177 | 0.00000 | 0.01258 | 0.00177 | 0.00000 |
| migration_rate_z | 0.00943 | 0.00222 | 0.00002 | 0.02001 | 0.00261 | 0.00000 |
| unemployment_rate_z | 0.00041 | 0.00130 | 0.75442 | -0.00227 | 0.00136 | 0.09610 |
| working_age_share_z | -0.00578 | 0.00134 | 0.00002 | -0.00646 | 0.00134 | 0.00000 |

No COVID predictor crosses the 0.05 boundary (unemployment_rate_z moves from 0.754 to 0.096 — both sides of the boundary are ≥0.05, does not flag under "crosses 0.05" as stated).

---

## 3. Migration zero check

From `spine_data.csv`:

| Column | n exact zeros | n NA | p1 | p5 | p25 | p50 | p75 | p95 | p99 |
|---|---|---|---|---|---|---|---|---|---|
| net_migration_rate_2007 | 13 | 99 | -25.3654 | -13.7750 | -4.2264 | 0.4703 | 6.4637 | 19.7913 | 33.9724 |
| net_migration_rate_2019 | 10 | 153 | -22.6272 | -13.5766 | -4.5787 | 0.0000 | 5.1677 | 16.8707 | 26.8002 |

**Trace of `pipeline/12_irs_migration.R` for suppression/negative/missing → 0 or dropped:**

**NOT FOUND.** No line in `pipeline/12_irs_migration.R` converts a missing,
suppressed, or negative IRS SOI value to 0, and no line explicitly drops
suppressed cells. The file contains no suppression-flag handling of any kind
(contrast with `pipeline/20_cbp_industry_mix.R`, which explicitly checks an
`EMP_F` suppression flag and forces suppressed cells to `NA` — no equivalent
flag or check exists in this file for the IRS SOI migration data). The only
value-coercion lines in the file are:

```r
transmute(fips = paste0(State_Code_Dest, County_Code_Dest), inflow_n2 = as.numeric(Exmpt_Num))
transmute(fips = paste0(State_Code_Origin, County_Code_Origin), outflow_n2 = as.numeric(Exmpt_Num))
transmute(fips = paste0(y2_statefips, y2_countyfips), inflow_n2 = as.numeric(n2))
transmute(fips = paste0(y1_statefips, y1_countyfips), outflow_n2 = as.numeric(n2))
```

`as.numeric()` on a non-numeric source value produces `NA` (with an R
coercion warning), not `0`. The 13 (2007) / 10 (2019) exact zeros counted
above are therefore not distinguishable, from this file's code alone, from
true zero net-flow counties vs. any other origin — **NOT FOUND** for whether
any of those specific zeros are IRS-suppressed values that happened to
coerce to 0 (no such coercion path exists in the code to check against).

---

## 4. R-squared

| Crisis | OLS adj. R² | rf_a OOB rsq | rf_b OOB rsq |
|---|---|---|---|
| 2008 | 0.1042 | 0.1453 | 0.1219 |
| COVID | 0.3237 | 0.3468 | 0.3366 |
| Placebo | 0.1945 | 0.3098 | NOT COMPUTED — no separate rf_b exists for the placebo window. `modeling_andrew.qmd` fits a single `placebo_rf` using the full `controls` set (equivalent in spec to `rf_a`, not `rf_b`); there is no placebo forest built with `controls_no_wealth`. |

---

## 5. LASSO output

| Crisis | lambda.min | lambda.1se | n nonzero studied @ 1se |
|---|---|---|---|
| 2008 | 0.0002730155 | 0.003066848 | 0 |
| COVID | 0.0001098803 | 0.001790775 | 0 |

**2008, coefficients at lambda.1se:**

| Term | Plain LASSO | Relaxed LASSO | OLS |
|---|---|---|---|
| proprietor_share_z | 0 | 0 | 0.00748 |
| migration_rate_z | 0 | 0 | 0.00142 |
| unemployment_rate_z | 0 | 0 | 0.00369 |
| working_age_share_z | 0 | 0 | -0.01635 |

**COVID, coefficients at lambda.1se:**

| Term | Plain LASSO | Relaxed LASSO | OLS |
|---|---|---|---|
| proprietor_share_z | 0 | 0 | 0.01094 |
| migration_rate_z | 0 | 0 | 0.00943 |
| unemployment_rate_z | 0 | 0 | 0.00041 |
| working_age_share_z | 0 | 0 | -0.00578 |

---

## 6. State-clustered SEs

Refit: `lm_robust(log_outcome ~ studied_z + controls, data = df, clusters = state_fips, se_type = "stata")`,
`state_fips <- substr(fips, 1, 2)`.

**2008 (n_clusters = 50):**

| Term | Estimate | HC1 SE | HC1 p | Clustered SE | Clustered p |
|---|---|---|---|---|---|
| proprietor_share_z | 0.00748 | 0.00376 | 0.04677 | 0.00334 | 0.02955 |
| migration_rate_z | 0.00142 | 0.00438 | 0.74641 | 0.00438 | 0.74783 |
| unemployment_rate_z | 0.00369 | 0.00262 | 0.15855 | 0.00660 | 0.57849 |
| working_age_share_z | -0.01635 | 0.00360 | 0.00001 | 0.00388 | 0.00011 |

**COVID (n_clusters = 50):**

| Term | Estimate | HC1 SE | HC1 p | Clustered SE | Clustered p |
|---|---|---|---|---|---|
| proprietor_share_z | 0.01094 | 0.00177 | 0.00000 | 0.00206 | 0.00000 |
| migration_rate_z | 0.00943 | 0.00222 | 0.00002 | 0.00265 | 0.00085 |
| unemployment_rate_z | 0.00041 | 0.00130 | 0.75442 | 0.00249 | 0.87084 |
| working_age_share_z | -0.00578 | 0.00134 | 0.00002 | 0.00141 | 0.00016 |

**2008 lower income band (n_clusters = 41, n obs = 1025):**

| Term | Estimate | HC1 SE | HC1 p | Clustered SE | Clustered p |
|---|---|---|---|---|---|
| proprietor_share_z | 0.01993 | 0.00657 | 0.00249 | 0.00597 | 0.00183 |
| migration_rate_z | 0.00175 | 0.00870 | 0.84102 | 0.00792 | 0.82672 |
| unemployment_rate_z | 0.01707 | 0.00443 | 0.00012 | 0.00865 | 0.05539 |
| working_age_share_z | -0.01763 | 0.00548 | 0.00134 | 0.00627 | 0.00758 |

---

## 7. Placebo RF importance (refit, `importance = TRUE`, seed 20260729, ntree 500)

`placebo_rf` refit in-memory only; the on-disk cache (`importance = FALSE`)
was not overwritten.

**Placebo, type = 1 (%IncMSE), ranked descending:**

| Term | %IncMSE |
|---|---|
| unemployment_rate_z | 40.8491 |
| pop_trend | 35.3371 |
| log_population | 26.3047 |
| poverty_rate | 25.8246 |
| income | 25.1501 |
| log_density | 24.5861 |
| proprietor_share_z | 23.5180 |
| rucc | 21.0757 |
| migration_rate_z | 17.8815 |
| working_age_share_z | 17.0096 |

**Three-column comparison** (placebo, 2008 `rf_b`, COVID `rf_b`), sorted by placebo value.
Note: placebo's forest includes `income`/`poverty_rate` (full `controls`);
`rf_b` for 2008/COVID excludes them (`controls_no_wealth`) — the three
columns are not run on identical predictor sets; `income`/`poverty_rate`
rows are `NA` for the 2008/COVID columns because those two variables were
not in `rf_b`'s formula, not because their importance was 0.

| Term | Placebo | 2008 rf_b | COVID rf_b |
|---|---|---|---|
| unemployment_rate_z | 40.8491 | 15.3850 | 10.6429 |
| pop_trend | 35.3371 | 34.7445 | 23.9502 |
| log_population | 26.3047 | 24.8308 | 10.7698 |
| poverty_rate | 25.8246 | NA | NA |
| income | 25.1501 | NA | NA |
| log_density | 24.5861 | 18.1072 | 12.1924 |
| proprietor_share_z | 23.5180 | 19.9471 | 16.4860 |
| rucc | 21.0757 | 14.0642 | 10.9860 |
| migration_rate_z | 17.8815 | 17.6757 | 24.8404 |
| working_age_share_z | 17.0096 | 23.0284 | 15.4203 |
