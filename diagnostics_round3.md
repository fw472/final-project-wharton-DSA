# Diagnostics Round 3

Two new files only: `baseline_snapshot.rds` (Part A) and this file. No
existing `.qmd`, `.R`, or `.csv` was modified. All refits below (Parts B, C,
D coefficient recomputations) are in-memory only. Tables and verbatim code
only, no interpretation.

`studied_z` = `proprietor_share_z, migration_rate_z, unemployment_rate_z, working_age_share_z`.
`controls` = `income, poverty_rate, log_population, log_density, rucc, pop_trend`.
`controls_no_wealth` = `controls` minus `income, poverty_rate`.

---

## Part A — baseline snapshot

**Saved:** `baseline_snapshot.rds` — a named list with top-level elements
`"2008"` and `"covid"`, each containing: `ols_coef_table` (data frame: term,
estimate, std.error, p.value, all 18 terms incl. intercept and controls),
`ols_adj_r2`, `rf_a_oob_rsq`, `rf_b_oob_rsq`, `rf_b_importance_type1` (named
numeric, 8 terms), `lambda_min`, `lambda_1se` (both from the plain, non-relaxed
`cvfit`), and `band_ols_coef_tables` (list of 3 data frames: lower/middle/higher,
same 4-column structure as `ols_coef_table`).

```
Saved baseline_snapshot.rds -- named list with elements: 2008, covid; each
containing ols_coef_table, ols_adj_r2, rf_a_oob_rsq, rf_b_oob_rsq,
rf_b_importance_type1, lambda_min, lambda_1se, band_ols_coef_tables
(lower/middle/higher).
```

---

## Part B — placebo forest, matched predictor set

Refit: `randomForest(log_outcome ~ studied_z + controls_no_wealth, data = placebo_df, ntree = 500, importance = TRUE)`,
`set.seed(20260729)` immediately before the call. `placebo_df` n = 3078
(rebuilt identically to `modeling_andrew.qmd`'s cached pieces, read-only).

**Placebo (controls_no_wealth), type = 1 (%IncMSE), ranked descending:**

| Term | %IncMSE |
|---|---|
| pop_trend | 35.7146 |
| unemployment_rate_z | 35.1258 |
| log_population | 26.7788 |
| log_density | 25.6027 |
| proprietor_share_z | 25.4641 |
| rucc | 22.6998 |
| migration_rate_z | 18.4662 |
| working_age_share_z | 17.9266 |

**Three-column comparison, identical predictor set (placebo / 2008 rf_b / COVID rf_b), sorted by placebo:**

| Term | Placebo | 2008 rf_b | COVID rf_b |
|---|---|---|---|
| pop_trend | 35.7146 | 34.7445 | 23.9502 |
| unemployment_rate_z | 35.1258 | 15.3850 | 10.6429 |
| log_population | 26.7788 | 24.8308 | 10.7698 |
| log_density | 25.6027 | 18.1072 | 12.1924 |
| proprietor_share_z | 25.4641 | 19.9471 | 16.4860 |
| rucc | 22.6998 | 14.0642 | 10.9860 |
| migration_rate_z | 18.4662 | 17.6757 | 24.8404 |
| working_age_share_z | 17.9266 | 23.0284 | 15.4203 |

**OOB rsq:**

| Forest | OOB rsq |
|---|---|
| Placebo (controls_no_wealth) | 0.3005 |
| 2008 rf_b | 0.1219 |
| COVID rf_b | 0.3366 |

---

## Part C — band models with clustered errors

Refit: `lm_robust(log_outcome ~ studied_z + controls, data = df_band, clusters = state_fips, se_type = "stata")`,
`state_fips <- substr(fips, 1, 2)`. Same income-tercile bands as the cached
primary spec. Studied predictors only, all 6 bands.

| Crisis | Band | Term | Estimate | HC1 SE | HC1 p | Clustered SE | Clustered p | n_clusters | n_obs |
|---|---|---|---|---|---|---|---|---|---|
| 2008 | lower | proprietor_share_z | 0.01993 | 0.00657 | 0.00249 | 0.00597 | 0.00183 | 41 | 1025 |
| 2008 | lower | migration_rate_z | 0.00175 | 0.00870 | 0.84102 | 0.00792 | 0.82672 | 41 | 1025 |
| 2008 | lower | unemployment_rate_z | 0.01707 | 0.00443 | 0.00012 | 0.00865 | 0.05539 | 41 | 1025 |
| 2008 | lower | working_age_share_z | -0.01763 | 0.00548 | 0.00134 | 0.00627 | 0.00758 | 41 | 1025 |
| 2008 | middle | proprietor_share_z | 0.00733 | 0.00605 | 0.22596 | 0.00500 | 0.14972 | 44 | 1024 |
| 2008 | middle | migration_rate_z | 0.01270 | 0.00717 | 0.07698 | 0.00812 | 0.12544 | 44 | 1024 |
| 2008 | middle | unemployment_rate_z | -0.00380 | 0.00386 | 0.32564 | 0.00680 | 0.57935 | 44 | 1024 |
| 2008 | middle | working_age_share_z | -0.02457 | 0.00411 | 0.00000 | 0.00431 | 0.00000 | 44 | 1024 |
| 2008 | higher | proprietor_share_z | -0.00356 | 0.00664 | 0.59165 | 0.00466 | 0.44869 | 50 | 1024 |
| 2008 | higher | migration_rate_z | -0.00012 | 0.00790 | 0.98818 | 0.00566 | 0.98359 | 50 | 1024 |
| 2008 | higher | unemployment_rate_z | 0.00186 | 0.00532 | 0.72711 | 0.00650 | 0.77627 | 50 | 1024 |
| 2008 | higher | working_age_share_z | -0.00642 | 0.00631 | 0.30951 | 0.00565 | 0.26138 | 50 | 1024 |
| covid | lower | proprietor_share_z | 0.01161 | 0.00306 | 0.00016 | 0.00300 | 0.00039 | 42 | 1006 |
| covid | lower | migration_rate_z | 0.01254 | 0.00320 | 0.00010 | 0.00293 | 0.00011 | 42 | 1006 |
| covid | lower | unemployment_rate_z | 0.00009 | 0.00181 | 0.96171 | 0.00236 | 0.97094 | 42 | 1006 |
| covid | lower | working_age_share_z | -0.00439 | 0.00233 | 0.05953 | 0.00218 | 0.05081 | 42 | 1006 |
| covid | middle | proprietor_share_z | 0.01027 | 0.00237 | 0.00002 | 0.00276 | 0.00054 | 47 | 1005 |
| covid | middle | migration_rate_z | 0.00491 | 0.00272 | 0.07170 | 0.00300 | 0.10871 | 47 | 1005 |
| covid | middle | unemployment_rate_z | 0.00239 | 0.00239 | 0.31744 | 0.00360 | 0.50925 | 47 | 1005 |
| covid | middle | working_age_share_z | -0.00954 | 0.00223 | 0.00002 | 0.00341 | 0.00752 | 47 | 1005 |
| covid | higher | proprietor_share_z | 0.01225 | 0.00333 | 0.00025 | 0.00406 | 0.00404 | 50 | 1006 |
| covid | higher | migration_rate_z | 0.00799 | 0.00393 | 0.04259 | 0.00413 | 0.05902 | 50 | 1006 |
| covid | higher | unemployment_rate_z | -0.00038 | 0.00312 | 0.90282 | 0.00564 | 0.94649 | 50 | 1006 |
| covid | higher | working_age_share_z | -0.00437 | 0.00240 | 0.06853 | 0.00248 | 0.08363 | 50 | 1006 |

---

## Part D — relaxed LASSO, reported properly

### 1. Penalization

`penalty <- ifelse(colnames(X) %in% studied_z, 1, 0)`, applied identically to
the relaxed fit (`cv.glmnet(..., relax = TRUE, penalty.factor = penalty)`).
**Penalized (penalty.factor = 1):** `proprietor_share_z`, `migration_rate_z`,
`unemployment_rate_z`, `working_age_share_z`.
**Unpenalized (penalty.factor = 0):** `income`, `poverty_rate`,
`log_population`, `log_density`, `pop_trend`, and all RUCC dummy columns
(`rucc2`...`rucc9`).

### 2. lambda / gamma / CV MSE

The relaxed fit object carries **two different `lambda.min` values** that
are not interchangeable: a top-level `$lambda.min` (identical to what a
non-relaxed `cv.glmnet` call would select) and `$relaxed$lambda.min` (the
lambda that is *jointly* optimal with `$relaxed$gamma.min`). They differ in
both crises. `$lambda.1se` and `$relaxed$lambda.1se` are identical in both
crises. Values below use `$relaxed$lambda.min`/`$relaxed$gamma.min` as the
authoritative relaxed-fit pair; the differing top-level value is listed for
reference since prior reporting (`modeling_andrew.qmd`'s own relax section)
paired the top-level `$lambda.min` with `$relaxed$gamma.min`.

**2008:**

| Quantity | Value |
|---|---|
| relaxed$lambda.min | 0.0006307011 |
| relaxed$gamma.min | 0 |
| CV MSE at (relaxed$lambda.min, gamma.min) | 0.01249518 |
| relaxed$lambda.1se | 0.003066848 |
| relaxed$gamma.1se | 1 |
| CV MSE at (relaxed$lambda.1se, gamma.1se) | 0.01270765 |
| [reference] top-level $lambda.min (differs) | 0.0002730155 |
| [reference] top-level $lambda.1se (identical to relaxed) | 0.003066848 |

**COVID:**

| Quantity | Value |
|---|---|
| relaxed$lambda.min | 0.0001594175 |
| relaxed$gamma.min | 0.25 |
| CV MSE at (relaxed$lambda.min, gamma.min) | 0.002659509 |
| relaxed$lambda.1se | 0.001790775 |
| relaxed$gamma.1se | 1 |
| CV MSE at (relaxed$lambda.1se, gamma.1se) | 0.002818473 |
| [reference] top-level $lambda.min (differs) | 0.0001098803 |
| [reference] top-level $lambda.1se (identical to relaxed) | 0.001790775 |

### 3. Coefficient tables (all terms, incl. controls), plain / relaxed / OLS

**2008, @ lambda.min** (plain LASSO evaluated at `relaxed$lambda.min` = 0.0006307011; relaxed at that lambda with gamma = 0):

| Term | Plain LASSO | Relaxed LASSO | OLS |
|---|---|---|---|
| (Intercept) | -0.103951 | -0.132172 | -0.128137 |
| proprietor_share_z | 0.002370 | 0.007557 | 0.007482 |
| migration_rate_z | 0.000000 | 0.000000 | 0.001416 |
| unemployment_rate_z | 0.000000 | 0.000000 | 0.003694 |
| working_age_share_z | -0.012505 | -0.015595 | -0.016354 |
| income | 0.000000 | 0.000000 | 0.000000 |
| poverty_rate | -0.003231 | -0.002966 | -0.003157 |
| log_population | 0.014424 | 0.015230 | 0.014898 |
| log_density | -0.009038 | -0.007887 | -0.008063 |
| rucc2 | -0.027518 | -0.025376 | -0.025360 |
| rucc3 | -0.031025 | -0.028462 | -0.028180 |
| rucc4 | -0.042231 | -0.038810 | -0.039430 |
| rucc5 | -0.037243 | -0.032615 | -0.032069 |
| rucc6 | -0.047237 | -0.044550 | -0.045083 |
| rucc7 | -0.037131 | -0.033349 | -0.033981 |
| rucc8 | -0.044498 | -0.043303 | -0.044281 |
| rucc9 | -0.035381 | -0.035288 | -0.035308 |
| pop_trend | 0.011243 | 0.010889 | 0.010382 |

**2008, @ lambda.1se** (= 0.003066848, gamma = 1):

| Term | Plain LASSO | Relaxed LASSO | OLS |
|---|---|---|---|
| (Intercept) | -0.089528 | -0.089528 | -0.128137 |
| proprietor_share_z | 0.000000 | 0.000000 | 0.007482 |
| migration_rate_z | 0.000000 | 0.000000 | 0.001416 |
| unemployment_rate_z | 0.000000 | 0.000000 | 0.003694 |
| working_age_share_z | 0.000000 | 0.000000 | -0.016354 |
| income | -0.000001 | -0.000001 | 0.000000 |
| poverty_rate | -0.003511 | -0.003511 | -0.003157 |
| log_population | 0.015878 | 0.015878 | 0.014898 |
| log_density | -0.010971 | -0.010971 | -0.008063 |
| rucc2 | -0.026282 | -0.026282 | -0.025360 |
| rucc3 | -0.027863 | -0.027863 | -0.028180 |
| rucc4 | -0.039118 | -0.039118 | -0.039430 |
| rucc5 | -0.033030 | -0.033030 | -0.032069 |
| rucc6 | -0.044682 | -0.044682 | -0.045083 |
| rucc7 | -0.035169 | -0.035169 | -0.033981 |
| rucc8 | -0.044049 | -0.044049 | -0.044281 |
| rucc9 | -0.031110 | -0.031110 | -0.035308 |
| pop_trend | 0.010740 | 0.010740 | 0.010382 |

**COVID, @ lambda.min** (= 0.0001594175, gamma = 0.25):

| Term | Plain LASSO | Relaxed LASSO | OLS |
|---|---|---|---|
| (Intercept) | -0.009566 | -0.016544 | -0.017428 |
| proprietor_share_z | 0.009999 | 0.010694 | 0.010943 |
| migration_rate_z | 0.008475 | 0.009228 | 0.009427 |
| unemployment_rate_z | 0.000000 | 0.000000 | 0.000406 |
| working_age_share_z | -0.004902 | -0.005546 | -0.005776 |
| income | 0.000000 | 0.000000 | 0.000000 |
| poverty_rate | -0.000561 | -0.000486 | -0.000497 |
| log_population | 0.004281 | 0.004550 | 0.004571 |
| log_density | 0.000720 | 0.000862 | 0.000941 |
| rucc2 | -0.008322 | -0.008028 | -0.008098 |
| rucc3 | -0.013423 | -0.012962 | -0.012991 |
| rucc4 | -0.014448 | -0.013926 | -0.013941 |
| rucc5 | -0.011520 | -0.010591 | -0.010410 |
| rucc6 | -0.016628 | -0.016197 | -0.016245 |
| rucc7 | -0.017834 | -0.017200 | -0.017196 |
| rucc8 | -0.014858 | -0.014801 | -0.015099 |
| rucc9 | -0.025564 | -0.025418 | -0.025605 |
| pop_trend | 0.025134 | 0.024467 | 0.024322 |

**COVID, @ lambda.1se** (= 0.001790775, gamma = 1):

| Term | Plain LASSO | Relaxed LASSO | OLS |
|---|---|---|---|
| (Intercept) | 0.075153 | 0.075153 | -0.017428 |
| proprietor_share_z | 0.000000 | 0.000000 | 0.010943 |
| migration_rate_z | 0.000000 | 0.000000 | 0.009427 |
| unemployment_rate_z | 0.000000 | 0.000000 | 0.000406 |
| working_age_share_z | 0.000000 | 0.000000 | -0.005776 |
| income | -0.000001 | -0.000001 | 0.000000 |
| poverty_rate | -0.001303 | -0.001303 | -0.000497 |
| log_population | -0.000268 | -0.000268 | 0.004571 |
| log_density | -0.000084 | -0.000084 | 0.000941 |
| rucc2 | -0.011178 | -0.011178 | -0.008098 |
| rucc3 | -0.019068 | -0.019068 | -0.012991 |
| rucc4 | -0.021394 | -0.021394 | -0.013941 |
| rucc5 | -0.023595 | -0.023595 | -0.010410 |
| rucc6 | -0.021810 | -0.021810 | -0.016245 |
| rucc7 | -0.025749 | -0.025749 | -0.017196 |
| rucc8 | -0.014721 | -0.014721 | -0.015099 |
| rucc9 | -0.027181 | -0.027181 | -0.025605 |
| pop_trend | 0.033089 | 0.033089 | 0.024322 |

### 4. Entry path (largest lambda at which each studied predictor first becomes nonzero, plain path)

**2008:**

| Term | Lambda (first nonzero) | Coefficient there |
|---|---|---|
| working_age_share_z | 0.0027944 | -0.001392 |
| proprietor_share_z | 0.000915039 | 0.000084 |
| unemployment_rate_z | 0.000477102 | 0.000174 |
| migration_rate_z | 0.000188179 | 0.000066 |

**COVID:**

| Term | Lambda (first nonzero) | Coefficient there |
|---|---|---|
| proprietor_share_z | 0.00163169 | 0.001146 |
| migration_rate_z | 0.00148673 | 0.000386 |
| working_age_share_z | 0.00102475 | -0.000258 |
| unemployment_rate_z | 0.0000628776 | 0.000019 |

### 5. Six log-spaced lambdas, lambda.1se → lambda.min (plain LASSO path)

Plain (non-relaxed) path shown — a relaxed-path table at intermediate
lambdas is not uniquely defined without also fixing gamma at each point,
which was not specified.

**2008** (lambda.1se=0.003067, ..., lambda.min=0.000273):

| Term | λ=0.0030668 | λ=0.0018906 | λ=0.0011654 | λ=0.00071844 | λ=0.00044288 | λ=0.00027302 |
|---|---|---|---|---|---|---|
| proprietor_share_z | 0 | 0.000000 | 0.000000 | 0.001675 | 0.003924 | 0.005332 |
| migration_rate_z | 0 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| unemployment_rate_z | 0 | 0.000000 | 0.000000 | 0.000000 | 0.000440 | 0.001669 |
| working_age_share_z | 0 | -0.006071 | -0.009823 | -0.012077 | -0.013501 | -0.014540 |

**COVID** (lambda.1se=0.001791, ..., lambda.min=0.0001099):

| Term | λ=0.0017908 | λ=0.0010247 | λ=0.0005864 | λ=0.00033556 | λ=0.00019202 | λ=0.00010988 |
|---|---|---|---|---|---|---|
| proprietor_share_z | 0 | 0.005094 | 0.007580 | 0.009002 | 0.009813 | 0.010278 |
| migration_rate_z | 0 | 0.003072 | 0.005810 | 0.007377 | 0.008270 | 0.008782 |
| unemployment_rate_z | 0 | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 0.000000 |
| working_age_share_z | 0 | -0.000258 | -0.002611 | -0.003958 | -0.004726 | -0.005167 |

### 6. Plain vs. relaxed at lambda.1se

**Identical in both crises — max absolute coefficient difference across all
18 terms is 0** in 2008 and in COVID. Reason: `gamma.1se = 1` in both
crises (confirmed in Part D.2 above), and `gamma = 1` is defined by the
relaxed-LASSO formulation as pure shrunk LASSO with no de-biasing blend — at
`gamma = 1` the relaxed fit is mathematically the plain fit, so CV selecting
`gamma.1se = 1` makes the two outputs identical by construction, not by
coincidence of the data.
