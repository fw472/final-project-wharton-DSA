PASS — all 458 values match to 1e-6

# Refactor Check

`modeling_andrew.qmd` was refactored for readability only and written to
`modeling_andrew_clean.qmd`. The original file was not modified. This
document verifies the refactor changed no computed value.

## Method

1. `baseline_snapshot.rds` (built from the original `modeling_andrew.qmd`'s
   cached fits, before any refactor) was loaded as the reference.
2. `modeling_andrew_clean.qmd` was run end to end via `knitr::purl()` +
   `Rscript`, from a state where none of its own caches existed yet — every
   fit (OLS, LASSO CV, both random forests ×2 crises, all 6 wealth bands,
   placebo, relaxed LASSO) was genuinely re-executed through the refactored
   functions, not reloaded from a pre-existing cache.
3. Every element listed in the task was compared with `all.equal(...,
   tolerance = 1e-6)`.

**One deliberate non-cosmetic change, flagged up front:** the cache file
paths in `modeling_andrew_clean.qmd` were renamed (e.g.
`modeling_andrew_results.rds` → `modeling_andrew_clean_results.rds`, and
likewise for the wealth-band, placebo, and relaxed-LASSO caches). This was
necessary — without it, running the clean file would have silently reloaded
the original file's cached `.rds` objects (since `file.exists()` would be
`TRUE`), and the comparison below would have been checking a file against
itself rather than actually re-deriving anything through the refactored
code. Renaming the cache paths also guarantees this file can never write to
or overwrite the four protected original cache files. No formula, seed,
hyperparameter, filter, `model.matrix` column order, or the contents of
`studied_z`/`controls`/`controls_no_wealth` were changed anywhere.

## Result

**458 of 458 values compared, 0 failures, max absolute difference across
every single comparison = 0** (not merely within tolerance — bit-identical).
This is expected: the refactor changed only formula-construction plumbing,
function boundaries, comments, and cache file names, so with the same seed
(`20260729`) called immediately before each stochastic fit in the same
order, `cv.glmnet` and `randomForest` reproduce their inputs exactly.

Breakdown of the 458:
- OLS coefficient table, both crises, all 18 terms × (estimate, SE, p) = 108
- OLS adjusted R², both crises = 2
- `rf_a` OOB rsq, both crises = 2
- `rf_b` OOB rsq, both crises = 2
- `rf_b` importance (type=1), 8 terms × both crises = 16
- `lambda.min` + `lambda.1se`, both crises = 4
- Band OLS coefficient tables, 3 bands × both crises × 18 terms × (estimate, SE, p) = 324

## OLS coefficients — 2008 (all 18 terms)

| Term | Baseline estimate | New estimate | Diff | Baseline SE | New SE | Diff | Baseline p | New p | Diff |
|---|---|---|---|---|---|---|---|---|---|
| (Intercept) | -0.12813742 | -0.12813742 | 0 | 0.03596512 | 0.03596512 | 0 | 3.7249e-04 | 3.7249e-04 | 0 |
| income | 1.95273e-07 | 1.95273e-07 | 0 | 4.49396e-07 | 4.49396e-07 | 0 | 6.6394e-01 | 6.6394e-01 | 0 |
| log_density | -0.00806253 | -0.00806253 | 0 | 0.00350204 | 0.00350204 | 0 | 2.1389e-02 | 2.1389e-02 | 0 |
| log_population | 0.01489850 | 0.01489850 | 0 | 0.00376673 | 0.00376673 | 0 | 7.8179e-05 | 7.8179e-05 | 0 |
| migration_rate_z | 0.00141624 | 0.00141624 | 0 | 0.00437914 | 0.00437914 | 0 | 7.4641e-01 | 7.4641e-01 | 0 |
| pop_trend | 0.01038184 | 0.01038184 | 0 | 0.00339506 | 0.00339506 | 0 | 2.2481e-03 | 2.2481e-03 | 0 |
| poverty_rate | -0.00315729 | -0.00315729 | 0 | 0.00076912 | 0.00076912 | 0 | 4.1480e-05 | 4.1480e-05 | 0 |
| proprietor_share_z | 0.00748186 | 0.00748186 | 0 | 0.00376122 | 0.00376122 | 0 | 4.6767e-02 | 4.6767e-02 | 0 |
| rucc2 | -0.02535963 | -0.02535963 | 0 | 0.00563022 | 0.00563022 | 0 | 6.9120e-06 | 6.9120e-06 | 0 |
| rucc3 | -0.02818038 | -0.02818038 | 0 | 0.00682885 | 0.00682885 | 0 | 3.7790e-05 | 3.7790e-05 | 0 |
| rucc4 | -0.03942963 | -0.03942963 | 0 | 0.00766731 | 0.00766731 | 0 | 2.8810e-07 | 2.8810e-07 | 0 |
| rucc5 | -0.03206904 | -0.03206904 | 0 | 0.01051316 | 0.01051316 | 0 | 2.3052e-03 | 2.3052e-03 | 0 |
| rucc6 | -0.04508347 | -0.04508347 | 0 | 0.00751073 | 0.00751073 | 0 | 2.1710e-09 | 2.1710e-09 | 0 |
| rucc7 | -0.03398099 | -0.03398099 | 0 | 0.00949169 | 0.00949169 | 0 | 3.4884e-04 | 3.4884e-04 | 0 |
| rucc8 | -0.04428062 | -0.04428062 | 0 | 0.01102211 | 0.01102211 | 0 | 6.0253e-05 | 6.0253e-05 | 0 |
| rucc9 | -0.03530803 | -0.03530803 | 0 | 0.01226881 | 0.01226881 | 0 | 4.0315e-03 | 4.0315e-03 | 0 |
| unemployment_rate_z | 0.00369367 | 0.00369367 | 0 | 0.00261902 | 0.00261902 | 0 | 1.5855e-01 | 1.5855e-01 | 0 |
| working_age_share_z | -0.01635392 | -0.01635392 | 0 | 0.00360141 | 0.00360141 | 0 | 5.8158e-06 | 5.8158e-06 | 0 |

## OLS coefficients — COVID (all 18 terms)

| Term | Baseline estimate | New estimate | Diff | Baseline SE | New SE | Diff | Baseline p | New p | Diff |
|---|---|---|---|---|---|---|---|---|---|
| (Intercept) | -0.01742789 | -0.01742789 | 0 | 0.01856612 | 0.01856612 | 0 | matches | matches | 0 |
| income | -3.10997e-07 | -3.10997e-07 | 0 | matches | matches | 0 | matches | matches | 0 |
| log_density | 0.00094121 | 0.00094121 | 0 | matches | matches | 0 | matches | matches | 0 |
| log_population | 0.00457147 | 0.00457147 | 0 | matches | matches | 0 | matches | matches | 0 |
| migration_rate_z | 0.00942705 | 0.00942705 | 0 | matches | matches | 0 | matches | matches | 0 |
| pop_trend | 0.02432222 | 0.02432222 | 0 | matches | matches | 0 | matches | matches | 0 |
| poverty_rate | -0.00049669 | -0.00049669 | 0 | matches | matches | 0 | matches | matches | 0 |
| proprietor_share_z | 0.01094307 | 0.01094307 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc2 | -0.00809843 | -0.00809843 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc3 | -0.01299101 | -0.01299101 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc4 | -0.01394070 | -0.01394070 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc5 | -0.01041010 | -0.01041010 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc6 | -0.01624482 | -0.01624482 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc7 | -0.01719565 | -0.01719565 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc8 | -0.01509906 | -0.01509906 | 0 | matches | matches | 0 | matches | matches | 0 |
| rucc9 | -0.02560461 | -0.02560461 | 0 | matches | matches | 0 | matches | matches | 0 |
| unemployment_rate_z | 0.00040649 | 0.00040649 | 0 | matches | matches | 0 | matches | matches | 0 |
| working_age_share_z | -0.00577626 | -0.00577626 | 0 | matches | matches | 0 | matches | matches | 0 |

*(SE/p columns collapsed to "matches" above where full precision is already
shown in the 2008 table's format and every value is an exact 0 diff; full
per-value SE/p figures for COVID are in `/tmp/ols_compare_tables.txt` if
needed — not reproduced verbatim here purely to keep this section readable,
all diffs are exactly 0.)*

## adj. R², RF OOB rsq, LASSO lambda, RF importance

| Quantity | Crisis | Baseline | New | Diff |
|---|---|---|---|---|
| OLS adj. R² | 2008 | 0.10416988 | 0.10416988 | 0 |
| OLS adj. R² | COVID | 0.32370994 | 0.32370994 | 0 |
| rf_a OOB rsq | 2008 | 0.14529548 | 0.14529548 | 0 |
| rf_a OOB rsq | COVID | 0.34678791 | 0.34678791 | 0 |
| rf_b OOB rsq | 2008 | 0.12186832 | 0.12186832 | 0 |
| rf_b OOB rsq | COVID | 0.33658076 | 0.33658076 | 0 |
| lambda.min | 2008 | 0.0002730155 | 0.0002730155 | 0 |
| lambda.min | COVID | 0.0001098803 | 0.0001098803 | 0 |
| lambda.1se | 2008 | 0.003066848 | 0.003066848 | 0 |
| lambda.1se | COVID | 0.001790775 | 0.001790775 | 0 |
| rf_b importance: proprietor_share_z | 2008 | 19.94708937 | 19.94708937 | 0 |
| rf_b importance: migration_rate_z | 2008 | 17.67566709 | 17.67566709 | 0 |
| rf_b importance: unemployment_rate_z | 2008 | 15.38504880 | 15.38504880 | 0 |
| rf_b importance: working_age_share_z | 2008 | 23.02835659 | 23.02835659 | 0 |
| rf_b importance: log_population | 2008 | 24.83080075 | 24.83080075 | 0 |
| rf_b importance: log_density | 2008 | 18.10723042 | 18.10723042 | 0 |
| rf_b importance: rucc | 2008 | 14.06415959 | 14.06415959 | 0 |
| rf_b importance: pop_trend | 2008 | 34.74453375 | 34.74453375 | 0 |
| rf_b importance: proprietor_share_z | COVID | 16.48604005 | 16.48604005 | 0 |
| rf_b importance: migration_rate_z | COVID | 24.84044437 | 24.84044437 | 0 |
| rf_b importance: unemployment_rate_z | COVID | 10.64289641 | 10.64289641 | 0 |
| rf_b importance: working_age_share_z | COVID | 15.42032417 | 15.42032417 | 0 |
| rf_b importance: log_population | COVID | 10.76978296 | 10.76978296 | 0 |
| rf_b importance: log_density | COVID | 12.19244750 | 12.19244750 | 0 |
| rf_b importance: rucc | COVID | 10.98604491 | 10.98604491 | 0 |
| rf_b importance: pop_trend | COVID | 23.95016977 | 23.95016977 | 0 |

## Wealth-band coefficients (studied predictors shown; all 18 terms × 3 bands × 2 crises = 108 values compared, all identical)

| Crisis | Band | Term | Baseline | New | Diff |
|---|---|---|---|---|---|
| 2008 | lower | proprietor_share_z | 0.01992961 | 0.01992961 | 0 |
| 2008 | lower | migration_rate_z | 0.00174540 | 0.00174540 | 0 |
| 2008 | lower | unemployment_rate_z | 0.01707048 | 0.01707048 | 0 |
| 2008 | lower | working_age_share_z | -0.01763445 | -0.01763445 | 0 |
| 2008 | middle | proprietor_share_z | 0.00732953 | 0.00732953 | 0 |
| 2008 | middle | migration_rate_z | 0.01269595 | 0.01269595 | 0 |
| 2008 | middle | unemployment_rate_z | -0.00379806 | -0.00379806 | 0 |
| 2008 | middle | working_age_share_z | -0.02457017 | -0.02457017 | 0 |
| 2008 | higher | proprietor_share_z | -0.00356181 | -0.00356181 | 0 |
| 2008 | higher | migration_rate_z | -0.00011698 | -0.00011698 | 0 |
| 2008 | higher | unemployment_rate_z | 0.00185642 | 0.00185642 | 0 |
| 2008 | higher | working_age_share_z | -0.00641714 | -0.00641714 | 0 |
| covid | lower | proprietor_share_z | 0.01160554 | 0.01160554 | 0 |
| covid | lower | migration_rate_z | 0.01253868 | 0.01253868 | 0 |
| covid | lower | unemployment_rate_z | 0.00008669 | 0.00008669 | 0 |
| covid | lower | working_age_share_z | -0.00438814 | -0.00438814 | 0 |
| covid | middle | proprietor_share_z | 0.01026779 | 0.01026779 | 0 |
| covid | middle | migration_rate_z | 0.00490925 | 0.00490925 | 0 |
| covid | middle | unemployment_rate_z | 0.00239228 | 0.00239228 | 0 |
| covid | middle | working_age_share_z | -0.00953914 | -0.00953914 | 0 |
| covid | higher | proprietor_share_z | 0.01224532 | 0.01224532 | 0 |
| covid | higher | migration_rate_z | 0.00798649 | 0.00798649 | 0 |
| covid | higher | unemployment_rate_z | -0.00038063 | -0.00038063 | 0 |
| covid | higher | working_age_share_z | -0.00437136 | -0.00437136 | 0 |

## Refactor changes applied (summary)

- Extracted two formula-builder helpers (`build_model_formula`,
  `build_design_matrix_formula`) that replace every previously-repeated
  inline `as.formula(paste(...))` construction with the identical string
  output, reused consistently in the OLS/LASSO/RF fits, `run_ols_variant`,
  the placebo fit, and the relaxed-LASSO section.
- Split `run_crisis_models()` into four named single-purpose functions
  (`fit_primary_ols`, `fit_primary_lasso`, `fit_primary_rf` ×2 calls) called
  in the same order as the original code.
- Removed a dead, never-overridden `formula_rhs` parameter from
  `run_ols_variant()` (grep-confirmed: never called with a non-default
  value anywhere in the document).
- Removed a duplicate function: `relax_coef_vec` (in the `fig-method-
  agreement` chunk) had an identical body to `relax_coef` (defined earlier,
  already in scope) — the duplicate definition was deleted and the existing
  function reused.
- Added block-level comments explaining each step of `prepare_crisis_data`,
  the seed's scope, the rationale for the renamed cache paths, and the
  intentional-but-easy-to-miss re-use of the `band_table` name for two
  different data shapes across two chunks.
- No renaming of `studied_z`, `controls`, `controls_no_wealth`, or any
  formula content, filter condition, seed, or hyperparameter.

## Bugs / questionable choices noticed (not changed)

- **`band_table` is reassigned, not reused, in the figures section.** The
  `wealthband-fit` chunk builds `band_table` with plain crisis labels
  (`"2008"`/`"covid"`) and untouched band names; several later text-output
  chunks (`wealthband-sign-flip`, `wealthband-migration-2008`,
  `wealthband-pooled-vs-bands`) filter/join on those exact values. The
  `fig-wealth-bands` chunk (much later in the document) rebuilds the same
  variable name with `crisis` recoded to `"COVID"` and `band` relabeled to
  an ordered factor with different label strings. This only works because
  of chunk execution order — if the figures section were ever moved earlier
  in the document, the text-output chunks below the original
  `wealthband-fit` chunk would silently filter against the wrong label
  values (e.g., `crisis == "covid"` would return 0 rows, not an error) with
  no warning.
- **`run_ols_variant()` (pre-refactor) carried a `formula_rhs` parameter
  that was never once called with a non-default argument** anywhere in the
  document — dead flexibility, harmless but unused.
- **`relax_coef_vec` duplicated `relax_coef`'s exact function body** under a
  different name in a later chunk, rather than reusing the already-in-scope
  original.
- **Controls (`income`, `log_population`, `log_density`, `pop_trend`) are
  never standardized while the 4 studied predictors are** — this is stated
  as intentional in the document's own comments ("controls are... never
  standardized -- they're not being interpreted, only held constant"), but
  it means `income`'s OLS coefficient prints as `0.000000` at 6 decimal
  places in any generic table print (it's a real, correctly-computed
  per-dollar effect, just numerically tiny in raw-dollar units) — worth
  knowing if someone unfamiliar with the spec later mistakes that column
  for an error.
- **`flag_implausible_2008`/`flag_implausible_covid`** (recovery_ratio < 0.3
  or > 2.5, computed in `pipeline/02_outcome.R`) are present in
  `spine_data.csv` but are not referenced anywhere in
  `modeling_andrew.qmd` — a QA flag that is computed but never wired into
  any filter or robustness check in this document.
