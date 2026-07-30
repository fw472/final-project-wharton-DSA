# Chart Guide

Every figure gets four things:

- **What it shows** — the plain summary, in one or two sentences
- **Anatomy** — what each mark, axis, color, and line means
- **How it's built** — the computation and the key code
- **Why it matters** — what it does for your project or presentation

Sections 1–2 are shared background, read once. Section 3 is the 41 exploration
charts, section 4 the 10 modeling charts. Section 5 has three conflicts to fix
before slides. Section 6 is a suggested slide set.

---

# 1. Two chart families

**Family A — 41 exploration charts.** Built by `graphs.R`, saved to `figures/01_*`
through `figures/41_*`, collected in `all_graphs.pdf`. These use the **raw
recovery ratio** shown as a percentage, where 100% means employment returned
exactly to its pre-crisis peak. They cover the full ~3,200-county dataset and
include two variables your final analysis dropped (education, birth rate).

**Family B — 10 modeling charts.** Built by `modeling_andrew.qmd` and
`modeling_optionc.qmd`. These use the **logged** recovery ratio, where 0 means
fully recovered, and the 3,012-county modeling sample.

Same underlying quantity, two different scales. A chart showing 100% and a chart
showing 0.0 mark the same thing.

**Why the split matters for slides:** Family A sets up the problem and shows what
the data looks like. Family B answers the research question. Don't put a Family A
regression next to a Family B regression — see §5.1 for what goes wrong.

---

# 2. Shared machinery

## 2.1 The two crises get stacked into one table

Your raw data has one row per county with separate columns per crisis
(`recovery_ratio_2008`, `recovery_ratio_covid`). The script stacks them:

```r
county_long <- bind_rows(
  county %>% transmute(fips, crisis = "Great Recession",
                       recovery_ratio = recovery_ratio_2008,
                       poverty_rate = poverty_rate_2007, ...),
  county %>% transmute(fips, crisis = "COVID-19",
                       recovery_ratio = recovery_ratio_covid,
                       poverty_rate = poverty_rate_2019, ...)
)
```

Each county now appears **twice**, once per crisis, with a `crisis` column saying
which. Note the second block pulls 2019 predictors while the first pulls 2007 —
the reshape is also what enforces "measure predictors before the crisis."

Because of this, `facet_wrap(~crisis)` splits any chart into side-by-side panels
with no extra work, and captions say "county-period observations" — roughly
double the county count, since each county contributes two rows.

## 2.2 The quality filter

```r
valid_county <- county_long %>%
  filter(!is.na(recovery_ratio),
         is.na(flag_implausible) | !flag_implausible)
```

Drops rows with no recovery ratio, and any flagged implausible (ratio under 0.3
or over 2.5). Your diagnostics found zero implausible counties, so this only
removes missing values in practice. `is.na(flag) | !flag` keeps rows where the
flag itself is missing rather than silently dropping them.

## 2.3 Colors and reference lines

```r
crisis_colors <- c("Great Recession" = "#355C7D",   # dark blue
                   "COVID-19"        = "#C44E52")   # brick red
```

**Blue is always 2008, red is always COVID**, across all 41 charts. Say it once
at the start of the presentation and the audience reads every later chart faster.

```r
recovery_reference <- geom_vline(xintercept = 1, linetype = "dashed")
```

The dashed line at 100% marks full recovery. Left of it, still below peak. Right
of it, recovered and grew. It appears on most charts.

Where charts use a color *gradient* instead:
- `scale_fill_gradient2(midpoint = 0)` — blue negative, white zero, red positive.
  Used for correlations and z-scores, where direction matters.
- `scale_fill_viridis_c(option = "C")` — the plasma ramp, dark purple through
  pink to yellow. Used for maps and heatmaps where only magnitude matters. Dark
  is low, bright yellow is high.

## 2.4 The plot registry

```r
plots <- list()
add_plot <- function(name, plot) { plots[[name]] <<- plot }
```

Charts get stored in a named list rather than saved immediately; one loop at the
end writes all the PNGs plus the combined PDF. The `<<-` assigns to the list
outside the function.

After `source("graphs.R")` you can type `plots[["33_recovery_quartile_transitions"]]`
to view any single chart without re-running everything.

## 2.5 Helper functions

Rather than copy-pasting near-identical charts, the script defines a pattern once
and calls it repeatedly: `make_predictor_plot()` for the eight scatters,
`make_ranking_plot()` for the four rankings, `make_recovery_map()` and
`make_never_map()` for the eight maps, `make_pca_scores()` for the PCA.

Worth mentioning if anyone asks about the code — it's why 51 charts fit in 1,661
readable lines instead of 6,000 unmaintainable ones.

## 2.6 Reading ggplot

Charts are built by stacking layers with `+`:

| Layer | Job |
|---|---|
| `ggplot(data, aes(x = a, y = b, color = c))` | which data; which column controls which visual property |
| `geom_point()`, `geom_col()`, `geom_tile()`, `geom_density()` | what marks to draw |
| `facet_wrap(~crisis)` | split into panels, one per group |
| `scale_*` | color palettes and axis formatting |
| `labs()` | titles, axis labels, caption |
| `theme_report()` | shared visual style |

`aes()` *maps* a column to a property. `aes(color = crisis)` means "let the crisis
column pick the color," not "make it one specific color." That distinction causes
most beginner ggplot confusion.

---

# 3. Family A: the 41 exploration charts

## Group 1 — What recovery looked like (01–08)

### 01_recovery_histograms

**What it shows.** How many counties ended up at each level of recovery, with the
two crises stacked one above the other. It is the first look at your outcome
variable and the chart that makes the project concrete.

**Anatomy.**
- **One bar** = a slice of the recovery-ratio range; its **height** = how many
  counties fell inside that slice
- **x-axis:** recovery ratio, 50%–200%. **y-axis:** county count
- **Dashed vertical line at 100%:** full recovery
- **Panels:** 2008 on top, COVID below

**How it's built.** The range is cut into 45 equal-width bins and counties are
tallied per bin. Each panel gets its own y-scale, so 2008's shorter bars aren't
flattened by COVID's tall spike — **the panels are not height-comparable**, only
shape-comparable.

```r
ggplot(valid_county, aes(recovery_ratio, fill = crisis)) +
  geom_histogram(bins = 45, color = "white", linewidth = 0.15) +
  recovery_reference +
  facet_wrap(~crisis, ncol = 1, scales = "free_y")
```

`color = "white"` is the thin border between bars. `fill = crisis` colors them
blue/red, with `guide = "none"` suppressing a legend since panel titles already
say which is which.

**Why it matters.** The 2008 bars pile up well left of the dashed line with a long
thin tail; COVID is a narrow spike sitting right at it. That single visual makes
two of your findings obvious before you say a word: most counties did not fully
recover from 2008, and the two crises produced very differently shaped outcomes.
Good opening slide.

### 02_recovery_density

**What it shows.** The same information as chart 01, but as smooth curves laid on
top of each other instead of separate panels — which makes the difference in
**spread** between the two crises immediately visible.

**Anatomy.**
- **One curve** = one crisis
- **x-axis:** recovery ratio. **y-axis:** density — **not** a count
- **Both curves share one panel** so they overlap

**How it's built.** Kernel density estimation slides a small window across the data
and measures how crowded each point is. The area under each curve equals 1, which
is the point: it removes the count difference between crises so the **shapes**
compare directly.

```r
geom_density(alpha = 0.18, linewidth = 1, adjust = 1.1)
```

`alpha = 0.18` makes fills 18% opaque so overlapping curves stay visible.
`adjust = 1.1` widens the smoothing window 10% — higher is smoother, lower is
bumpier.

**Why it matters.** COVID's curve is tall and narrow because the same total area is
squeezed into a smaller width. That is your **SD 0.062 versus 0.118** finding,
drawn. The dispersion gap is what makes your predictability comparison defensible
(2008 had more variation to explain and still explained less of it), so having a
chart that shows it plainly is worth a slot.

### 03_recovery_boxplots

**What it shows.** The most compressed possible summary of both crises — median
and middle range, side by side.

**Anatomy.**
- **One box** = one crisis
- **Box top and bottom:** 75th and 25th percentile. The box holds the middle 50%
  of counties
- **Line inside the box:** the median
- **Whiskers:** extend to the furthest county within 1.5 × the box height
- **Dots beyond the whiskers:** individual outlier counties
- **y-axis:** recovery ratio. **Horizontal dashed line at 100%:** full recovery

```r
geom_boxplot(width = 0.58, outlier.alpha = 0.25, outlier.size = 1)
```

Outliers faded to 25% opacity so thousands of them don't swamp the boxes.

**Why it matters.** Both boxes sit mostly below the line and the 2008 box is
noticeably taller. It says the same things as charts 01 and 02 in less space, but
also less vividly. Use it only if you need a very compact summary slide —
otherwise 01 or 02 does more work.

### 04_recovery_ecdf

**What it shows.** For any recovery level you pick, what share of counties came in
at or below it. This is the chart that answers "what fraction never got back to
peak?" with an exact number.

**Anatomy.**
- **One line** = one crisis, always rising left to right
- **x-axis:** recovery ratio. **y-axis:** share of counties at or below that ratio
- **How to read it:** pick any x value, go up to the curve, read across. At
  x = 100%, the y value is the share that failed to return to peak

**How it's built.** `stat_ecdf()` sorts every county by recovery ratio and plots the
running proportion — county 1 of 3,000 gives 1/3000, county 1,500 gives 0.5, and
so on. No binning and no smoothing, so nothing is lost.

**Why it matters.** It's the most precise of the four distribution charts and the
least intuitive. Use it when you want to state an exact share out loud; use 01 or
02 when you want the audience to feel the shape. The gap between the two curves at
x = 100% is the cleanest statement of how differently the crises played out.

### 05_county_crisis_comparison

**What it shows.** Whether the counties that recovered well from 2008 are the same
counties that recovered well from COVID. It puts your research question on a
single chart, before any modeling.

**Anatomy.**
- **One dot** = one county
- **x-axis:** that county's 2008 recovery ratio. **y-axis:** its COVID recovery
  ratio
- **Dashed diagonal:** where the two would be equal. Above it = recovered better
  from COVID; below = better from 2008
- **Color:** orange if peak employment was under 1,000 in *either* crisis, blue
  otherwise — your small-county noise flag, made visible
- **Sample:** 3,214 counties with valid outcomes in both crises

```r
geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
coord_equal(xlim = paired_limits, ylim = paired_limits)
```

`coord_equal` forces identical scales on both axes — without it the 45° diagonal
would no longer mean "equal," and the chart would lie. `paired_limits` comes from
`range()` across both columns so both axes span the same values.

**Why it matters.** **This is one of your strongest charts and it's underused.**
Most dots sit above the diagonal (better COVID recovery), but the cloud is wide
and round rather than a tight band. A tight band would mean resilience is a fixed
property of a place. A round cloud means it isn't — which is exactly the
conclusion your cross-crisis Spearman test reached, visible here without a model.
The orange dots also show your reader where the small-county noise lives, which
preempts a likely question.

### 06_peak_to_trough_loss

**What it shows.** How big a hit each county took — the drop from its employment
peak to its bottom — separately for each crisis.

**Anatomy.**
- **One curve** = one crisis
- **x-axis:** share of employment lost from peak to bottom. **y-axis:** density
- **Dotted vertical at 0:** no loss
- Negative values in the left tail mean the county's trough was *above* its
  identified peak, possible when employment rose through the search window

**How it's built.** Computed during the reshape:

```r
peak_to_trough_loss = 1 - trough_employment / peak_employment
```

So 0.20 means the county lost 20% of its jobs between peak and trough.

**Why it matters.** This measures the size of the **hit**, while your recovery ratio
measures the **bounce**. Those are two different things that people constantly
conflate, and keeping them separate is part of why your outcome definition holds
up. Useful as a setup slide when explaining what a recovery ratio actually is.

### 07_trough_years

**What it shows.** Which year each county actually hit bottom — and that they
didn't all hit bottom at the same time.

**Anatomy.**
- **One bar** = one calendar year. **Height** = how many counties hit their
  employment low point that year
- **Panels:** each crisis gets its own x-range (2008 spans 2007–2013, COVID spans
  2019–2022)

**How it's built.** `count(crisis, trough_year)` tallies rows.
`factor(trough_year)` makes years evenly spaced categories rather than numbers.

**Why it matters.** This is a defensive slide. Your outcome uses each county's own
peak and trough rather than fixed national dates, and someone will ask why. The
answer is on this chart: counties bottomed out across a seven-year span in 2008.
A fixed national date would have measured many counties at the wrong moment.

### 08_never_recovered_share

**What it shows.** The share of counties and states whose employment never once
exceeded its pre-crisis peak, at any point through 2025.

**Anatomy.**
- **One bar** = one crisis at one geographic level. **Height** = the share
- **x-axis:** two groups, County and State/DC. **Bars dodged** within each group
  by crisis
- **Number above each bar:** the value, printed by `geom_text`
- **Values:** County 42.8% (2008) and 36.4% (COVID). State 3.9% and 9.8%

**How it's built.**

```r
summarise(share = mean(never_recovered))
```

`never_recovered` is TRUE/FALSE. R treats TRUE as 1 and FALSE as 0, so averaging
gives the proportion of TRUEs directly.

```r
scale_y_continuous(expand = expansion(mult = c(0, 0.12)))
```

Adds 12% headroom so the printed labels don't get clipped.

**Why it matters.** Two reasons. First, 42.8% of counties never coming back is a
striking number on its own — it's the scale of the problem your project studies.
Second, note the **reversal**: at county level 2008 looks worse, at state level
COVID does. That's an aggregation effect, since state totals average away local
collapse. It's a live illustration of why you analyze counties and states
separately instead of pooling them, which is a methodological choice you can
defend with this one chart.

**⚠️ Label carefully.** These use "never exceeded peak through 2025." Your modeling
outcome uses a fixed checkpoint and gives 78% and 51%. See §5.2.

## Group 2 — Recovery by group (09–10)

### 09_recovery_by_wealth_band

**What it shows.** Recovery split by how wealthy a county was before the crisis —
the obvious relationship your whole project is designed to look past.

**Anatomy.**
- **x-axis:** three income terciles (lower, middle, higher), computed separately
  for each crisis year. **y-axis:** recovery ratio
- **Two shapes per band**, dodged side by side, one per crisis:
  - **The violin** (wide outer shape): a density curve mirrored around its center.
    Where it bulges, many counties sit at that recovery level
  - **The narrow box inside:** the same median and interquartile range as a
    standard boxplot
- **Horizontal dashed line at 100%:** full recovery

```r
geom_violin(position = position_dodge(width = 0.8), alpha = 0.55,
            trim = TRUE, scale = "width") +
geom_boxplot(position = position_dodge(width = 0.8), width = 0.16,
             outlier.shape = NA)
```

`scale = "width"` makes every violin the same maximum width so shapes compare even
when group sizes differ. `outlier.shape = NA` hides the box's outlier dots since
the violin already shows the tails. Both layers use the same dodge width so the
box lands centered inside its violin.

**Why it matters.** Recovery clearly rises with income in both crises. This is the
finding you deliberately set aside as boring and already well documented. **Show
it anyway.** Putting it on screen, naming it as obvious, and saying "that's why
wealth is a control in every one of our models" is the move that earns you
credibility for everything after. It shows you know what the trivial answer is and
chose to look past it, rather than never having found it.

### 10_recovery_by_rural_urban_code

**What it shows.** Recovery across all nine USDA rural–urban categories, from
large metro counties to remote rural ones.

**Anatomy.**
- **One box** = one crisis within one category, dodged in pairs
- **x-axis:** all nine categories, labeled readably. **y-axis:** recovery ratio
- Standard boxplot parts: box = middle 50%, line = median, whiskers = 1.5× the
  box height, dots = outliers faded to 12% opacity

The labels come from a lookup applied during the reshape:

```r
rucc_labels <- c("1" = "Metro: 1M+", "2" = "Metro: 250K–1M", "3" = "Metro: <250K",
                 "4" = "Nonmetro: 20K+, adjacent", "5" = "Nonmetro: 20K+, remote",
                 "6" = "Nonmetro: 2.5K–20K, adjacent", "7" = "Nonmetro: 2.5K–20K, remote",
                 "8" = "Rural: adjacent", "9" = "Rural: remote")
```

**Why it matters.** The pattern isn't a straight downhill slope — it zigzags,
because codes 4 through 9 alternate on whether a county sits next to a metro area.
That zigzag is the visual justification for a specific modeling decision: your
models treat RUCC as an unordered `factor()` rather than a number, because a
straight-line assumption would be wrong. If anyone asks why you didn't just use
1–9 as a continuous variable, this chart is the answer.

## Group 3 — One predictor at a time (11–18)

**What this group shows.** Eight charts, each plotting one predictor against
recovery, so you can see the raw relationship before any controls enter. Together
they're the bridge between "here's our data" and "here's our model."

All eight come from the same helper, so the anatomy is identical.

**Anatomy.**
- **One dot** = one county-period observation (a county in one crisis)
- **x-axis:** the predictor. **y-axis:** recovery ratio
- **Solid line:** an ordinary least squares best-fit line — the single straight
  line minimizing squared vertical distance to the dots
- **Shaded band around the line:** the 95% confidence interval for the line's
  position. Narrow means well-determined, wide means uncertain
- **Horizontal dashed line at 100%:** full recovery
- **Panels:** one per crisis. **Caption:** the observation count

```r
make_predictor_plot <- function(data, variable, title, x_label, x_labels, log_x = FALSE) {
  ggplot(plot_data, aes(x = .data[[variable]], y = recovery_ratio, color = crisis)) +
    geom_point(alpha = 0.28, size = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
    facet_wrap(~crisis) + ...
}
```

`.data[[variable]]` lets one function accept any column name as text.
`alpha = 0.28` fades dots so dense regions read as dark clouds instead of a solid
blob. `method = "lm"` is the OLS line; `se = TRUE` draws the band. When
`log_x = TRUE`, the axis switches to `scale_x_log10()` and non-positive values are
dropped first, with a console message reporting how many.

| # | Predictor | n | What the line does |
|---|---|---|---|
| 11 | Poverty rate | 6,258 | Clear downward slope, both crises |
| 12 | Median income (log x) | 6,258 | Upward, both crises |
| 13 | Population (log x) | 6,258 | Nearly flat |
| 14 | Density (log x) | 6,257 | Nearly flat |
| 15 | Proprietor share | 6,156 | **Nearly flat in 2008** |
| 16 | Net migration | 6,197 | **Clearly steeper in COVID** |
| 17 | Bachelor's degree | 1,593 | Note the n |
| 18 | Birth rate | 6,258 | Cut from the final analysis |

**Why charts 11–14 matter.** They're your controls, and they behave exactly as
expected — poverty down, income up, population and density roughly flat. Boring is
the right outcome here. Showing one or two proves your data is sane.

**Why chart 15 matters most.** The raw relationship between self-employment and
2008 recovery is essentially flat, and the correlation in chart 19 is 0.01. But
your model, controlling for income and the rest, finds **+0.0075 and significant**.
That gap **is** the argument for multiple regression: the relationship was masked
because self-employment concentrates in poorer, more rural counties that recover
worse for unrelated reasons. Put chart 15 beside your coefficient plot and you
have a genuinely sophisticated point that most high school projects never make.

**Why chart 16 matters.** The COVID trend line is visibly steeper than the 2008
one. Your sharpest finding — migration mattering in one crisis and not the other —
is already visible here with no statistics at all.

**Why chart 17 matters.** Its n is 1,593, roughly 780 counties × 2 crises. That is
the single-year ACS 65,000-population cutoff showing up as a number. It's the
visual answer to "why isn't education in your main model?"

## Group 4 — Everything against everything (19)

### 19_predictor_correlations

**What it shows.** How every variable relates to every other variable, and to
recovery, in one grid per crisis. It's the single densest chart in your gallery.

**Anatomy.**
- **One tile** = one pair of variables. **The number printed in it and its color**
  = the Pearson correlation between them
- **Color scale:** red positive, white zero, blue negative, fixed from −1 to +1
- **The diagonal** is all 1.00 — every variable correlates perfectly with itself
- **The matrix is symmetric:** the tile above the diagonal and its mirror below
  carry the same number
- **Panels:** one per crisis

**What a correlation is.** A number from −1 to +1 for how tightly two variables move
together. +1 = perfect lockstep, 0 = no linear relationship, −1 = perfect
opposition. It measures only *straight-line* association and says nothing about
causation.

```r
correlation <- cor(matrix_data, use = "pairwise.complete.obs")
as.data.frame(as.table(correlation)) %>%
  transmute(predictor_x = Var1, predictor_y = Var2, correlation = Freq)
```

`pairwise.complete.obs` computes each pair using every county where **both**
variables exist, rather than dropping a county entirely for one missing value.
`as.table()` then `as.data.frame()` flattens the square matrix into three columns —
row, column, value — the shape ggplot needs for tiles. `coord_equal()` keeps tiles
square. Income, population, and density are `log10()`-transformed first, since
correlation on raw skewed variables gets dominated by a handful of huge counties.

**Why it matters — three specific cells.**

- **Recovery × Migration: 0.12 in 2008, 0.42 in COVID.** Your sharpest finding, as
  a raw correlation. No model, no controls, no assumptions. If you want one number
  that makes the cross-crisis argument to a skeptical audience, it's this pair.
- **Recovery × Bachelor's: 0.30 in 2008, 0.09 in COVID.** Matches your Option C
  education result. But **Bachelor's × Log income is 0.65 and 0.70** — sitting in
  the same chart is the reason education can't be presented as a finding separate
  from wealth. You can show the problem and the evidence for it simultaneously.
- **Recovery × Proprietors: 0.01 and 0.11.** The near-zero that becomes significant
  once you control. Pairs with chart 15.

Also worth noting: Poverty × Log income at −0.82 and −0.84. Strong, expected, and
both are controls — which is the honest answer if someone asks about
multicollinearity in your main model.

## Group 5 — Named extremes (20–23)

**What this group shows.** The ten best and ten worst recovering counties (20, 21)
and states (22, 23), by name. It turns 3,000 abstract rows into recognizable
places.

**Anatomy.**
- **One row** = one county or state, named
- **The dot** sits at its recovery ratio; **the horizontal stem** runs from 0 out
  to the dot. This is a lollipop chart — the stem only guides the eye from label
  to value
- **Green rows** = the ten highest. **Red rows** = the ten lowest, in two stacked
  blocks
- **Dashed vertical at 100%:** full recovery

```r
ranked <- data %>% filter(crisis == crisis_name) %>% arrange(recovery_ratio)
extremes <- bind_rows(
  ranked %>% slice_head(n = 10) %>% mutate(group = "Lowest recovery"),
  ranked %>% slice_tail(n = 10) %>% mutate(group = "Highest recovery"))
```

Sort, take the first ten and last ten, label them.

```r
display_name = factor(display_name, levels = unique(display_name[order(recovery_ratio)]))
```

Without this line ggplot orders rows alphabetically. Setting factor levels by
recovery order is what makes the bars read as a ranking.

**Why the county charts (20, 21) matter.** They double as a data quality check. The
2008 list is topped by North Dakota — Williams, McKenzie, Mountrail, Dunn, Divide.
That's the Bakken shale boom, a real economic event, not a data error. The COVID
list is dominated by tiny Texas counties, with Loving County at 169 residents.
Being able to look at your extremes and explain every one of them is what
separates a checked analysis from an unchecked one.

**Why the state charts (22, 23) matter more for the presentation.** 2008's worst:
New Mexico, Georgia, Illinois, Delaware, Rhode Island. COVID's worst: Connecticut,
Maine, Illinois, New York, Massachusetts. Almost no overlap. That's your entire
thesis — different crises hurt different places — expressed in names an audience
already recognizes, with no statistics required.

## Group 6 — Maps (24–31)

**What this group shows.** The geography of recovery. Charts 24–27 color by how
fully a place recovered; 28–31 color by whether it ever recovered at all. County
and state versions of each.

**Anatomy.**
- **One shape** = one county or state
- **Charts 24–27** fill by recovery ratio on the viridis plasma ramp: dark
  purple/navy low, magenta middle, bright yellow high. **Grey** = no valid ratio
- **Charts 28–31** fill by a two-level category: pale green = recovered at some
  point, red = never recovered through 2025

**⚠️ Watch the legend range.** Each map sets its own scale from its own data. The
2008 state map runs 92%–100%; the COVID state map runs 98%–108%. **The same color
means different values on the two maps.** If you show them side by side, say so
out loud, or the audience will read the comparison backwards.

```r
county_map_fips <- usmap::us_map(regions = "counties", data_year = 2021)
do.call(usmap::plot_usmap, list(regions = regions, data = map_data,
                                values = "value", data_year = 2021))
```

The `data_year = 2021` pin matters: Connecticut replaced counties with planning
regions in 2022, so a newer vintage wouldn't match your FIPS codes. The script also
verifies the installed `usmap` supports `data_year` and stops with a clear message
if not, pre-filters to FIPS the map recognizes, and prints how many records were
dropped — so unmatched geography can't vanish silently. Alaska and Hawaii are
repositioned by `usmap`, which is why they sit below the mainland.

**Why the state maps (26, 27) matter.** The 2008 map shows Michigan and Ohio
darkest with Texas and North Dakota brightest — the Rust Belt story everyone
recognizes. The COVID map is a completely different picture, with the Northeast
dark and Utah, Idaho, Arizona bright. **Side by side these may be your single most
persuasive slide**, because they require zero statistical background and make the
crisis-specific vulnerability argument instantly.

**Why the never-recovered maps (28–31) matter.** They show that non-recovery is
geographically concentrated rather than scattered, which supports your structural
decline story and explains why you added a pre-crisis population trend control.

**Why the county maps (24, 25) are weaker.** 3,000 shapes are too small to read
individually, and a few extreme counties compress the color scale so most of the
map looks uniform. Keep them as backup.

## Group 7 — Derived and advanced views (32–41)

### 32_resilience_quadrants

**What it shows.** Whether how hard a county got hit predicted how well it bounced
back — and it separates those two things, which people constantly conflate.

**Anatomy.**
- **One bubble** = one county in one crisis
- **x-axis:** peak-to-trough employment loss (how hard it was hit).
  **y-axis:** recovery ratio (how well it bounced back)
- **Bubble area** (not radius) = baseline population
- **Color:** green = eventually recovered, red = never recovered through 2025
- **Horizontal dashed at 100%:** full recovery
- **Vertical dotted line:** that crisis's **median** peak-to-trough loss
- **The two lines cut the panel into four quadrants:** hit hard and recovered
  (upper right), hit hard and stayed down (lower right), hit lightly and recovered
  (upper left), hit lightly and stayed down (lower left)

```r
scale_size_area(max_size = 9)
```

Maps population to **area**, which is correct — the eye judges bubble area, not
radius. Using radius would exaggerate large counties dramatically.

```r
geom_vline(data = shock_medians, aes(xintercept = median_loss), inherit.aes = FALSE)
```

`inherit.aes = FALSE` stops the median lines from inheriting the bubble's size and
color mappings, which would error since that table has no population column.

**Why it matters.** All four quadrants are populated, which is the finding: a county
can take a massive hit and bounce back, or a small hit and stay down. Shock
severity does not determine recovery. That's a useful point because it implies
recovery depends on something about the place — which is exactly what your four
predictors are trying to measure.

### 33_recovery_quartile_transitions

**What it shows.** Whether a county's 2008 recovery ranking predicted its COVID
recovery ranking. It's your research question answered as a simple grid of
percentages.

**Anatomy.**
- **A 4×4 grid.** **x-axis:** which quarter a county fell into for 2008 recovery.
  **y-axis:** which quarter for COVID
- **Each cell** shows two numbers: the count of counties, and that count as a
  percentage of its **row**
- **Cell color** = the same percentage, on the plasma ramp

**How it's built.**

```r
recovery_quartile_2008 = ntile(recovery_ratio_2008, 4)
recovery_quartile_covid = ntile(recovery_ratio_covid, 4)
```

`ntile(x, 4)` sorts counties and splits them into four equal-sized groups.

```r
count(recovery_quartile_2008, recovery_quartile_covid) %>%
  group_by(recovery_quartile_2008) %>%
  mutate(row_share = count / sum(count))
```

Cross-tabulate, then divide each cell by its row total so **every row sums to
100%**.

**The key to reading it.** If 2008 recovery told you nothing about COVID recovery,
every cell would sit near **25%**. Above 25% is persistence, below is reversal.

**Why it matters.** The corners are 32.1% (worst-to-worst) and 30.6%
(best-to-best) — above 25%, so persistence is real, but only about a quarter more
than random chance. Off-diagonal cells run 18–27%, meaning plenty of counties
moved.

That is **your cross-crisis Spearman result in a form anyone can read without
knowing what a rank correlation is.** It answers "is resilience a property of a
place?" with "partly, but much less than you'd think." Strong candidate for your
closing slide, and easier to explain than `crosscrisis.png`.

### 34_recovery_gap_distribution

**What it shows.** For each county, how much better or worse it did in COVID
compared to 2008.

**Anatomy.**
- **One bar** = a slice of the gap range. **Height** = county count
- **x-axis:** `recovery_ratio_covid − recovery_ratio_2008`. Positive = better in
  COVID
- **Dashed vertical at 0:** no change between crises
- **Solid red vertical:** the median county's gap

Two reference lines because "no change" and "typical change" are different
questions — the distance between them tells you how far the typical county
shifted.

**Why it matters.** Most counties did better in COVID, but the spread runs in both
directions and is wide. Same conclusion as charts 05 and 33, from a third angle.
Use it if you want a simple one-variable version of the persistence story;
otherwise 33 says more.

### 35_structural_change_and_recovery_gap

**What it shows.** Whether a county changing in some way between 2007 and 2019 —
getting richer, more educated, more populated — predicted it recovering better
from COVID than from 2008.

**Anatomy.**
- **Nine panels**, one per characteristic
- **One dot** = one county. **x-axis:** how much that characteristic changed from
  2007 to 2019, as a z-score. **y-axis:** the recovery gap (COVID minus 2008)
- **Red line:** an OLS fit for that panel alone. **Band:** its 95% confidence
  interval. **Dashed horizontal at 0:** no change in recovery performance

**How the x-axis works.** Each metric is standardized **within itself**:

```r
group_by(metric) %>% mutate(change_z = as.numeric(scale(change)))
```

`scale()` subtracts the mean and divides by the standard deviation, so every panel
reads in standard deviations and all nine share one axis. Without this, "income
changed by $12,000" and "poverty changed by 2 points" couldn't sit on the same
scale. `pivot_longer()` stacks nine separate columns into one long table with a
`metric` label, which is what lets a single `facet_wrap` produce all nine panels.

**Why it matters — and its limits.** Most trend lines are close to flat, so this is
an honest negative result: how a county changed over the decade didn't much
predict its relative recovery performance. Worth having run. But note these are
*changes measured across both crises*, so they could never serve as predictors in
your design without leakage. **Appendix material** — interesting to have done, thin
as a slide.

### 36_standardized_quartile_profiles

**What it shows.** A profile of what a strong-recovery county looks like compared
to a weak one — and how that profile differs completely between the two crises.

**Anatomy.**
- **One tile** = one recovery quartile crossed with one characteristic
- **The number in it** = the average z-score of that characteristic among counties
  in that quartile
- **Color:** red above average, white average, blue below
- **Two stacked panels:** 2008 on top, COVID below

**How to read a number.** COVID's top quartile shows **0.68** for migration. That
means counties in the best COVID-recovery quarter averaged 0.68 standard
deviations above the typical county on net migration. The bottom quartile shows
−0.43.

**How it's built, in order:**

```r
group_by(crisis) %>%
  mutate(recovery_quartile = ntile(recovery_ratio, 4),
         across(all_of(profile_variables), ~ as.numeric(scale(.x)))) %>%
  group_by(crisis, recovery_quartile) %>%
  summarise(across(starts_with("z_"), mean))
```

1. Within each crisis, convert every characteristic to a z-score
2. Sort counties into four recovery quartiles
3. Average the z-scores within each quartile

Standardizing **before** averaging is what makes numbers comparable across columns
with different units.

**Why it matters.** The two panels tell different stories. In COVID, migration
dominates (+0.68 top, −0.43 bottom). In 2008, migration is nearly flat and
education dominates instead — the bottom quartile sits at **−0.96** on bachelor's
attainment and **−0.77** on high school.

**That independently corroborates both of your crisis-specific findings**, from a
completely different direction than the regression: migration marks COVID recovery,
education marks 2008 recovery. When a simple descriptive summary and a controlled
regression agree, the finding is much harder to dismiss as a modeling artifact.
Strong backup slide for the Q&A.

### 37_binned_predictor_relationships

**What it shows.** Whether each predictor's relationship with recovery is actually
a straight line, or whether it bends.

**Anatomy.**
- **Seven panels**, one per predictor
- **One point** = one decile — a group of about 10% of counties, **not** a single
  county
- **x-axis:** the median standardized value of the predictor inside that decile.
  **y-axis:** the mean recovery ratio of counties in it
- **Error bars:** mean ± 1.96 × standard error, the standard 95% interval
- **Lines** connect deciles in order. **Colors:** blue 2008, red COVID

**How it's built.**

```r
group_by(crisis, predictor) %>%
  mutate(predictor_z = as.numeric(scale(value)), decile = ntile(value, 10)) %>%
  group_by(crisis, predictor, decile) %>%
  summarise(predictor_z = median(predictor_z),
            mean_recovery = mean(recovery_ratio),
            se = sd(recovery_ratio) / sqrt(n()))
```

`ntile(value, 10)` splits into ten equal-sized groups. `se = sd / sqrt(n)` is the
standard error of the mean — how precisely that decile's average is pinned down.

**Why it matters.** A straight OLS line assumes the relationship *is* straight.
This chart doesn't assume that. Where the connected points **bend**, the true
relationship is curved and a single linear coefficient is an average over a shape
that isn't linear.

That's a direct explanation for something in your results: your random forest,
which handles curves natively, sometimes ranks predictors differently than your
regression does. If anyone asks why the two methods don't rank identically, this
chart is the answer.

### 38_standardized_model_coefficients

**What it shows.** Standardized OLS coefficients with confidence intervals for both
crises — visually almost identical to your main result chart.

**⚠️ Do not put this on a slide.** It is a different regression from your report,
on a quarter of the sample, missing two of your four studied predictors, and
including a variable your professor told you to cut. The two charts disagree. Full
detail in §5.1.

### 39_county_predictor_pca

**What it shows.** Whether high-recovery and low-recovery counties occupy genuinely
different regions of "economic structure space" once you compress eight predictors
down to two dimensions.

**Anatomy.**
- **One small dot** = one county, placed by its scores on two constructed axes
- **Large white-filled circles** = the centroid (average position) of each recovery
  quartile, labeled
- **Color:** recovery quartile. **Grey crosshairs:** the origin
- **Panels:** one per crisis, each with its own scales

**What PCA does, plainly.** You have eight predictors, so each county is a point in
eight-dimensional space, which can't be drawn. PCA finds the two directions through
that cloud along which counties differ most and uses them as x and y. It's a way of
flattening eight numbers into two while losing as little information as possible.

```r
pca <- prcomp(pca_data %>% select(-fips, -county_name, -recovery_ratio),
              center = TRUE, scale. = TRUE)
explained <- 100 * pca$sdev^2 / sum(pca$sdev^2)
```

`scale. = TRUE` is essential — without it, population (in millions) would swamp
poverty rate (in percent) purely because of units. Run separately per crisis so
each gets its own components.

**Why it matters, and why to skip it.** The quartile clouds overlap almost
completely and the centroids sit close together. That's an honest negative result —
county economic structure doesn't cleanly separate by recovery outcome, consistent
with your modest R² — but it's not a compelling slide.

Two practical problems. Explaining PCA well takes two minutes you probably don't
have, and the payoff is "the groups overlap." And the script computes `explained`,
the share of variance each axis captures, then **never displays it**. A PCA plot
without those percentages is close to uninterpretable. If you do use it, add them
to the axis labels first.

### 40_state_recovery_change

**What it shows.** Every state ranked by how much better or worse it did in COVID
versus 2008.

**Anatomy.**
- **One row** = one state
- **x-axis:** COVID recovery ratio minus 2008 recovery ratio
- **Stem** runs from 0 to the value; **dot** marks the value
- **Green** = stronger after COVID, **red** = weaker
- **States ordered** by the gap, worst at top
- **Dashed vertical at 0:** performed identically in both

```r
state_name = factor(state_name, levels = state_name[order(recovery_gap)])
```

Same ordering trick as the ranking charts — set factor levels by the value to
override alphabetical sorting. `theme_report(base_size = 8)` shrinks text to fit 51
labels; `panel.grid.major.y = element_blank()` removes horizontal gridlines that
would clutter 51 rows.

**Why it matters.** It's the cleanest ranked version of your cross-crisis argument,
using names an audience knows. North Dakota sits at the far negative end, which is
**not** a failure — it boomed in 2008 on shale, so it had an unusually high bar to
clear. Being able to explain that is a good demonstration that you understand your
own data. Florida, Utah, and Idaho anchor the positive end.

A cleaner alternative to the state maps if you'd rather show a ranked list than
geography, though the maps land harder visually.

### 41_rural_wealth_interaction

**What it shows.** Recovery at every combination of geography and wealth — nine
cells per crisis — revealing that the two factors interact rather than acting
independently.

**Anatomy.**
- **A 3×3 grid per crisis**
- **x-axis:** income tercile. **y-axis:** three geography groups
- **Each cell** shows the median recovery ratio and the number of counties in it
- **Cell color** = the median, on the plasma ramp

**How the geography groups are built:**

```r
rural_group = case_when(
  as.integer(rural_urban_code) <= 3 ~ "Metro",      # RUCC 1-3
  as.integer(rural_urban_code) <= 7 ~ "Nonmetro",   # RUCC 4-7
  TRUE ~ "Rural")                                    # RUCC 8-9
```

Nine RUCC codes collapsed into three, then `group_by()` and `median()` per cell.

**Printing n in every cell is good practice** — a median from 84 counties deserves
less confidence than one from 670, and this chart lets the reader see that
directly.

**Why it matters.** In 2008 the grid spans 89.5% (rural, lower income) to 97.3%
(metro, higher income) — nearly eight points. In COVID it compresses to
98.2%–102.1%, under four points.

**That compression is a real finding.** COVID's effects were far more uniform across
wealth and geography; 2008's depended heavily on where a county started. Your
wealth-band coefficient plot says the same thing in model terms, but this says it
in plain medians anyone can read. Excellent companion slide — show the model
version, then this one as the intuitive confirmation.

---

# 4. Family B: the 10 modeling charts

These come from the Quarto documents. All use the **logged** outcome — so **0 means
fully recovered**, not 100%.

## coefficient_plot.png — your main result

**What it shows.** The effect of each of your four studied predictors on recovery,
per crisis, after controlling for wealth and everything else. This is the chart
your entire project reduces to.

**Anatomy.**
- **One row** = one studied predictor. **The dot** = the estimated coefficient
- **The horizontal line through it** = the 95% confidence interval from HC1 robust
  standard errors
- **x-axis:** change in log recovery ratio per one-standard-deviation increase in
  the predictor. **Dashed vertical at 0:** no effect
- **Panels:** 2008 left, COVID right

**The single rule for reading it.** If the interval crosses the dashed line, that
effect can't be distinguished from zero.

**Why it matters.** Everything else in the presentation supports or qualifies this
chart. Working-age share sits furthest left in 2008 — largest effect, negative.
Migration's interval crosses zero in 2008 and sits clearly right of it in COVID,
which is your headline contrast in its most rigorous form. Spend the most time
here.

## method_agreement.png

**What it shows.** The same four coefficients estimated three different ways, so you
can see whether the answer depends on the method.

**Anatomy.**
- **Three marks per predictor**, stacked vertically:
  - **Blue circle with a line** — OLS with its confidence interval
  - **Orange triangle** — standard LASSO at `lambda.min`
  - **Green square** — relaxed LASSO at `lambda.min`
- **x-axis:** coefficient. **Dashed vertical at 0**
- Where a mark sits exactly on the dashed line, LASSO dropped that predictor
- The orange triangle sits consistently closer to zero than the green square —
  that's LASSO's shrinkage, and relaxed LASSO removing it

**Why it matters.** **The stacking is the message.** Where three marks sit on top of
each other, three mathematically different methods agree. That's much stronger
evidence than any single method, and it's the answer to "how do you know this isn't
just an artifact of using OLS?" Use it right after the main coefficient plot.

## lasso_path.png

**What it shows.** How LASSO actually works — coefficients getting squeezed toward
zero as the penalty increases, until they're dropped entirely.

**Anatomy.**
- **One line per studied predictor.** **x-axis:** log(lambda), the penalty strength,
  increasing left to right. **y-axis:** the standardized coefficient
- **Lines labeled** at the left edge
- **Dotted verticals** mark `lambda.min` and `lambda.1se`, the two cross-validated
  penalty choices
- **Working-age share is the dashed black line** — it's negative, so it runs below
  zero and rises toward it

**How to read it.** At the far left the penalty is near zero and coefficients sit at
full size. Moving right, the penalty grows and every line gets squeezed toward
zero. Where a line **hits** zero, that predictor has been dropped.

**Why it matters.** It's the chart that explains what LASSO *does* to someone who's
never seen it. Use it before `method_agreement.png` if your audience needs LASSO
introduced. Skip it if they already know, since it shows method rather than
findings.

## relaxed_lasso.png

**What it shows.** Relaxed LASSO coefficients on their own, with dropped predictors
sitting visibly at zero.

**Anatomy.** One dot per predictor, no confidence intervals, value labeled.
Predictors that were zeroed sit exactly on the dashed line at 0.

**Why it matters — barely.** Everything here is inside `method_agreement.png` with
more context, and relaxed LASSO only means something *next to* the other two
methods. Backup slide at most.

## rf_importance.png

**What it shows.** Which variables the random forest actually relies on to make
predictions, ranked, for each crisis.

**Anatomy.**
- **One horizontal bar** = one variable. **Length** = permutation importance
- **Bars sorted** longest to shortest within each panel
- **Blue** = your four studied predictors, **grey** = controls
- **x-axis:** percent increase in prediction error when that variable is randomly
  shuffled

**What permutation importance means.** The model makes its predictions, then one
variable's values get scrambled at random and predictions are made again. If error
jumps a lot, the model was relying on that variable. If error barely moves, it
wasn't.

**Why it matters.** Migration ranks **first for COVID and sixth for 2008**. A
completely different type of model — nonlinear, no regression assumptions — reaches
your regression's conclusion. That convergence is the strongest single piece of
evidence you have for the migration finding.

**Say out loud that population trend and log(population) are controls.** They rank
high, and without that sentence the audience will read them as findings.

## predictability.png

**What it shows.** How much of county recovery your model can explain in each crisis
— plus a calm, non-crisis period as a baseline.

**Anatomy.**
- **Two panels.** Left: out-of-bag R². Right: out-of-bag RMSE
- **Three bars each:** 2008, COVID, Placebo (2014–2017). **Values labeled** on each
  bar

**Out-of-bag** means each tree in the forest is scored only on the counties it never
saw during training — an honest accuracy estimate without a separate test set.
**R²** = share of variation explained, higher is better. **RMSE** = typical
prediction error in log units, lower is better. Both panels tell the same story
from opposite directions.

**Why it matters.** This is your most counterintuitive result. The placebo bar
(31.0%) sits nearly level with COVID (34.7%), while 2008 (14.5%) is far below both.
That means COVID recovery was about as predictable as an ordinary economic period —
completely normal — while the financial crisis was **half** as predictable as a
normal year.

Most people asked to guess would say a pandemic was the more chaotic event. The
data says the opposite. **Practice explaining this one** — it's the slide most
likely to draw a question, and the answer is genuinely interesting rather than
defensive.

## outcome_distributions.png

**What it shows.** The logged recovery ratio for both crises, with each crisis's
standard deviation labeled.

**Anatomy.** Histograms, one panel per crisis. **x-axis:** log(recovery ratio).
**y-axis:** density. **Dashed vertical at 0:** exactly recovered. **SD annotated**
in each panel.

**Why it matters.** It justifies the log transform and carries the dispersion
comparison (0.118 vs 0.062) that defends your predictability finding — 2008 had
more variation to explain and still explained less of it.

**Don't show this and chart 01 both.** Pick one. Chart 01 is easier for a general
audience (percentages, familiar scale); this one is correct for a methods slide and
carries the SD numbers.

## decomposition.png (figures_optionc/)

**What it shows.** Why your main analysis and your extension give different numbers
— by changing one thing at a time instead of two at once.

**Anatomy.**
- **Three marks per predictor**, each with a confidence interval:
  - **Blue circle** — Option A, full sample, 4 predictors
  - **Orange triangle** — Option C, same 4 predictors, reduced sample (rescaled)
  - **Green square** — Option C, full 21-predictor model (rescaled)
- **Reading left to right within a predictor:** blue → orange isolates the effect of
  **shrinking the sample**. Orange → green isolates the effect of **adding
  predictors**

All coefficients are standardized on Option A's scale so they're genuinely
comparable — otherwise part of any shift would be a units artifact rather than a
real change.

**Why it matters.** Without this chart, your two analyses look like they contradict
each other and someone will ask which one is right. With it, every difference is
attributable to a specific cause. It converts an apparent weakness into a
demonstration of careful method. **Essential if you present the extension at all.**

## industry_coefficients.png (figures_optionc/)

**What it shows.** Which industries a county's workforce was concentrated in, and
whether that predicted recovery — separately for each crisis.

**Anatomy.**
- **One row** = education or one of twelve industry sectors. **Dot** = coefficient,
  **line** = 95% interval
- **Rows sorted** by the 2008 coefficient
- **Filled circle** = VIF ≤ 5 (stable estimate). **Open circle** = VIF > 5 (treat
  cautiously)
- **Dashed vertical at 0.** Public administration is the omitted reference, so every
  coefficient means "relative to a county with more public administration
  employment"

**Why it matters.** It contains the single cleanest finding in your project.
Construction is significant and negative in 2008 and sits **exactly on zero in
COVID (p = 0.995)**. A crisis that started in housing hurt counties whose workers
built houses; a crisis that started with a virus did not touch them at all.
Information runs the reverse.

That's not just "different traits mattered in different crises" — it's a mechanism
anyone can understand, which is what makes it presentable.

**When presenting, point at construction specifically.** Thirteen rows is a lot to
absorb and the audience needs to be told where to look.

## crosscrisis.png (figures_optionc/)

**What it shows.** Whether a model trained on one crisis could have predicted the
other — the question a policymaker would actually ask.

**Anatomy.**
- **One dot** = one county. **x-axis:** its actual log recovery. **y-axis:** what a
  model trained on the *other* crisis predicted for it
- **Dashed 45° diagonal:** perfect prediction
- **Spearman rho annotated** in each panel. **Two panels:** the two directions

**Read the Spearman, not the visual fit.** The clouds sit off the diagonal, which
looks like failure but is just the level difference between crises — 2008 counties
averaged 7% below peak, COVID counties averaged right at peak, so a 2008-trained
model predicts everyone too low. Spearman ignores that level shift and asks only
whether the model **ranked** counties correctly.

**Why it matters.** It answers your project's own question about durability with a
number: the cross-crisis models retain about 57–60% of each crisis's own ranking
ability. So studying the last crisis gets you a little over half of what you'd want
to know about the next one. Some resilience is durable; a substantial share depends
on how the shock arrives.

**Without the Spearman explanation this chart reads as much worse than it is.** If
you're short on time, chart 33 makes the same point more legibly.

---

# 5. Three conflicts to fix before slides

## 5.1 ⚠️ Figure 38 contradicts your report

Figure 38 is titled "Multivariate predictors of county recovery" and subtitled
"Standardized OLS coefficients" — nearly identical to your `coefficient_plot.png`.
It is a different model:

| | `coefficient_plot.png` | Figure 38 |
|---|---|---|
| Outcome | log(recovery ratio) | raw ratio, standardized |
| Your 4 studied predictors | all 4 | **only 2** — no working-age share, no unemployment |
| Extra predictors | none | education, birth rate |
| Controls | includes RUCC and population trend | neither |
| Standard errors | HC1 robust | plain `lm()` |
| Sample | 3,073 / 3,017 | **769 / 800** |

The code:

```r
transmute(recovery = recovery_ratio, poverty, log_income, log_population,
          log_density, proprietor_share, migration, bachelors, birth_rate) %>%
  mutate(across(everything(), ~ as.numeric(scale(.x))))
fit <- lm(recovery ~ ., data = model_data)
```

The sample is a quarter the size because including `bachelors` caps it at counties
above 65,000. And `birth_rate` is in there — the variable your professor told you
to cut for having no theoretical justification.

**Leave it out.** If a teammate wants a coefficient chart, it should be
`coefficient_plot.png`. If figure 38 appears next to your results, the numbers
won't match and a faculty member will ask which regression you actually ran.

## 5.2 Two different "never recovered" numbers

- Figure 08: **42.8% and 36.4%** — employment never exceeded peak through 2025
- Your modeling outcome: **78% and 51%** — still below peak at the trough+2/3
  checkpoint

Both correct, measuring different things. Unlabeled together, it reads as an error.
Either pick one for the presentation or label them explicitly.

## 5.3 The gallery contains two variables you cut

Education appears in figures 17, 19, 36, 37, 38, 39. Birth rate in 18, 19, 35, 36,
37, 38.

Fine for exploration — looking is what EDA is for. But have the answer ready for
"why is education in your charts but not your model?" It's a good answer: it only
exists for about a quarter of counties before 2008, and where it exists it
correlates 0.68–0.73 with income.

---

# 6. Suggested slide set

Twelve charts, ordered as an argument.

**Setup**
1. `01_recovery_histograms` — the outcome, and how many counties never got back
2. `26` + `27` state maps side by side — different crises, different places
   *(say the color scales differ)*
3. `09_recovery_by_wealth_band` — yes, wealth matters; that's why we control it

**The question**
4. `05_county_crisis_comparison` or `33_recovery_quartile_transitions` — did the
   same counties recover from both? Barely.

**Results**
5. `coefficient_plot.png` — the main answer
6. `19_predictor_correlations` — migration 0.12 vs 0.42, no model required
7. `rf_importance.png` — different method, same conclusion
8. `method_agreement.png` — three methods agree

**The surprise**
9. `predictability.png` — COVID was ordinary; 2008 was the anomaly

**Depth**
10. `wealth_bands.png` — the unemployment effect pooling hid
11. `industry_coefficients.png` — construction, plus `decomposition.png` if time

**Close**
12. `crosscrisis.png` — the last crisis is a partial guide to the next

**Backup:** `41_rural_wealth_interaction`, `36_standardized_quartile_profiles`,
`04_recovery_ecdf`, `22`/`23` state rankings, `lasso_path.png`, the
standard/relaxed/OLS table, chart `15` beside the coefficient plot.

**Leave out:** `38` (conflicts), `39` (PCA — overlapping clouds, missing variance
labels), `35` (flat), `24`/`25` (county maps too dense), `relaxed_lasso.png`
(redundant), `02`/`03` (redundant with `01`).
