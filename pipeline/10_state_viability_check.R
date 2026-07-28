# 10_state_viability_check.R
#
# Same viability check as the county spine, at state level: distribution
# summary, best/worst recovering states per crisis (face validity), and a
# confirmation that all 50 states + DC have data.

library(dplyr)
library(readr)

data <- read_csv("state_spine.csv", show_col_types = FALSE)

stopifnot(nrow(data) == 51)
message("Confirmed: all 50 states + DC present (", nrow(data), " rows).")

describe_distribution <- function(x, label) {
  x <- x[!is.na(x)]
  q <- quantile(x, c(0, .25, .5, .75, 1))
  message(sprintf("\n=== %s: n = %d ===", label, length(x)))
  message(sprintf("min = %.3f | Q1 = %.3f | median = %.3f | Q3 = %.3f | max = %.3f",
                   q[1], q[2], q[3], q[4], q[5]))
  message(sprintf("mean = %.3f | sd = %.3f", mean(x), sd(x)))
}

describe_distribution(data$recovery_ratio_2008, "STATE 2008 RECOVERY RATIO")
describe_distribution(data$recovery_ratio_covid, "STATE COVID RECOVERY RATIO")

show_extremes <- function(data, ratio_col, label) {
  cols <- c("state_name", ratio_col)
  d <- data %>% filter(!is.na(.data[[ratio_col]])) %>% select(all_of(cols))

  message(sprintf("\n=== %s: BEST RECOVERING STATES ===", label))
  print(as.data.frame(d %>% arrange(desc(.data[[ratio_col]])) %>% slice_head(n = 10)), row.names = FALSE)

  message(sprintf("\n=== %s: WORST RECOVERING STATES ===", label))
  print(as.data.frame(d %>% arrange(.data[[ratio_col]]) %>% slice_head(n = 10)), row.names = FALSE)
}

show_extremes(data, "recovery_ratio_2008", "2008 CRISIS")
show_extremes(data, "recovery_ratio_covid", "COVID CRISIS")
