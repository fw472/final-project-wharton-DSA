

library(ggplot2)
library(scales)

get_script_directory <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)

  if (length(file_arg) > 0L) {
    script_path <- sub("^--file=", "", file_arg[[1L]])
    script_path <- gsub("~\\+~", " ", script_path)
    return(dirname(normalizePath(script_path, mustWork = TRUE)))
  }

  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else frame$ofile
    },
    character(1L)
  )
  frame_files <- frame_files[!is.na(frame_files)]

  if (length(frame_files) > 0L) {
    return(dirname(normalizePath(tail(frame_files, 1L), mustWork = TRUE)))
  }

  # Interactive fallback: use the current directory when the script path is
  # unavailable (for example, when code is pasted directly into the console).
  normalizePath(getwd(), mustWork = TRUE)
}

project_directory <- get_script_directory()
data_directory <- file.path(project_directory, "data")
figure_directory <- file.path(project_directory, "figures")
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

prices <- read.csv(
  file.path(data_directory, "sp500_stocks_2007_2009.csv"),
  stringsAsFactors = FALSE
)
prices$date <- as.Date(prices$date)
prices <- prices[order(prices$symbol, prices$date), ]

# Calculate each stock's daily return, then equally weight the available stocks.
# This avoids averaging raw prices, which are not comparable across companies.
prices$daily_return <- ave(
  prices$close,
  prices$symbol,
  FUN = function(close) c(NA_real_, close[-1L] / close[-length(close)] - 1)
)

market <- aggregate(
  daily_return ~ date,
  data = prices,
  FUN = function(x) mean(x, na.rm = TRUE)
)
market <- market[order(market$date), ]
market$index <- 100 * cumprod(1 + market$daily_return)

# Locate the equal-weighted basket's pre-crash peak and subsequent trough.
peak_candidates <- market$date <= as.Date("2008-06-30")
peak_row <- which.max(ifelse(peak_candidates, market$index, -Inf))
trough_candidates <- market$date >= market$date[peak_row] &
  market$date <= as.Date("2009-06-30")
trough_row <- which.min(ifelse(trough_candidates, market$index, Inf))

peak_date <- market$date[peak_row]
peak_value <- market$index[peak_row]
trough_date <- market$date[trough_row]
trough_value <- market$index[trough_row]
decline_pct <- 100 * (trough_value / peak_value - 1)

lehman_date <- as.Date("2008-09-15")
lehman_row <- which.min(abs(market$date - lehman_date))
lehman_value <- market$index[lehman_row]

plot <- ggplot(market, aes(date, index)) +
  annotate(
    "rect",
    xmin = peak_date,
    xmax = trough_date,
    ymin = -Inf,
    ymax = Inf,
    fill = "#E63946",
    alpha = 0.10
  ) +
  geom_line(linewidth = 1.05, color = "#173F5F") +
  geom_point(
    data = market[c(peak_row, trough_row), ],
    size = 3,
    color = "#C62828"
  ) +
  geom_vline(
    xintercept = lehman_date,
    linetype = "dashed",
    linewidth = 0.6,
    color = "#555555"
  ) +
  annotate(
    "label",
    x = peak_date,
    y = peak_value + 5,
    label = paste0(
      "Pre-crash peak\n",
      format(peak_date, "%b %d, %Y")
    ),
    hjust = 0,
    size = 3.5,
    fill = "white",
    linewidth = 0.2
  ) +
  annotate(
    "label",
    x = trough_date,
    y = trough_value - 6,
    label = paste0(
      "Market trough\n",
      format(trough_date, "%b %d, %Y"),
      "\n",
      number(decline_pct, accuracy = 0.1, suffix = "%"),
      " from peak"
    ),
    hjust = 1,
    size = 3.5,
    fill = "white",
    linewidth = 0.2
  ) +
  annotate(
    "text",
    x = lehman_date + 12,
    y = lehman_value + 8,
    label = "Lehman Brothers bankruptcy",
    hjust = 0,
    size = 3.4,
    color = "#444444"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    name = "Equal-weighted stock-price index (Jan. 2007 = 100)",
    breaks = pretty_breaks(7),
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  labs(
    title = "S&P 500 Stocks During the 2008 Market Crash",
    subtitle = paste0(
      "Daily data from 2007-2009 show the buildup, ",
      "peak-to-trough collapse, and early recovery"
    ),
    x = NULL,
    caption = paste0(
      "Source: DarkMatterNet. (2026). S&P 500 Stocks: 25 Years of Data ",
      "(Updated Daily) [Data set]. Kaggle. Retrieved July 28, 2026.\n",
      "https://www.kaggle.com/datasets/darkmatternet/",
      "s-and-p-500-stocks-25-years-of-data-updated-daily\n",
      "Equal-weighted current-constituent basket; survivorship bias applies. ",
      "The shaded area marks this basket's measured peak-to-trough decline."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#173F5F"),
    plot.subtitle = element_text(size = 11.5, margin = margin(b = 12)),
    plot.caption = element_text(size = 8.5, color = "#555555", hjust = 0),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(18, 24, 16, 18)
  )

ggsave(
  file.path(figure_directory, "sp500_2008_market_crash.png"),
  plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(figure_directory, "sp500_2008_market_crash.pdf"),
  plot,
  width = 12,
  height = 7,
  bg = "white"
)

write.csv(
  market,
  file.path(data_directory, "sp500_equal_weight_index_2007_2009.csv"),
  row.names = FALSE
)

cat(
  sprintf(
    "Peak: %s (%.2f); trough: %s (%.2f); decline: %.1f%%\n",
    peak_date,
    peak_value,
    trough_date,
    trough_value,
    decline_pct
  )
)
