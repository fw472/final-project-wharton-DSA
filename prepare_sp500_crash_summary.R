



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

  normalizePath(getwd(), mustWork = TRUE)
}

project_directory <- get_script_directory()
data_directory <- file.path(project_directory, "data")

prices <- read.csv(
  file.path(data_directory, "sp500_stocks_2007_2009.csv"),
  stringsAsFactors = FALSE
)
companies <- read.csv(
  file.path(data_directory, "sp500_companies.csv"),
  stringsAsFactors = FALSE
)

prices$date <- as.Date(prices$date)

last_close_in_year <- function(stock, year) {
  rows <- stock[format(stock$date, "%Y") == as.character(year), ]
  if (nrow(rows) == 0L) return(NA_real_)
  tail(rows$close, 1L)
}

close_on <- function(stock, date) {
  values <- stock$close[stock$date == as.Date(date)]
  if (length(values) == 0L) return(NA_real_)
  values[[1L]]
}

summarize_stock <- function(stock) {
  stock <- stock[order(stock$date), ]
  stock <- stock[!is.na(stock$close), ]

  close_2007 <- last_close_in_year(stock, 2007)
  close_2008 <- last_close_in_year(stock, 2008)
  benchmark_peak_close <- close_on(stock, "2007-10-09")
  benchmark_trough_close <- close_on(stock, "2009-03-09")

  running_peak <- cummax(stock$close)
  drawdown <- stock$close / running_peak - 1
  trough_index <- which.min(drawdown)
  peak_index <- which.max(stock$close[seq_len(trough_index)])

  data.frame(
    symbol = stock$symbol[[1L]],
    first_date = min(stock$date),
    last_date = max(stock$date),
    trading_days = nrow(stock),
    calendar_2008_return_pct = 100 * (close_2008 / close_2007 - 1),
    gfc_benchmark_return_pct =
      100 * (benchmark_trough_close / benchmark_peak_close - 1),
    max_drawdown_pct = 100 * drawdown[[trough_index]],
    max_drawdown_peak_date = stock$date[[peak_index]],
    max_drawdown_trough_date = stock$date[[trough_index]]
  )
}

summary_rows <- lapply(split(prices, prices$symbol), summarize_stock)
crash_summary <- do.call(rbind, summary_rows)
row.names(crash_summary) <- NULL

company_columns <- companies[
  ,
  c("symbol", "company", "sector", "sub_industry", "date_added")
]
crash_summary <- merge(
  crash_summary,
  company_columns,
  by = "symbol",
  all.x = TRUE,
  sort = TRUE
)

write.csv(
  crash_summary,
  file.path(data_directory, "sp500_crash_summary.csv"),
  row.names = FALSE,
  na = ""
)
