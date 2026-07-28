# 00_setup.R
#
# Run this first. Checks packages, raw_data/ cache folder, and a real Census
# API key. Run all scripts with your R working directory set to the
# "Final Project (econ cycles)" folder (go up one level out of pipeline/).

options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  "dplyr", "tidyr", "readr", "readxl", "httr", "jsonlite"
)

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

if (!dir.exists("raw_data")) dir.create("raw_data")

census_key <- Sys.getenv("CENSUS_API_KEY")
if (census_key == "" || census_key == "YOUR_KEY_HERE") {
  stop(
    "CENSUS_API_KEY is not set (or is still the placeholder).\n",
    "Get a free key at https://api.census.gov/data/key_signup.html and add ",
    "it to ~/.Renviron as:\n",
    "  CENSUS_API_KEY='your-real-key-here'\n",
    "then restart R so it gets loaded."
  )
}

message("Setup OK: packages installed, raw_data/ ready, Census key found (",
        nchar(census_key), " characters).")
