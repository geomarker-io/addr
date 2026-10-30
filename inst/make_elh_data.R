# Maintainer preparation from the provider's explicitly ODC-BY 1.0-licensed
# archive. See inst/DATA-SOURCES for attribution and redistribution terms.
# An optional local archive path allows regeneration without network access.
args <- commandArgs(trailingOnly = TRUE)
archive <- if (length(args)) args[[1L]] else tempfile(fileext = ".zip")
source_url <- paste0(
  "https://eviction-lab-data-downloads.s3.amazonaws.com/",
  "ets/hotspots_reports.zip"
)
if (!length(args)) {
  download.file(source_url, archive, mode = "wb")
}

csv <- unz(archive, "hotspots_reports/cincinnati_hotspots_media_report.csv")
d <- read.csv(csv, colClasses = c(
  "numeric", "character", "character", "character", "character",
  "numeric", "numeric", "numeric", "numeric"
), stringsAsFactors = FALSE)
d$time_period <- as.Date(d$time_period)
d$end_date <- as.Date(d$end_date)
stopifnot(
  nrow(d) == 100L, ncol(d) == 9L,
  all(d$time_period == as.Date("2022-09-01")),
  all(d$end_date == as.Date("2023-08-31"))
)
d <- tibble::as_tibble(d)
attr(d, "source") <- source_url
attr(d, "license") <- "https://opendatacommons.org/licenses/by/1-0/"
attr(d, "attribution") <- paste(
  "Contains information from the Eviction Lab, Princeton University,",
  "Eviction Tracking System, Cincinnati hotspot report for",
  "2022-09-01 through 2023-08-31, made available under ODC-BY 1.0."
)
saveRDS(d, file.path("inst", "extdata", "elh_data.rds"))
