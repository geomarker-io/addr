test_that("packaged example fixtures stay small and deterministic", {
  nad_data <- nad_example_data()
  voter_data <- voter_addresses()

  expect_s3_class(nad_data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(nad_data), 5000L)
  expect_equal(ncol(nad_data), 7L)
  expect_identical(attr(nad_data, "nad_version"), 23L)
  expect_identical(attr(nad_data, "county_fips"), "39061")

  expect_type(voter_data, "character")
  expect_length(voter_data, 2000L)
  expect_equal(length(unique(voter_data)), 2000L)
  expect_identical(voter_data, unique(format(nad_data$nad_addr))[seq_len(2000L)])

  prepared <- nad_example_data(match_prepared = TRUE)
  expect_s3_class(prepared, "addr_match_index")
  expect_identical(attr(prepared, "addr_match_index_version"), 1L)
  expect_identical(attr(prepared, "nad_version"), 23L)
  expect_identical(attr(prepared, "county_fips"), "39061")
  expect_true(all(vapply(
    as.data.frame(nad_data$nad_addr),
    function(x) identical(x, toupper(x)),
    logical(1)
  )))
})


test_that("the hotspot fixture retains its licensed snapshot and notices", {
  d <- elh_data()
  expect_s3_class(d, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(d), 100L)
  expect_named(d, c(
    "position", "time_period", "end_date", "xplaintiff", "xstreet_clean",
    "filings", "top100", "lat", "lon"
  ))
  expect_true(all(d$time_period == as.Date("2022-09-01")))
  expect_true(all(d$end_date == as.Date("2023-08-31")))
  expect_identical(
    attr(d, "source"),
    paste0("https://eviction-lab-data-downloads.s3.amazonaws.com/",
           "ets/hotspots_reports.zip")
  )
  expect_identical(attr(d, "license"), "https://opendatacommons.org/licenses/by/1-0/")
  expect_match(attr(d, "attribution"), "Eviction Lab, Princeton University", fixed = TRUE)
  for (notice in c("DATA-SOURCES", "LICENSE.note", "ODC-BY-1.0")) {
    expect_true(file.exists(system.file(notice, package = "addr", mustWork = TRUE)))
  }
})
