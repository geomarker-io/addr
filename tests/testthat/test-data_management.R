test_that("empty removal previews do not create data directories", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-empty-"))
  root <- tools::R_user_dir("addr", "data")
  expect_equal(nrow(nad_remove()), 0L)
  expect_equal(nrow(taf_remove()), 0L)
  expect_false(dir.exists(root))
  expect_error(nad_remove(dry_run = NA), "dry_run")
  expect_error(taf_remove(county = "../outside"), "FIPS")
  expect_error(taf_remove(version = "../../outside"), "version")
})

test_that("removal refuses a symlink outside the dataset", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  outside <- withr::local_tempfile()
  writeLines("retain this file", outside)
  link <- file.path(root, "county.parquet")
  expect_true(file.symlink(outside, link))
  expect_error(addr_processed_inventory(link, root), "inside the dataset")
  expect_identical(readLines(outside), "retain this file")
})

test_that("geocoding emits its retained-data notice once when data are missing", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-notice-"))
  previous <- geocode_data_state$notice_shown
  withr::defer(geocode_data_state$notice_shown <- previous)
  geocode_data_state$notice_shown <- FALSE
  expect_message(geocode_data_notice(tibble::tibble(county_fips = character()), "2025"), NA)
  missing <- tibble::tibble(county_fips = c("39061", "39061", "39017"))
  expect_message(geocode_data_notice(missing, "2025"), "2 missing county/counties", fixed = TRUE)
  expect_message(geocode_data_notice(missing, "2025"), NA)
  expect_false(dir.exists(tools::R_user_dir("addr", "data")))
})
