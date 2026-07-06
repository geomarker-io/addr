test_that("can download files from tiger", {
  skip_if_offline()
  skip_on_cran()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  dl_file <- tiger_download(
    "TIGER2024/FEATNAMES/tl_2024_39061_featnames.zip"
  )
  expect_true(file.exists(dl_file))
})

test_that("tiger_download_file uses binary mode and checks download status", {
  dest <- tempfile(fileext = ".zip")
  url <- "https://example.test/file.zip"
  download_args <- NULL

  tiger_download_file(
    url,
    dest,
    download_file = function(url, dest, mode) {
      download_args <<- list(url = url, dest = dest, mode = mode)
      writeBin(charToRaw("zip"), dest)
      0L
    }
  )

  expect_equal(download_args$url, url)
  expect_equal(download_args$dest, dest)
  expect_equal(download_args$mode, "wb")
  expect_equal(rawToChar(readBin(dest, "raw", n = 3L)), "zip")

  expect_error(
    tiger_download_file(
      url,
      tempfile(fileext = ".zip"),
      download_file = function(url, dest, mode) 7L
    ),
    "download returned status 7",
    fixed = TRUE
  )
})

test_that("tiger_download uses HTTPS TIGER endpoint", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  tiger_path <- "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  expected_url <- paste0("https://www2.census.gov/geo/tiger/", tiger_path)
  expected_dest <- file.path(tools::R_user_dir("addr", "data"), tiger_path)
  download_args <- NULL

  local_mocked_bindings(
    tiger_download_file = function(url, dest) {
      download_args <<- list(url = url, dest = dest)
      writeBin(charToRaw("zip"), dest)
      invisible(dest)
    }
  )

  dl_file <- tiger_download(tiger_path)

  expect_equal(dl_file, expected_dest)
  expect_equal(download_args$url, expected_url)
  expect_false(identical(download_args$dest, expected_dest))
  expect_true(file.exists(expected_dest))
  expect_equal(rawToChar(readBin(expected_dest, "raw", n = 3L)), "zip")
})

test_that("tiger_download reports failed URL and destination", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  tiger_path <- "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  expected_url <- paste0("https://www2.census.gov/geo/tiger/", tiger_path)
  expected_dest <- file.path(tools::R_user_dir("addr", "data"), tiger_path)
  temp_dest <- NULL

  local_mocked_bindings(
    tiger_download_file = function(url, dest) {
      temp_dest <<- dest
      writeBin(charToRaw("partial"), dest)
      stop("network broke", call. = FALSE)
    }
  )

  expect_error(
    tiger_download(tiger_path),
    paste0(
      "failed to download TIGER file from `",
      expected_url,
      "` to `",
      expected_dest,
      "`: network broke"
    ),
    fixed = TRUE
  )
  expect_false(file.exists(temp_dest))
  expect_false(file.exists(expected_dest))
})

test_that("tiger_download reuses cached files without downloading", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  tiger_path <- "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  expected_dest <- file.path(tools::R_user_dir("addr", "data"), tiger_path)
  dir.create(dirname(expected_dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("cached"), expected_dest)
  downloaded <- FALSE

  local_mocked_bindings(
    tiger_download_file = function(url, dest) {
      downloaded <<- TRUE
      stop("should not download", call. = FALSE)
    }
  )

  dl_file <- tiger_download(tiger_path)

  expect_equal(dl_file, expected_dest)
  expect_false(downloaded)
  expect_equal(rawToChar(readBin(expected_dest, "raw", n = 6L)), "cached")
})

test_that("tiger_addr_feat() can download addr feat from tiger", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})

test_that("taf requires arrow for the dataset interface", {
  local_mocked_bindings(
    check_installed = function(pkg, reason = NULL) {
      stop(paste(pkg, reason), call. = FALSE)
    }
  )

  expect_error(taf("2025"), "arrow.*multi-file taf dataset")
})

test_that("taf_catalog reads installed ZIP county catalog", {
  catalog_root <- tempfile()
  withr::local_options(list(
    addr.taf_catalog_dir = file.path(catalog_root, "inst", "extdata")
  ))

  manifest <- tibble::tibble(
    county_fips = c("39061", "39017"),
    ZIP = c("45220", "45249"),
    zip3 = c("452", "452"),
    zip2 = c("20", "49"),
    n_ranges = c(10L, 4L),
    installed_at = c("2026-01-01 UTC", "2026-01-01 UTC")
  )
  taf_write_catalog(
    manifest,
    year = "2025",
    version = "v1",
    root = catalog_root
  )

  expect_equal(
    taf_catalog(year = "2025", version = "v1"),
    tibble::tibble(
      county_fips = c("39061", "39017"),
      ZIP = c("45220", "45249"),
      zip3 = c("452", "452"),
      zip2 = c("20", "49"),
      n_ranges = c(10L, 4L)
    )
  )
})

test_that("taf_needed_counties uses catalog and selected ZIP variants", {
  catalog_root <- tempfile()
  withr::local_options(list(
    addr.taf_catalog_dir = file.path(catalog_root, "inst", "extdata")
  ))

  taf_write_catalog(
    tibble::tibble(
      county_fips = c("39061", "39061", "21117", "39017"),
      ZIP = c("45220", "45219", "42520", "45221"),
      zip3 = c("452", "452", "425", "452"),
      zip2 = c("20", "19", "20", "21"),
      n_ranges = c(10L, 3L, 2L, 4L),
      installed_at = rep("2026-01-01 UTC", 4)
    ),
    year = "2025",
    version = "v1",
    root = catalog_root
  )

  needed <- taf_needed_counties(
    "45220",
    year = "2025",
    version = "v1",
    zip_variant = c("minus1", "swap")
  )

  expect_equal(
    needed[c("county_fips", "ZIP", "source_zip", "source_zip_variant")],
    tibble::tibble(
      county_fips = c("39061", "39061", "21117"),
      ZIP = c("45220", "45219", "42520"),
      source_zip = c("45220", "45220", "45220"),
      source_zip_variant = c("exact", "minus1", "swap")
    )
  )
})

test_that("taf_ensure installs only missing needed counties", {
  catalog_root <- tempfile()
  withr::local_options(list(
    addr.taf_catalog_dir = file.path(catalog_root, "inst", "extdata")
  ))
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))

  taf_write_catalog(
    tibble::tibble(
      county_fips = c("39061", "39017"),
      ZIP = c("45220", "45220"),
      zip3 = c("452", "452"),
      zip2 = c("20", "20"),
      n_ranges = c(10L, 4L),
      installed_at = c("2026-01-01 UTC", "2026-01-01 UTC")
    ),
    year = "2025",
    version = "v1",
    root = catalog_root
  )
  taf_write_county_zip_manifest(
    tibble::tibble(
      county_fips = "39061",
      ZIP = "45220",
      zip3 = "452",
      zip2 = "20",
      n_ranges = 10L,
      installed_at = "2026-01-01 UTC"
    ),
    year = "2025",
    version = "v1"
  )

  installed <- character()
  local_mocked_bindings(
    taf_install = function(county, ...) {
      installed <<- c(installed, county)
      invisible(county)
    }
  )

  missing <- taf_ensure(
    "45220",
    year = "2025",
    version = "v1",
    zip_variants = FALSE
  )

  expect_equal(installed, "39017")
  expect_equal(missing$county_fips, "39017")
})

test_that("tiger_addr_feat() works with existing user data dir", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})
