test_that("can download files from tiger", {
  skip_live_tiger_downloads()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  dl_file <- tiger_download(
    "TIGER2024/FEATNAMES/tl_2024_39061_featnames.zip",
    subdir = "tiger_feat_names"
  )
  expect_true(file.exists(dl_file))
})

make_tiger_test_zip <- function(path) {
  source_dir <- tempfile("tiger-zip-source-")
  dir.create(source_dir)
  source_file <- file.path(source_dir, "fixture.txt")
  writeLines("fixture", source_file)
  old_dir <- setwd(source_dir)
  on.exit(setwd(old_dir), add = TRUE)
  utils::zip(path, basename(source_file), flags = "-q")
  invisible(path)
}

test_that("tiger_download fixes ownership and forwards managed-copy controls", {
  tiger_path <- "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  expected_url <- paste0("https://www2.census.gov/geo/tiger/", tiger_path)
  received <- NULL

  local_mocked_bindings(
    stow = function(...) {
      received <<- list(...)
      "/managed/canonical.zip"
    },
    .package = "stow"
  )

  dl_file <- tiger_download(
    tiger_path,
    subdir = "tiger_addr_feat",
    overwrite = TRUE,
    offline = FALSE
  )

  expect_identical(dl_file, "/managed/canonical.zip")
  expect_identical(received[[1]], expected_url)
  expect_identical(received$package, "addr")
  expect_identical(received$subdir, "tiger_addr_feat")
  expect_identical(received$overwrite, TRUE)
  expect_identical(received$offline, FALSE)
  expect_identical(received$etag, FALSE)
  expect_identical(received$validate, tiger_validate_zip)
})

test_that("public TIGER consumers own separate stow subdirectories", {
  received <- list()
  local_mocked_bindings(
    tiger_download = function(...) {
      received[[length(received) + 1L]] <<- list(...)
      "/managed/canonical.zip"
    }
  )

  expect_identical(
    tiger_feat_names_download("39061", "2025", TRUE),
    "/managed/canonical.zip"
  )
  expect_identical(
    tiger_addr_feat_download("39061", "2025", FALSE),
    "/managed/canonical.zip"
  )

  expect_identical(received[[1]]$subdir, "tiger_feat_names")
  expect_identical(received[[1]]$overwrite, TRUE)
  expect_identical(received[[2]]$subdir, "tiger_addr_feat")
  expect_identical(received[[2]]$overwrite, FALSE)
})

test_that("tiger ZIP validator accepts ZIPs and rejects other content", {
  zip_file <- tempfile(fileext = ".zip")
  make_tiger_test_zip(zip_file)
  invalid_file <- tempfile()
  writeLines("not a ZIP", invalid_file)

  expect_identical(tiger_validate_zip(zip_file), TRUE)
  expect_identical(tiger_validate_zip(invalid_file), FALSE)
})

test_that("tiger_download supports offline managed local copies", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  tiger_path <- "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  url <- tiger_download_url(tiger_path)
  managed_root <- stow::stow_path(package = "addr")
  managed_dir <- stow::stow_path(
    package = "addr",
    subdir = "tiger_feat_names"
  )
  expect_identical(basename(managed_root), "stow")
  expect_identical(dirname(managed_dir), managed_root)
  stow_filename <- getFromNamespace(".stow_url_to_filename", "stow")
  managed_copy <- file.path(managed_dir, stow_filename(url))
  make_tiger_test_zip(managed_copy)

  dl_file <- tiger_download(
    tiger_path,
    subdir = "tiger_feat_names",
    offline = TRUE
  )

  expect_identical(dl_file, normalizePath(managed_copy, winslash = "/"))
  expect_error(
    tiger_download(
      sub("internationalboundary", "coastline", tiger_path),
      subdir = "tiger_feat_names",
      offline = TRUE
    ),
    "No managed local copy is available in offline mode",
    fixed = TRUE
  )
})

test_that("tiger_download ignores the former unmanaged TIGER layout", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  tiger_path <- "TIGER2024/FEATNAMES/tl_2024_39061_featnames.zip"
  legacy_root <- dirname(stow::stow_path(package = "addr"))
  legacy <- file.path(legacy_root, tiger_path)
  dir.create(dirname(legacy), recursive = TRUE, showWarnings = FALSE)
  make_tiger_test_zip(legacy)

  expect_error(
    tiger_download(
      tiger_path,
      subdir = "tiger_feat_names",
      offline = TRUE
    ),
    "No managed local copy is available in offline mode",
    fixed = TRUE
  )
  expect_true(file.exists(legacy))
})

test_that("tiger_download does not discover stow 0.2 managed copies", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  tiger_path <- "TIGER2024/FEATNAMES/tl_2024_39061_featnames.zip"
  url <- tiger_download_url(tiger_path)
  managed_root <- stow::stow_path(package = "addr")
  old_managed_dir <- file.path(dirname(managed_root), "tiger_feat_names")
  dir.create(old_managed_dir, recursive = TRUE, showWarnings = FALSE)
  stow_filename <- getFromNamespace(".stow_url_to_filename", "stow")
  old_copy <- file.path(old_managed_dir, stow_filename(url))
  make_tiger_test_zip(old_copy)

  expect_error(
    tiger_download(
      tiger_path,
      subdir = "tiger_feat_names",
      offline = TRUE
    ),
    "No managed local copy is available in offline mode",
    fixed = TRUE
  )
  expect_true(file.exists(old_copy))
})

test_that("tiger_addr_feat() can download addr feat from tiger", {
  skip_live_tiger_downloads()
  skip_on_ci()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})

test_that("taf_dataset requires arrow for the dataset interface", {
  local_mocked_bindings(
    check_installed = function(pkg, reason = NULL) {
      stop(paste(pkg, reason), call. = FALSE)
    }
  )

  expect_error(taf_dataset("2025"), "arrow.*multi-file taf dataset")
})

test_that("taf reads installed Parquet files by ZIP code", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-taf-data-"))
  year <- "2025"
  version <- "v1"
  path <- file.path(
    taf_dataset_path(year = year, version = version),
    "zip3=452",
    "zip2=20",
    "39061.parquet"
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  nanoparquet::write_parquet(
    tibble::tibble(
      LINEARID = "fixture-line",
      FULLNAME = "MAIN ST",
      side = "L",
      ZIP = "45220",
      FROMHN = 1L,
      TOHN = 99L,
      PARITY = "B",
      OFFSET = 0,
      geometry_wkt = "LINESTRING (-84.5 39.1, -84.49 39.11)",
      street_predirectional = "",
      street_premodifier = "",
      street_pretype = "",
      street_name = "MAIN",
      street_posttype = "ST",
      street_postdirectional = "",
      street_tag_parsed = FALSE,
      county_fips = "39061"
    ),
    path
  )
  taf_write_county_zip_manifest(
    tibble::tibble(
      county_fips = "39061",
      ZIP = "45220",
      zip3 = "452",
      zip2 = "20",
      n_ranges = 1L,
      installed_at = "2026-08-23 UTC"
    ),
    year = year,
    version = version
  )

  out <- taf(c("45220", "45220"), year = year, version = version)
  expect_equal(nrow(out), 1L)
  expect_identical(out$county_fips, "39061")
  expect_identical(format(out$addr_street), "MAIN ST")
  expect_s3_class(out$s2_geography, "s2_geography")
})

test_that("TAF public reader names are hard renamed", {
  exports <- getNamespaceExports("addr")
  expect_true(all(c("taf", "taf_dataset") %in% exports))
  expect_false("taf_zip" %in% exports)
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
  skip_live_tiger_downloads()
  skip_on_ci()
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})
