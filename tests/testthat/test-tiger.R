test_that("can download files from tiger", {
  skip_if_offline()
  skip_on_cran()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  dl_file <- tiger_download(
    "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  )
  expect_true(file.exists(dl_file))
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

test_that("taf_catalog reads installed ZIP county catalog", {
  skip_if_not_installed("arrow")
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
  taf_write_catalog(manifest, year = "2025", version = "v1", root = catalog_root)

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
  skip_if_not_installed("arrow")
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
  skip_if_not_installed("arrow")
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
