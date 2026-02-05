test_that("can download files from tiger", {
  skip_if_offline()
  skip_on_cran()
  ## # works
  ## curl -L "https://www2.census.gov/geo/tiger/TIGER2022/ADDRFEAT/tl_2022_39061_addrfeat.zip" \
  ##   -H "Host: www2.census.gov" \
  ##   -H "User-Agent: addr package for R (https://geomarker.io/addr)" \
  ##   -H "Sec-GPC: 1" \
  ##   -o tl_2022_39061_addrfeat.zip

  withr::local_envvar(list("R_USER_DATA_DIR" = tempdir()))
  dl_file <- tiger_download(
    "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  )

  sf::read_sf(paste0("/vsizip/", dl_file)) |>
    ncol() |>
    expect_identical(4L)
})

test_that("tiger_street_ranges() works", {
  withr::local_envvar(list("R_USER_DATA_DIR" = tempdir()))
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$geometry, "sfc")
})
