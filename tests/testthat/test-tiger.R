test_that("can download files from tiger", {
  ## # works
  ## curl -L "https://www2.census.gov/geo/tiger/TIGER2022/ADDRFEAT/tl_2022_39061_addrfeat.zip" \
  ##   -H "Host: www2.census.gov" \
  ##   -H "User-Agent: addr package for R (https://geomarker.io/addr)" \
  ##   -H "Sec-GPC: 1" \
  ##   -o tl_2022_39061_addrfeat.zip

  withr::local_envvar(list("R_USER_CACHE_DIR" = tempdir()))
  dl_file <- tiger_download("TIGER2022/ADDRFEAT/tl_2022_39061_addrfeat.zip")

  sf::read_sf(paste0("/vsizip/", dl_file)) |>
    ncol() |>
    expect_identical(26L)
})

test_that("tiger_street_ranges() works", {
  withr::local_envvar(list("R_USER_CACHE_DIR" = tempdir()))
  d <- tiger_street_ranges(county = "39061", year = "2022")
  expect_identical(length(d), 9267L)
  expect_identical(names(d[[1]]), c("TLID", "s2_geography", "from", "to"))
  d[[3]] |>
    expect_s3_class(c("tbl_df")) |>
    nrow() |>
    expect_identical(4L)
})
