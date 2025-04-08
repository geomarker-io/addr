## # works
## curl -L "https://www2.census.gov/geo/tiger/TIGER2022/ADDRFEAT/tl_2022_39061_addrfeat.zip" \
##   -H "Host: www2.census.gov" \
##   -H "User-Agent: addr package for R (https://geomarker.io/addr)" \
##   -H "Sec-GPC: 1" \
##   -o tl_2022_39061_addrfeat.zip


test_that("can download files from tiger", {

  withr::local_envvar(list("R_USER_CACHE_DIR" = tempdir()))
  dl_file <- tiger_download("TIGER2022/ADDRFEAT/tl_2022_39061_addrfeat.zip")

  sf::read_sf(paste0("/vsizip/", dl_file)) |>
    ncol() |>
    expect_identical(26L)


})

test_that("get_tiger_street_ranges() works", {
  d <- get_tiger_street_ranges(county = "39061", year = "2022")
  expect_identical(length(d), 9267L)
  expect_identical(names(d[[1]]), c("TLID", "s2_geography", "from", "to"))
  d[[3]] |>
    expect_s3_class(c("tbl_df")) |>
    nrow() |>
    expect_identical(4L)
})

test_that("addr_match_tiger_street_ranges() works", {
  addr_match_tiger_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave", "33333 Burnet Ave", "609 Walnut St", "609 Weknut Street")),
    street_only_match = "none"
  ) |>
    purrr::map(nrow) |>
    expect_identical(list(
      `224 Woolper Avenue` = 1L,
      `3333 Burnet Avenue` = 2L,
      `33333 Burnet Avenue` = 0L,
      `609 Walnut Street` = 2L,
      `609 Weknut Street` = NULL
    ))

  addr_match_tiger_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave", "33333 Burnet Ave", "609 Walnut St")),
    street_only_match = "all"
  ) |>
    purrr::map(nrow) |>
    expect_identical(list(
      `224 Woolper Avenue` = 1L,
      `3333 Burnet Avenue` = 2L,
      `33333 Burnet Avenue` = 20L,
      `609 Walnut Street` = 2L
    ))

  addr_match_tiger_street_ranges(as_addr(c("224 Woolper Ave", "3333 Burnet Ave", "33333 Burnet Ave", "609 Walnut St")),
    street_only_match = "closest"
  ) |>
    purrr::map(nrow) |>
    expect_identical(list(
      `224 Woolper Avenue` = 1L,
      `3333 Burnet Avenue` = 2L,
      `33333 Burnet Avenue` = 1L,
      `609 Walnut Street` = 2L
    ))
})
