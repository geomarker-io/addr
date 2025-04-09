test_that("addr_match_tiger_street_ranges() works", {
  withr::local_envvar(list("R_USER_CACHE_DIR" = tempdir()))
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
