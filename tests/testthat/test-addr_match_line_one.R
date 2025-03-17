test_that("addr_match_line_one works", {
  addr_match_line_one(
    as_addr(c("3333 Burnet Ave Cincinnati OH 45219", "123 Foofy St Cincinnati OH 45219")),
    as_addr("3333 Burnet Avenue Cinti Ohio 45219")
  ) |>
    expect_identical(list(`3333 Burnet Avenue Cincinnati OH 45219` = structure(list(
      street_number = 3333, street_name = "burnet", street_type = "avenue",
      city = "cinti", state = "ohio", zip_code = "45219"
    ), class = c(
      "addr",
      "vctrs_rcrd", "vctrs_vctr"
    )), `123 Foofy Street Cincinnati OH 45219` = structure(list(
      street_number = numeric(0), street_name = character(0), street_type = character(0),
      city = character(0), state = character(0), zip_code = character(0)
    ), class = c(
      "addr",
      "vctrs_rcrd", "vctrs_vctr"
    ))))

  addr_match_line_one(
    as_addr(c("3333 Burnet Ave Cincinnati OH 45219", "123 Foofy St Cincinnati OH 45219")),
    as_addr("Burnet Avenue Cinti Ohio 45219"),
    max_dist_street_number = NULL
  ) |>
    expect_identical(list(`3333 Burnet Avenue Cincinnati OH 45219` = structure(list(
      street_number = NA_real_, street_name = "burnet", street_type = "avenue",
      city = "cinti", state = "ohio", zip_code = "45219"
    ), class = c(
      "addr",
      "vctrs_rcrd", "vctrs_vctr"
    )), `123 Foofy Street Cincinnati OH 45219` = structure(list(
      street_number = numeric(0), street_name = character(0), street_type = character(0),
      city = character(0), state = character(0), zip_code = character(0)
    ), class = c(
      "addr",
      "vctrs_rcrd", "vctrs_vctr"
    ))))


  as_addr(c(
    "222 E Central Parkway Foofyville SJ 00000",
    "222 East Central Parkway",
    "221 E Central Parkway Somewhere OS 00000",
    "222 East Central Cincinnati"
  )) |>
    addr_match_line_one(as_addr(c("222 E CENTRAL PKWY", "221 E CENTRAL PKWY", "222 CENTRAL PKWY", "222 E CENTRAL PKWY"))) |>
    sapply(length) |>
    expect_identical(c(
      `222 E Central Parkway Foofyville SJ 00000` = 2L, `222 E Central Parkway` = 2L,
      `221 E Central Parkway Somewhere OS 00000` = 1L, `222 E Central Cincinnati` = 0L
    ))


  
})
