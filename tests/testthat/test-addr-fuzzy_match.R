test_that("fuzzy_match works", {
  my_names <- c(
    "Pinye",
    "Pine",
    "Oalck",
    "Sunset",
    "Walington",
    "riverbend",
    "Chersire",
    "Greenfild"
  )
  the_names <- c(
    "Piney",
    "Pine",
    "Oak",
    "Wallington",
    "Cheshire",
    "Greenfield",
    "Maple",
    "Elm",
    "Willow",
    "Cedar",
    "Birch",
    "Main",
    "Lakeview",
    "Highland",
    "Sunrise",
    "Sunset",
    "Riverbend",
    "Meadow",
    "Forest",
    "Orchard",
    "Bridgewater"
  )

  expect_identical(
    the_names[vapply(fuzzy_match(my_names, the_names), \(.) .[1], integer(1))],
    c(
      "Piney",
      "Pine",
      NA,
      "Sunset",
      "Wallington",
      "Riverbend",
      NA,
      "Greenfield"
    )
  )

  expect_identical(
    the_names[vapply(
      fuzzy_match(my_names, the_names, 0),
      FUN = \(.) .[1],
      integer(1)
    )],
    c(NA, "Pine", NA, "Sunset", NA, "Riverbend", NA, NA)
  )

  fuzzy_match(my_names, the_names, 1) |>
    expect_identical(list(1:2, 2L, integer(0), 16L, 4L, 17L, integer(0), 6L))

  fuzzy_match(my_names, the_names, 2) |>
    lapply(\(.x) the_names[.x]) |>
    expect_identical(list(
      c("Piney", "Pine"),
      "Pine",
      "Oak",
      "Sunset",
      "Wallington",
      "Riverbend",
      "Cheshire",
      "Greenfield"
    ))

  # y cannot contain NA
  expect_error(
    fuzzy_match(
      c("woolper", "burnet", "burnet", "walnut", "weknut"),
      c(NA, "adams", "woolper")
    ),
    "y must not contain NA"
  )

  fuzzy_match(
    c(NA, "adams", "woolper"),
    c("woolper", "burnet", "burnet", "walnut", "weknut")
  ) |>
    expect_identical(list(NA, integer(0), 1L))
})

test_that("fuzzy_match_addr_field works", {
  fuzzy_match_addr_field(
    as_addr(c(
      "222 E Central Parkway Foofyville SJ 00000",
      "222 East Central Parkway",
      "221 E Central Parkway Somewhere OS 00000",
      "222 East Central Cincinnati"
    )),
    as_addr(c(
      "222 E CENTRAL PKWY",
      "221 E CENTRAL PKWY",
      "222 CENTRAL PKWY",
      "222 E CENTRAL PKWY"
    )),
    addr_field = "street_predirectional"
  ) |>
    expect_identical(list(
      c(1L, 2L, 4L),
      c(1L, 2L, 4L),
      c(1L, 2L, 4L),
      c(1L, 2L, 4L)
    ))

  fuzzy_match_addr_field(
    as_addr(c(
      "222 E Central Parkway Foofyville SJ 00000",
      "222 East Central Parkway",
      "221 E Central Parkway Somewhere OS 00000",
      "222 East Central Cincinnati"
    )),
    as_addr(c(
      "222 E CENTRAL PKWY",
      "221 E CENTRAL PKWY",
      "222 CENTRAL PKWY",
      "222 E CENTRAL PKWY"
    )),
    addr_field = "number_digits"
  ) |>
    expect_identical(list(c(1L, 3L, 4L), c(1L, 3L, 4L), c(2L), c(1L, 3L, 4L)))

  fuzzy_match_addr_field(
    as_addr(c(
      "222 E Central Parkway Foofyville SJ 00000",
      "222 East Central Parkway",
      "221 E Central Parkway Somewhere OS 00000",
      "222 East Central Cincinnati"
    )),
    as_addr(c(
      "222 E CENTRAL PKWY",
      "221 E CENTRAL PKWY",
      "222 CENTRAL PKWY",
      "222 E CENTRAL PKWY"
    )),
    addr_field = "street_posttype"
  ) |>
    expect_identical(list(1:4, 1:4, 1:4, integer(0)))
})

test_that("addr_fuzzy_match applies defaults and supports partial overrides", {
  x <- as_addr(c(
    "123 Main St Cincinnati OH 45202",
    "456 Oak Ave Dayton OH 45402"
  ))
  y <- as_addr(c(
    "123 Mani St Cincinnati OH 45202",
    "456 Oak Avenue Dayton OH 45402"
  ))

  addr_fuzzy_match(x, y) |>
    expect_identical(list(1L, 2L))

  addr_fuzzy_match(x, y, addr_fields = c("street_name" = 0)) |>
    expect_identical(list(integer(0), 2L))
})

test_that("addr_fuzzy_match validates addr_fields", {
  expect_error(
    addr_fuzzy_match_resolve_max_dist(c(foo = 1)),
    "Allowed fields are"
  )
  expect_error(
    addr_fuzzy_match_resolve_max_dist(c(1, 2)),
    "Allowed fields are"
  )
})

test_that("addr_fuzzy_match returns matches with multiple candidates", {
  x <- as_addr(c(
    "10 River Rd Columbus OH 43085",
    "20 Lakeview Dr Columbus OH 43085",
    "3333 Burnet Ave Cincinnati OH 45219",
    NA
  ))
  y <- as_addr(c(
    "10 River Road Columbus OH 43085",
    "10 River Rd Columbus OH 43085",
    "20 Lake View Drive Columbus OH 43085"
  ))
  addr_fuzzy_match(x, y) |>
    expect_identical(list(c(1L, 2L), 3L, integer(0), NA))
})
