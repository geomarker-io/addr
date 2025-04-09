test_that("fuzzy_match works", {
  my_names <- c("Pinye", "Pine", "Oalck", "Sunset", "Walington", "riverbend", "Chersire", "Greenfild")
  the_names <- c(
    "Piney", "Pine", "Oak", "Wallington", "Cheshire", "Greenfield", "Maple", "Elm", "Willow", "Cedar", "Birch",
    "Main", "Lakeview", "Highland", "Sunrise", "Sunset", "Riverbend", "Meadow", "Forest", "Orchard", "Bridgewater"
  )

  expect_identical(
    the_names[fuzzy_match(my_names, the_names)],
    c("Piney", "Pine", NA, "Sunset", "Wallington", "Riverbend", NA, "Greenfield")
  )

  expect_identical(
    the_names[fuzzy_match(my_names, the_names, 0)],
    c(NA, "Pine", NA, "Sunset", NA, "Riverbend", NA, NA)
  )

  fuzzy_match(my_names, the_names, 1, ties = "random") |>
    expect_length(length(my_names))

  fuzzy_match(my_names, the_names, 1, ties = "all") |>
    expect_identical(list(1:2, 2L, NA, 16L, 4L, 17L, NA, 6L))

  fuzzy_match(my_names, the_names, 2, ties = "all") |>
    lapply(\(.x) the_names[.x]) |>
    expect_identical(list(c("Piney", "Pine"), "Pine", "Oak", "Sunset", "Wallington", "Riverbend", "Cheshire", "Greenfield"))

  # deals with NA
  fuzzy_match(
    c("woolper", "burnet", "burnet", "walnut", "weknut"),
    c(NA, "adams", "woolper")
  ) |>
    expect_identical(c(3L, NA, NA, NA, NA))

  fuzzy_match(
    c(NA, "adams", "woolper"),
    c("woolper", "burnet", "burnet", "walnut", "weknut")
  ) |>
    expect_identical(c(NA, NA, 1L))

})

test_that("fuzzy_match_addr_field works", {

  fuzzy_match_addr_field(
    as_addr(c(
      "222 E Central Parkway Foofyville SJ 00000",
      "222 East Central Parkway",
      "221 E Central Parkway Somewhere OS 00000",
      "222 East Central Cincinnati"
    )),
    as_addr(c("222 E CENTRAL PKWY", "221 E CENTRAL PKWY", "222 CENTRAL PKWY", "222 E CENTRAL PKWY")),
    addr_field = "street_name"
  ) |>
    expect_identical(list(c(1L, 2L, 4L), c(1L, 2L, 4L), c(1L, 2L, 4L), NA))

  fuzzy_match_addr_field(
    as_addr(c(
      "222 E Central Parkway Foofyville SJ 00000",
      "222 East Central Parkway",
      "221 E Central Parkway Somewhere OS 00000",
      "222 East Central Cincinnati"
    )),
    as_addr(c("222 E CENTRAL PKWY", "221 E CENTRAL PKWY", "222 CENTRAL PKWY", "222 E CENTRAL PKWY")),
    addr_field = "street_number"
  ) |>
    expect_identical(list(c(1L, 3L, 4L), c(1L, 3L, 4L), c(2L), c(1L, 3L, 4L)))

  fuzzy_match_addr_field(
    as_addr(c(
      "222 E Central Parkway Foofyville SJ 00000",
      "222 East Central Parkway",
      "221 E Central Parkway Somewhere OS 00000",
      "222 East Central Cincinnati"
    )),
    as_addr(c("222 E CENTRAL PKWY", "221 E CENTRAL PKWY", "222 CENTRAL PKWY", "222 E CENTRAL PKWY")),
    addr_field = "street_type",
    ties = "first"
  ) |>
    expect_identical(c(1L, 1L, 1L, NA))

})
