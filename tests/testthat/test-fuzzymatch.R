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

})
