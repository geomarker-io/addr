test_that("map_street_name_post_type maps variants and preserves blanks", {
  expect_equal(
    map_street_name_post_type(c("Avenue", "Avnue", "Blvrd", "", NA, "Woop")),
    c("Ave", "Ave", "Blvd", "", NA, NA)
  )
})

test_that("map_street_name_pre_type maps variants and preserves blanks", {
  expect_equal(
    map_street_name_pre_type(c("US", "U.S.", "I-", "", NA, "Nope")),
    c("US Hwy", "US Hwy", "I-", "", NA, NA)
  )
})

test_that("map_direction maps variants and preserves blanks", {
  expect_equal(
    map_direction(c("North", "N.E.", "south west", "", NA, "Nope")),
    c("N", "NE", "SW", "", NA, NA)
  )
})
