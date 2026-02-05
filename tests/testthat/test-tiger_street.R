test_that("map_street_name_post_type maps variants and preserves blanks", {
  expect_equal(
    map_street_name_post_type(c("Avenue", "Avnue", "Blvrd", "", NA, "Woop")),
    c("Ave", "Ave", "Blvd", "", NA, NA)
  )
})

test_that("map_street_name_post_type handles NULL and trims", {
  expect_equal(map_street_name_post_type(NULL), NA_character_)
  expect_equal(
    map_street_name_post_type(c(" Ave ", "Rd", "Riv ")),
    c("Ave", "Rd", "Riv")
  )
})

test_that("map_street_name_pre_type maps variants and preserves blanks", {
  expect_equal(
    map_street_name_pre_type(c("US", "U.S.", "I-", "", NA, "Nope")),
    c("US Hwy", "US Hwy", "I-", "", NA, NA)
  )
})

test_that("map_street_name_pre_type handles NULL and trims", {
  expect_equal(map_street_name_pre_type(NULL), NA_character_)
  expect_equal(
    map_street_name_pre_type(c(" U.S. Hwy ", "Co Rd", "Rte ")),
    c("US Hwy", "Co Rd", "Rte")
  )
})

test_that("map_direction maps variants and preserves blanks", {
  expect_equal(
    map_direction(c("North", "N.E.", "south west", "", NA, "Nope")),
    c("N", "NE", "SW", "", NA, NA)
  )
})

test_that("map_direction handles NULL and trims", {
  expect_equal(map_direction(NULL), NA_character_)
  expect_equal(
    map_direction(c(" N ", "S.", "North-East ")),
    c("N", "S", "NE")
  )
})
