test_that("map_state_to_abbrev handles names, abbreviations, and empty strings", {
  expect_equal(
    map_state_to_abbrev(c("Ohio", "oh", "District of Columbia", "PR", "foo", "", NA)),
    c("OH", "OH", "DC", "PR", NA, "", NA)
  )

  expect_equal(
    map_state_to_abbrev(c("  new york ", "U.S. Virgin Islands")),
    c("NY", "VI")
  )
})
