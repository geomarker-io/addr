test_that("addr_* lengths behave like vectors", {
  num <- addr_number(digits = c("1", "2"))
  street <- addr_street(
    name = c("Main", "Elm"),
    posttype = c("St", "Ave"),
    map_posttype = FALSE,
    map_pretype = FALSE,
    map_directional = FALSE
  )
  place <- addr_place(
    name = c("Cincinnati", "Cincinnati"),
    state = c("OH", "OH"),
    zipcode = c("45220", "45229"),
    map_state = FALSE
  )
  addr_vec <- addr(num, street, place)
  expect_equal(length(num), 2L)
  expect_equal(length(street), 2L)
  expect_equal(length(place), 2L)
  expect_equal(length(addr_vec), 2L)
})

test_that("unique, [, and length works for addr and addr_* vectors", {
  num <- addr_number(digits = c("1", "1", "2"))
  expect_equal(length(unique(num)), 2L)
  expect_equal(format(unique(num)), c("1", "2"))

  street <- addr_street(
    name = c("Main", "Main", "Elm"),
    posttype = c("St", "St", "St"),
    map_posttype = FALSE,
    map_pretype = FALSE,
    map_directional = FALSE
  )
  place <- addr_place(
    name = "Cincinnati",
    state = "OH",
    zipcode = "45220",
    map_state = FALSE
  )
  addr_vec <- addr(num, street, place)

  expect_length(addr_vec[2], 1)
  expect_equal(addr_vec[3]@street@name, "Elm")

  addr_unique <- unique(addr_vec)

  expect_equal(length(addr_unique), 2L)
  expect_equal(
    format(addr_unique),
    c("1 Main St Cincinnati OH 45220", "2 Elm St Cincinnati OH 45220")
  )
})
