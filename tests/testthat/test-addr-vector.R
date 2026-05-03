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

test_that("[<- works for addr_* vectors", {
  num <- addr_number(digits = c("1", "2", "3"))
  num[2] <- addr_number(digits = "9")
  expect_equal(format(num), c("1", "9", "3"))

  street <- addr_street(
    name = c("Main", "Elm", "Pine"),
    posttype = "St",
    map_posttype = FALSE,
    map_pretype = FALSE,
    map_directional = FALSE
  )
  street[c(1, 3)] <- addr_street(
    name = c("Oak", "Beech"),
    posttype = "Ave",
    map_posttype = FALSE,
    map_pretype = FALSE,
    map_directional = FALSE
  )
  expect_equal(format(street), c("Oak Ave", "Elm St", "Beech Ave"))

  place <- addr_place(
    name = c("Cincinnati", "Columbus"),
    state = "OH",
    zipcode = c("45220", "43215")
  )
  place[] <- addr_place(name = "Dayton", state = "OH", zipcode = "45402")
  expect_equal(
    format(place),
    c("Dayton OH 45402", "Dayton OH 45402")
  )
})

test_that("[<- works for addr vectors", {
  x <- addr(
    addr_number(digits = c("1", "2")),
    addr_street(
      name = c("Main", "Elm"),
      posttype = "St",
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(
      name = "Cincinnati",
      state = "OH",
      zipcode = "45220",
      map_state = FALSE
    )
  )

  x[1] <- addr(
    addr_number(digits = "9"),
    addr_street(
      name = "Oak",
      posttype = "Ave",
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(
      name = "Dayton",
      state = "OH",
      zipcode = "45402",
      map_state = FALSE
    )
  )

  expect_equal(
    format(x),
    c("9 Oak Ave Dayton OH 45402", "2 Elm St Cincinnati OH 45220")
  )
})

test_that("addr works as a tibble and data.frame column", {
  x <- addr(
    addr_number(digits = c("1", "2")),
    addr_street(
      name = c("Main", "Elm"),
      posttype = "St",
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(
      name = "Cincinnati",
      state = "OH",
      zipcode = "45220",
      map_state = FALSE
    )
  )

  tbl <- tibble::tibble(address = x)
  expect_equal(ncol(tbl), 1L)
  expect_true(inherits(tbl$address, "addr"))
  expect_equal(format(tbl$address), format(x))

  df <- data.frame(address = I(x))
  expect_equal(ncol(df), 1L)
  expect_true(inherits(df$address, "AsIs"))
  expect_true(inherits(df$address, "addr"))
  expect_equal(length(df$address), length(x))
})
