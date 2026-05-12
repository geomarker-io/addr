test_that("addr_street() maps components and formats", {
  street <- addr_street(
    predirectional = c("North", "S"),
    premodifier = "Old",
    pretype = c("US", "I-"),
    name = c("Main", "75"),
    posttype = c("Avnue", "Blvrd"),
    postdirectional = c("West", "E")
  )

  expect_true(inherits(street, "addr_street"))
  expect_equal(
    as.data.frame(street),
    structure(
      list(
        street_predirectional = c("N", "S"),
        street_premodifier = c("Old", "Old"),
        street_pretype = c("US Hwy", "I-"),
        street_name = c("Main", "75"),
        street_posttype = c("Ave", "Blvd"),
        street_postdirectional = c("W", "E")
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
    )
  )
  expect_equal(
    format(street),
    c("N Old US Hwy Main Ave W", "S Old I- 75 Blvd E")
  )
})

test_that("addr_street() can skip mapping", {
  street <- addr_street(
    predirectional = "north",
    premodifier = NA_character_,
    pretype = "US",
    name = "Main",
    posttype = "Avenue",
    postdirectional = "west",
    map_directional = FALSE,
    map_pretype = FALSE,
    map_posttype = FALSE
  )

  expect_equal(
    format(street),
    "north US Main Avenue west"
  )
})

test_that("addr_street() preserves unmapped street tags", {
  expect_warning(
    street <- addr_street(name = "Main", posttype = "Foofy"),
    "foofy"
  )
  expect_equal(street@posttype, "Foofy")

  expect_warning(
    street <- addr_street(predirectional = "Uphill", name = "Main"),
    "uphill"
  )
  expect_equal(street@predirectional, "Uphill")
})

test_that("addr_street() rejects mismatched lengths", {
  expect_error(
    addr_street(
      predirectional = c("N", "S"),
      premodifier = NA_character_,
      pretype = NA_character_,
      name = c("Main", "Elm", "Pine"),
      posttype = NA_character_,
      postdirectional = NA_character_
    ),
    "addr_street fields must have length 1 or 3 for recycling"
  )
})
