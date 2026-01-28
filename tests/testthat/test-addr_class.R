test_that("addr() combines components and formats", {
  number <- addr_number(prefix = "N", digits = "10", suffix = "A")
  street <- addr_street(
    predirectional = "N",
    premodifier = NA_character_,
    pretype = NA_character_,
    name = "Main",
    posttype = "St",
    postdirectional = "W",
    map_directional = FALSE,
    map_posttype = FALSE,
    map_pretype = FALSE
  )
  place <- addr_place(
    name = "Cincinnati",
    state = "OH",
    zipcode = "45220"
  )

  out <- addr(number, street, place)

  expect_true(inherits(out, "addr"))
  expect_equal(
    as.data.frame(out),
    structure(
      list(
        number_prefix = "N",
        number_digits = "10",
        number_suffix = "A",
        street_predirectional = "N",
        street_premodifier = NA_character_,
        street_pretype = NA_character_,
        street_name = "Main",
        street_posttype = "St",
        street_postdirectional = "W",
        place_name = "Cincinnati",
        place_state = "OH",
        place_zipcode = "45220"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
  expect_equal(format(out), "N10A N Main St W Cincinnati OH 45220")
})

test_that("addr() preserves empty strings", {
  number <- addr_number(prefix = "", digits = "", suffix = "")
  street <- addr_street(
    predirectional = "",
    premodifier = "",
    pretype = "",
    name = "",
    posttype = "",
    postdirectional = "",
    map_directional = FALSE,
    map_posttype = FALSE,
    map_pretype = FALSE
  )
  place <- addr_place(name = "", state = "", zipcode = "")

  out <- addr(number, street, place)

  expect_equal(
    as.data.frame(out),
    structure(
      list(
        number_prefix = "",
        number_digits = "",
        number_suffix = "",
        street_predirectional = "",
        street_premodifier = "",
        street_pretype = "",
        street_name = "",
        street_posttype = "",
        street_postdirectional = "",
        place_name = "",
        place_state = "",
        place_zipcode = ""
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
  expect_equal(format(out), "")
})

test_that("addr() rejects mismatched component lengths", {
  number <- addr_number(
    prefix = NA_character_,
    digits = c("10", "20"),
    suffix = NA_character_
  )
  street <- addr_street(
    predirectional = NA_character_,
    premodifier = NA_character_,
    pretype = NA_character_,
    name = c("Main", "Elm", "Pine"),
    posttype = NA_character_,
    postdirectional = NA_character_
  )
  place <- addr_place(
    name = "Cincinnati",
    state = "OH",
    zipcode = "45220"
  )

  expect_error(
    addr(number, street, place),
    "addr components must have length 1 or 3 for recycling"
  )
})
