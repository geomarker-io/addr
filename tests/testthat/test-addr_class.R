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
        street_name = "MAIN",
        street_posttype = "ST",
        street_postdirectional = "W",
        place_name = "CINCINNATI",
        place_state = "OH",
        place_zipcode = "45220"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
  expect_equal(format(out), "N10A N MAIN ST W CINCINNATI OH 45220")
})

test_that("addr() canonicalizes every stored field to uppercase", {
  out <- addr(
    addr_number(prefix = "n", digits = "10", suffix = "a"),
    addr_street(
      predirectional = "north",
      premodifier = "Old",
      pretype = "US",
      name = "Main",
      posttype = "Avenue",
      postdirectional = "west",
      map_directional = FALSE,
      map_pretype = FALSE,
      map_posttype = FALSE,
      map_ordinal = FALSE
    ),
    addr_place(
      name = "Cincinnati",
      state = "ohio",
      zipcode = "45220",
      map_state = FALSE
    )
  )

  fields <- as.data.frame(out)
  expect_true(all(vapply(
    fields,
    function(x) identical(x, toupper(x)),
    logical(1)
  )))
  expect_identical(
    format(out),
    "N10A NORTH OLD US MAIN AVENUE WEST CINCINNATI OHIO 45220"
  )
})

test_that("addr() canonicalizes legacy mixed-case component objects", {
  number <- addr_number(prefix = "N", digits = "10", suffix = "A")
  street <- addr_street(
    name = "MAIN",
    posttype = "ST",
    map_posttype = FALSE
  )
  place <- addr_place(name = "CINCINNATI", state = "OH", zipcode = "45220")
  attr(number, "prefix") <- "n"
  attr(street, "name") <- "Main"
  attr(street, "posttype") <- "St"
  attr(place, "name") <- "Cincinnati"

  out <- addr(number, street, place)

  expect_identical(format(out), "N10A MAIN ST CINCINNATI OH 45220")
})

test_that("addr() formatting works well with vector-like addr", {
  out <- addr(
    addr_number(digits = as.character(101:109)),
    addr_street(name = "Main", posttype = "Ave"),
    addr_place("Cincinnati", "OH", "45219")
  )
  format(out) |>
    expect_identical(
      c(
        "101 MAIN AVE CINCINNATI OH 45219",
        "102 MAIN AVE CINCINNATI OH 45219",
        "103 MAIN AVE CINCINNATI OH 45219",
        "104 MAIN AVE CINCINNATI OH 45219",
        "105 MAIN AVE CINCINNATI OH 45219",
        "106 MAIN AVE CINCINNATI OH 45219",
        "107 MAIN AVE CINCINNATI OH 45219",
        "108 MAIN AVE CINCINNATI OH 45219",
        "109 MAIN AVE CINCINNATI OH 45219"
      )
    )
})

test_that("addr() preserves empty strings", {
  out <- as_addr(c("123 Cincinnati OH 45219", ""))

  expect_equal(
    as.data.frame(out),
    structure(
      list(
        number_prefix = c("", ""),
        number_digits = c("123", ""),
        number_suffix = c("", ""),
        street_predirectional = c("", ""),
        street_premodifier = c("", ""),
        street_pretype = c("", ""),
        street_name = c("", ""),
        street_posttype = c("", ""),
        street_postdirectional = c("", ""),
        place_name = c("CINCINNATI", ""),
        place_state = c("OH", ""),
        place_zipcode = c("45219", "")
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
    )
  )
  expect_equal(format(out), c("123 CINCINNATI OH 45219", ""))
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
