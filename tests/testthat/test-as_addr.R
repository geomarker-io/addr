test_that("as_addr returns canonical addr unchanged", {
  x <- addr(
    addr_number(digits = "290"),
    addr_street(name = "Burnet", posttype = "Ave"),
    addr_place(name = "Cincinnati", state = "OH", zipcode = "45229")
  )

  expect_identical(as_addr(x), x)
})

test_that("as_addr upgrades legacy mixed-case addr objects", {
  x <- addr(
    addr_number(prefix = "N", digits = "290", suffix = "A"),
    addr_street(name = "BURNET", posttype = "AVE"),
    addr_place(name = "CINCINNATI", state = "OH", zipcode = "45229")
  )
  number <- x@number
  street <- x@street
  place <- x@place
  attr(number, "prefix") <- "n"
  attr(street, "name") <- "Burnet"
  attr(street, "posttype") <- "Ave"
  attr(place, "name") <- "Cincinnati"
  attr(x, "number") <- number
  attr(x, "street") <- street
  attr(x, "place") <- place

  out <- as_addr(x)

  expect_identical(format(out), "N290A BURNET AVE CINCINNATI OH 45229")
  expect_true(all(vapply(
    as.data.frame(out),
    function(value) identical(value, toupper(value)),
    logical(1)
  )))
})

test_that("as_addr handles character vectors", {
  x <- as_addr("200 W 14th Street Cincinnati OH 45222")
  expect_true(inherits(x, "addr"))
  expect_equal(
    as.data.frame(x),
    structure(
      list(
        number_prefix = "",
        number_digits = "200",
        number_suffix = "",
        street_predirectional = "W",
        street_premodifier = "",
        street_pretype = "",
        street_name = "14TH",
        street_posttype = "ST",
        street_postdirectional = "",
        place_name = "CINCINNATI",
        place_state = "OH",
        place_zipcode = "45222"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("as_addr maps abbreviations for data.frame inputs", {
  df <- data.frame(
    number_digits = "200",
    street_predirectional = "west",
    street_pretype = "US",
    street_name = "Main",
    street_posttype = "avenue",
    place_name = "Cincinnati",
    place_state = "ohio",
    place_zipcode = "45220",
    stringsAsFactors = FALSE
  )

  mapped <- as_addr(df)
  expect_equal(
    as.data.frame(mapped),
    structure(
      list(
        number_prefix = "",
        number_digits = "200",
        number_suffix = "",
        street_predirectional = "W",
        street_premodifier = "",
        street_pretype = "US HWY",
        street_name = "MAIN",
        street_posttype = "AVE",
        street_postdirectional = "",
        place_name = "CINCINNATI",
        place_state = "OH",
        place_zipcode = "45220"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )

  unmapped <- as_addr(
    df,
    map_state = FALSE,
    map_posttype = FALSE,
    map_directional = FALSE,
    map_pretype = FALSE
  )
  expect_equal(
    as.data.frame(unmapped),
    structure(
      list(
        number_prefix = "",
        number_digits = "200",
        number_suffix = "",
        street_predirectional = "WEST",
        street_premodifier = "",
        street_pretype = "US",
        street_name = "MAIN",
        street_posttype = "AVENUE",
        street_postdirectional = "",
        place_name = "CINCINNATI",
        place_state = "OHIO",
        place_zipcode = "45220"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("as_addr preserves unmapped street tags for data.frame inputs", {
  df <- data.frame(
    number_digits = "200",
    street_name = "Main",
    street_posttype = "Foofy",
    place_zipcode = "45220",
    stringsAsFactors = FALSE
  )

  expect_warning(
    mapped <- as_addr(df),
    "foofy"
  )
  expect_equal(mapped@street@posttype, "FOOFY")
})

test_that("as_addr handles NA and empty inputs", {
  na_addr <- as_addr(NA_character_)
  expect_equal(
    as.data.frame(na_addr),
    structure(
      list(
        number_prefix = NA_character_,
        number_digits = NA_character_,
        number_suffix = NA_character_,
        street_predirectional = NA_character_,
        street_premodifier = NA_character_,
        street_pretype = NA_character_,
        street_name = NA_character_,
        street_posttype = NA_character_,
        street_postdirectional = NA_character_,
        place_name = NA_character_,
        place_state = NA_character_,
        place_zipcode = NA_character_
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )

  empty_addr <- as_addr(character(0))
  expect_equal(nrow(as.data.frame(empty_addr)), 0)
})

test_that("as_addr deals with multiples of an address tag", {
  as_addr(
    c(
      "1234 Main St Cincinnati Cincinnati OH 45229",
      "1234 Main St Clifton Cincinnati OH 45229"
    )
  )@place@name |>
    expect_identical(c("CINCINNATI", "CLIFTON CINCINNATI"))
})

test_that("as_addr tries to fix zipcodes", {
  as_addr(c(
    "123 Main Street Anytown IL 34502-2230",
    "123 Main Street Anytown IL 34502",
    "67 Main Avenue Anytown PA 83127-0333"
  )) |>
    expect_warning(
      "Truncating 2 parsed ZIP codes to the first five characters."
    )
  as_addr(
    "1234 Main St Cincinnati OH 45229 Cincinnati OH 45230"
  ) |>
    expect_warning("Truncating 1 parsed ZIP codes to the first five characters")
})

test_that("as_addr makes malformed parsed zipcodes missing", {
  expect_warning(
    expect_warning(
      x <- as_addr(c(
        "123 Main Street Anytown IL 1234",
        "123 Main Street Anytown IL 00021",
        "123 Main Street Anytown IL 1234A",
        "123 Main Street Anytown IL 00021-1234",
        "123 Main Street Anytown IL 45220"
      )),
      "Setting 4 malformed parsed ZIP codes to missing"
    ),
    "Truncating 1 parsed ZIP codes to the first five characters"
  )

  expect_equal(
    x@place@zipcode,
    c(NA_character_, NA_character_, NA_character_, NA_character_, "45220")
  )
})

test_that("as_addr malformed zipcode warning identifies affected inputs", {
  expect_warning(
    as_addr(c(
      "123 Main Street Anytown IL 00021",
      "123 Main Street Anytown IL 45220"
    )),
    "Affected address examples: 1:"
  )
})

test_that("as_addr deals with some seriously messy addresses", {
  as_addr(c(
    "1234Main St Cincinnati OH 45229",
    "12D Main St Cincinnati OH 45229",
    "222E CENTRAL PARKWAY CINCINNATI Ohio 45202"
  )) |>
    expect_warning(
      "Removing non-numeric characters from parsed address number digits in 2 addresses"
    )
})

test_that("as_addr truncates parsed address numbers above the NAD maximum", {
  expect_warning(
    x <- as_addr(c(
      "1000000 Main St Cincinnati OH 45220",
      "999999 Main St Cincinnati OH 45220",
      "1234567 Main St Cincinnati OH 45220"
    )),
    "Truncating 2 parsed address number digits greater than 999999 to the first six digits"
  )

  expect_equal(x@number@digits, c("100000", "999999", "123456"))
})
