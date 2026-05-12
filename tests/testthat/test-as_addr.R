test_that("as_addr returns addr unchanged", {
  x <- addr(
    addr_number(digits = "290"),
    addr_street(name = "Burnet", posttype = "Ave"),
    addr_place(name = "Cincinnati", state = "OH", zipcode = "45229")
  )

  expect_identical(as_addr(x), x)
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
        street_posttype = "St",
        street_postdirectional = "",
        place_name = "Cincinnati",
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
        street_pretype = "US Hwy",
        street_name = "Main",
        street_posttype = "Ave",
        street_postdirectional = "",
        place_name = "Cincinnati",
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
        street_predirectional = "west",
        street_premodifier = "",
        street_pretype = "US",
        street_name = "Main",
        street_posttype = "avenue",
        street_postdirectional = "",
        place_name = "Cincinnati",
        place_state = "ohio",
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
  expect_equal(mapped@street@posttype, "Foofy")
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
    expect_identical(c("Cincinnati", "Clifton Cincinnati"))
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
