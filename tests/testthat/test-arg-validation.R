test_that("nad-family functions validate county inputs and path scalars", {
  expect_error(
    nad(NA_character_, "OH", refresh_binary = "no", refresh_source = "no"),
    "county must not be missing"
  )
  expect_error(
    nad("Hamilton", refresh_binary = "no", refresh_source = "no"),
    "state must be supplied when county is not a 5-digit FIPS identifier"
  )
  expect_error(
    nad(
      "Hamilton",
      c("OH", "KY"),
      refresh_binary = "no",
      refresh_source = "no"
    ),
    "state must be NULL or length one"
  )
  expect_error(
    nad_read(
      "Hamilton",
      NA_character_,
      version = 22L,
      refresh_source = "no"
    ),
    "state must be NULL or not missing"
  )
  expect_error(
    nad_download(version = "latest", refresh_source = "no"),
    "version must be an integer vector"
  )
  expect_error(
    nad_sd_path(1, "OH"),
    "county must be a character vector"
  )
})

test_that("tiger functions validate scalar inputs before doing work", {
  expect_error(
    tiger_download(NA_character_),
    "x must not be missing"
  )
  expect_error(
    tiger_addr_feat(39061, "2024"),
    "county must be a character vector"
  )
})

test_that("text parsing helpers validate control arguments", {
  expect_error(clean_address_text(1), "x must be a character vector")
  expect_error(phonetic_street_key(1), "x must be a character vector")
  expect_error(
    tag_usaddress("123 Main St Cincinnati OH 45220", clean = NA),
    "clean must be TRUE or FALSE"
  )
  expect_error(
    as_addr("123 Main St Cincinnati OH 45220", clean = NA),
    "clean must be TRUE or FALSE"
  )
})

test_that("matching helpers validate scalar tuning arguments", {
  x_street <- addr_street(name = "Main", posttype = "St")
  y_street <- addr_street(name = "Main", posttype = "St")
  expect_error(
    match_addr_street(x_street, y_street, name_fuzzy_dist = c(1L, 2L)),
    "name_fuzzy_dist must be length one"
  )

  x_number <- addr_number(digits = "10")
  y_number <- addr_number(digits = "10")
  expect_error(
    match_addr_number(x_number, y_number, number_fuzzy_dist = NA_integer_),
    "number_fuzzy_dist must not be missing"
  )

  expect_error(
    match_zipcodes(45220, "45220"),
    "x must be a character vector"
  )
  expect_error(
    match_zipcodes("45220", "45220", zip_variant = character()),
    "zip_variant must not be empty"
  )
  expect_error(
    match_zipcodes("45220", "45220", zip_variant = NA_character_),
    "zip_variant must not contain missing values"
  )

  expect_error(
    addr_match_prepare(
      as_addr("10 MAIN ST CINCINNATI OH 45220"),
      progress = NA
    ),
    "progress must be TRUE or FALSE"
  )
})

test_that("geocode validates progress argument", {
  x <- addr(
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )
  expect_error(
    geocode(x, progress = NA),
    "progress must be TRUE or FALSE"
  )
  expect_error(
    geocode(x, offset = -1),
    "offset must be non-negative"
  )
  expect_error(
    geocode(x, match_street_posttype = NA),
    "match_street_posttype must be TRUE or FALSE"
  )
  expect_error(
    geocode_zip(x, offset = NA_real_),
    "offset must not be missing"
  )
  expect_error(
    geocode_zip(x, name_phonetic_dist = NA_integer_),
    "name_phonetic_dist must not be missing"
  )
  expect_error(
    geocode_zip(x, zip_variants = NA),
    "zip_variants must be TRUE or FALSE"
  )
  expect_error(
    geocode(x, taf_install = NA),
    "taf_install must be TRUE or FALSE"
  )
  expect_error(
    geocode_zip(x, taf_redownload = NA),
    "taf_redownload must be TRUE or FALSE"
  )
  expect_error(
    geocode_zip(x, taf_check = NA),
    "taf_check must be TRUE or FALSE"
  )
  expect_error(
    geocode(x, zip_variant = character()),
    "zip_variant must not be empty"
  )
})

test_that("join helpers validate by, suffix, and progress arguments", {
  x <- tibble::tibble(addr = as_addr("10 MAIN ST CINCINNATI OH 45220"))
  y <- tibble::tibble(addr = as_addr("10 MAIN ST CINCINNATI OH 45220"))

  expect_error(
    addr_left_join(x, y, by = NA_character_, progress = FALSE),
    "by must not contain missing values"
  )
  expect_error(
    addr_fuzzy_left_join(
      x,
      y,
      suffix = c(".x", NA_character_),
      progress = FALSE
    ),
    "suffix must not contain missing values"
  )
  expect_error(
    addr_fuzzy_left_join(x, y, progress = NA),
    "progress must be TRUE or FALSE"
  )
})

test_that("constructors validate logical mapping flags", {
  expect_error(
    addr_street(name = "Main", map_posttype = NA),
    "map_posttype must be TRUE or FALSE"
  )
  expect_error(
    addr_place(
      name = "Cincinnati",
      state = "OH",
      zipcode = "45220",
      map_state = NA
    ),
    "map_state must be TRUE or FALSE"
  )
  expect_error(
    as_addr(
      data.frame(street_name = "Main", stringsAsFactors = FALSE),
      map_directional = NA
    ),
    "map_directional must be TRUE or FALSE"
  )
})
