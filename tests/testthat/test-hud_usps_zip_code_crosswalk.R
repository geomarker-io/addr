test_that("zip_fips_lookup translates ZIP codes to matched county FIPS", {
  expect_equal(
    addr:::zip_fips_lookup("45249"),
    c("39061", "39165")
  )
})

test_that("zip_fips_lookup translates ZIP and FIPS to preferred place metadata", {
  place <- addr:::zip_fips_lookup("45249", "39061")

  expect_true(inherits(place, "addr_place"))
  expect_equal(
    as.data.frame(place),
    structure(
      list(
        place_name = "CINCINNATI",
        place_state = "OH",
        place_zipcode = "45249"
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
})

test_that("zip_fips_lookup validates ZIP inputs", {
  expect_error(
    addr:::zip_fips_lookup(45249),
    "zip must be a character vector"
  )
  expect_error(
    addr:::zip_fips_lookup(c("45249", "45220")),
    "zip must be length one"
  )
  expect_error(
    addr:::zip_fips_lookup(NA_character_),
    "zip must not be missing"
  )
  expect_error(
    addr:::zip_fips_lookup(""),
    "zipcode `` was not found"
  )
  expect_error(
    addr:::zip_fips_lookup("123"),
    "@zipcode must be exactly five numeric digits"
  )
})

test_that("zip_fips_lookup validates FIPS inputs", {
  expect_error(
    addr:::zip_fips_lookup("45249", 39061),
    "fips must be a character vector"
  )
  expect_error(
    addr:::zip_fips_lookup("45249", c("39061", "39165")),
    "fips must be length one"
  )
  expect_error(
    addr:::zip_fips_lookup("45249", NA_character_),
    "fips must not be missing"
  )
  expect_error(
    addr:::zip_fips_lookup("45249", ""),
    "fips must be 5 digits"
  )
})
