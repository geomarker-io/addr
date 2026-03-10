test_that("zipcode_variant returns known variants", {
  expect_equal(
    zipcode_variant("45220"),
    c(
      "42520",
      "45200",
      "45210",
      "45219",
      "45221",
      "45222",
      "45223",
      "45224",
      "45225",
      "45226",
      "45227",
      "45228",
      "45229",
      "45230",
      "45240",
      "45250",
      "45260",
      "45270",
      "45280",
      "45290"
    )
  )

  expect_equal(zipcode_variant(""), "")
  expect_equal(zipcode_variant(NA_character_), NA_character_)
})

test_that("match_zipcodes uses known zipcode examples", {
  expect_equal(
    match_zipcodes(
      c("45222", "45219", "45219", "45220", "45220", "", NA_character_),
      c("42522", "45200", "45219", "45221", "45223", "45321", "")
    ),
    c("45221", "45219", "45219", "45221", "45221", NA_character_, NA_character_)
  )
})

test_that("match_zipcodes rejects invalid zipcodes", {
  expect_error(
    match_zipcodes("00000", "45220"),
    "@zipcode must be exactly five numeric digits"
  )
  expect_error(
    match_zipcodes("00021", "45220"),
    "@zipcode must be exactly five numeric digits"
  )
  expect_error(
    match_zipcodes("1234", "45220"),
    "@zipcode must be exactly five numeric digits"
  )
  expect_error(
    match_zipcodes("45220", "00000"),
    "@zipcode must be exactly five numeric digits"
  )
  expect_error(
    match_zipcodes("45220", "00021"),
    "@zipcode must be exactly five numeric digits"
  )
  expect_error(
    match_zipcodes("45220", "1234"),
    "@zipcode must be exactly five numeric digits"
  )
})
