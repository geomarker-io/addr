test_that("match_addr_number uses the documented addr_number examples", {
  x <- addr_number(
    prefix = "",
    digits = as.character(c(1, 10, 228, 11, 22, 22, 22, 10, 99897, NA)),
    suffix = ""
  )
  y <- addr_number(
    prefix = "",
    digits = as.character(c(12, 11, 10, 22)),
    suffix = ""
  )

  expect_equal(
    as.data.frame(match_addr_number(x, y)),
    as.data.frame(
      addr_number(
        prefix = c(
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          "",
          NA_character_,
          NA_character_
        ),
        digits = c(
          "10",
          "10",
          "22",
          "11",
          "22",
          "22",
          "22",
          "10",
          NA_character_,
          NA_character_
        ),
        suffix = c("", "", "", "", "", "", "", "", NA_character_, NA_character_)
      )
    )
  )

  expect_equal(
    as.data.frame(match_addr_number(x, y, osa_max_dist = 0L)),
    as.data.frame(
      addr_number(
        prefix = c(
          NA_character_,
          "",
          NA_character_,
          "",
          "",
          "",
          "",
          "",
          NA_character_,
          NA_character_
        ),
        digits = c(
          NA_character_,
          "10",
          NA_character_,
          "11",
          "22",
          "22",
          "22",
          "10",
          NA_character_,
          NA_character_
        ),
        suffix = c(
          NA_character_,
          "",
          NA_character_,
          "",
          "",
          "",
          "",
          "",
          NA_character_,
          NA_character_
        )
      )
    )
  )
})

test_that("match_addr_number treats NA and empty addr_number values as unmatched", {
  x <- addr_number(
    prefix = "",
    digits = c("", NA_character_, "10", "11"),
    suffix = ""
  )
  y <- addr_number(
    prefix = "",
    digits = c("", NA_character_, "12", "11"),
    suffix = ""
  )

  out <- match_addr_number(x, y)

  expect_true(is.na(out[1]))
  expect_true(is.na(out[2]))
  expect_false(is.na(out[3]))
  expect_false(is.na(out[4]))
  expect_equal(
    as.data.frame(out),
    as.data.frame(
      addr_number(
        prefix = c(NA_character_, NA_character_, "", ""),
        digits = c(NA_character_, NA_character_, "11", "11"),
        suffix = c(NA_character_, NA_character_, "", "")
      )
    )
  )
})

test_that("match_addr_number threshold controls whether borderline matches are returned", {
  x <- addr_number(prefix = "", digits = c("1", "228", "10"), suffix = "")
  y <- addr_number(prefix = "", digits = c("10", "22"), suffix = "")

  out_thresh_1 <- match_addr_number(x, y, osa_max_dist = 1L)
  out_thresh_0 <- match_addr_number(x, y, osa_max_dist = 0L)

  expect_equal(as.character(out_thresh_1), c("10", "22", "10"))
  expect_equal(as.character(out_thresh_0), c("", "", "10"))
  expect_false(is.na(out_thresh_1[1]))
  expect_true(is.na(out_thresh_0[1]))
  expect_false(is.na(out_thresh_1[2]))
  expect_true(is.na(out_thresh_0[2]))
  expect_false(is.na(out_thresh_1[3]))
  expect_false(is.na(out_thresh_0[3]))
})

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
  x <- c("45222", "45219", "45219", "45220", "45220", "", NA_character_)
  y <- c("42522", "45200", "45219", "45221", "45223", "45321", "")

  expect_equal(
    match_zipcodes(x, y),
    c("45221", "45219", "45219", "45221", "45221", NA_character_, NA_character_)
  )

  expect_equal(
    match_zipcodes(x, y, zip_variants = TRUE),
    c("45221", "45219", "45219", "45221", "45221", NA_character_, NA_character_)
  )

  expect_equal(
    match_zipcodes(x, y, zip_variants = FALSE),
    c(
      NA_character_,
      "45219",
      "45219",
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_
    )
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
