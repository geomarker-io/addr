test_that("phonetic_street_key encodes ordinal and non-ordinal street names", {
  expect_identical(
    phonetic_street_key(c(
      "MEADOWLARK",
      "7TH",
      "5th",
      "FIFTH",
      "BURNET",
      "TWENTY FIRST",
      "main"
    )),
    c("M346", "#0007", "#0005", "#0005", "B653", "#0021", "M500")
  )
})

test_that("soundex returns expected four-character phonetic codes", {
  expect_identical(
    soundex(c(
      "Robert",
      "Rupert",
      "Rubin",
      "Ashcraft",
      "Tymczak",
      "Pfister",
      "A",
      "",
      " ",
      NA
    )),
    c(
      "R163",
      "R163",
      "R150",
      "A261",
      "T522",
      "P236",
      "A000",
      "0000",
      "0000",
      NA
    )
  )
})

test_that("map_ordinal_street_names maps ordinal words and normalizes ordinal numbers", {
  expect_identical(
    map_ordinal_street_names(c(
      "fifth",
      "7th",
      "main",
      "Twentieth",
      "twenty first",
      "",
      " ",
      NA
    )),
    c("5TH", "7TH", "main", "20TH", "21ST", "", " ", NA_character_)
  )
})

test_that("ordinal helpers detect, parse, and format ordinal street numbers", {
  expect_identical(
    is_ordinal_street_number(c("1ST", "2nd", "A1", "21", NA, "", " ")),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )

  expect_identical(
    ordinal_street_number(c("1ST", "2nd", "23RD", "44th")),
    c(1L, 2L, 23L, 44L)
  )

  expect_identical(
    canonical_ordinal(as.integer(c(1, 2, 3, 4, 11, 12, 13, 21, 22, 23, NA))),
    c(
      "1ST",
      "2ND",
      "3RD",
      "4TH",
      "11TH",
      "12TH",
      "13TH",
      "21ST",
      "22ND",
      "23RD",
      NA
    )
  )
})

test_that("parse_word_ordinal parses supported one- and two-word ordinal strings", {
  expect_identical(
    parse_word_ordinal(c(
      "fifth",
      "twentieth",
      "twenty first",
      "thirty second",
      "twenty-first",
      "twentieth first",
      "thirtieth second",
      "main",
      NA
    )),
    c(5L, 20L, 21L, 32L, 21L, NA, NA, NA, NA)
  )
})

test_that("ordinal helpers validate input types", {
  expect_error(soundex(1), "x must be a character vector")
  expect_error(map_ordinal_street_names(1), "x must be a character vector")
  expect_error(canonical_ordinal(c(1, 2, 3)), "typeof\\(n\\) == \"integer\"")
})
