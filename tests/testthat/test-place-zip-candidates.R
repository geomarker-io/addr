test_that("normalize_place_name normalizes conservative Census suffixes", {
  expect_equal(
    normalize_place_name(c(
      "Cincinnati city",
      "West Chester township",
      "Anderson township",
      "Mason city",
      "Batavia village"
    )),
    c("CINCINNATI", "WEST CHESTER", "ANDERSON", "MASON", "BATAVIA")
  )

  expect_equal(
    normalize_place_name(c(
      "  St.   Louis city  ",
      "Example, city",
      "Bellingham CCD",
      "City of Industry",
      "San José CDP"
    )),
    c(
      "ST. LOUIS",
      "EXAMPLE",
      "BELLINGHAM CCD",
      "CITY OF INDUSTRY",
      "SAN JOSÉ"
    )
  )

  expect_equal(normalize_place_name(c(NA_character_, "")), c(NA, ""))
  expect_error(normalize_place_name(1), "x must be a character vector")
})

test_that("place_zip_candidates has a deterministic candidate schema", {
  expect_s3_class(place_zip_candidates, "data.frame")
  expect_identical(
    names(place_zip_candidates),
    c("place_name", "state", "zcta", "source")
  )
  expect_true(all(vapply(place_zip_candidates, is.character, logical(1))))
  expect_equal(nrow(place_zip_candidates), 149564L)

  expect_true(all(!is.na(place_zip_candidates$place_name)))
  expect_true(all(nzchar(place_zip_candidates$place_name)))
  expect_true(all(grepl("^[A-Z]{2}$", place_zip_candidates$state)))
  expect_true(all(grepl("^[0-9]{5}$", place_zip_candidates$zcta)))
  expect_true(all(place_zip_candidates$source %in% c(
    "place",
    "cousub",
    "both"
  )))

  candidate_key <- with(
    place_zip_candidates,
    paste(place_name, state, zcta, sep = "\r")
  )
  expect_equal(anyDuplicated(candidate_key), 0L)

  expected_order <- with(
    place_zip_candidates,
    order(state, place_name, zcta, method = "radix")
  )
  expect_identical(expected_order, seq_len(nrow(place_zip_candidates)))
})

test_that("place_zip_candidates contains known Ohio relationships", {
  cincinnati <- subset(
    place_zip_candidates,
    place_name == "CINCINNATI" & state == "OH" & zcta == "45202"
  )
  expect_equal(nrow(cincinnati), 1L)
  expect_equal(cincinnati$source, "both")

  anderson <- subset(
    place_zip_candidates,
    place_name == "ANDERSON" & state == "OH"
  )
  expect_setequal(anderson$zcta, c("45226", "45230", "45244", "45255"))
  expect_true(all(anderson$source == "cousub"))

  west_chester <- subset(
    place_zip_candidates,
    place_name == "WEST CHESTER" & state == "OH"
  )
  expect_setequal(
    west_chester$zcta,
    c("45011", "45014", "45069", "45241", "45246")
  )
  expect_true(all(west_chester$source == "cousub"))

  mason <- subset(
    place_zip_candidates,
    place_name == "MASON" & state == "OH"
  )
  expect_setequal(
    mason$zcta,
    c("45036", "45039", "45040", "45659", "45678", "45696")
  )

  batavia <- subset(
    place_zip_candidates,
    place_name == "BATAVIA" & state == "OH"
  )
  expect_setequal(batavia$zcta, c("45102", "45103"))
})
