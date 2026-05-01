test_that("geocode returns non-matches for missing zipcodes", {
  x <- addr(
    addr_number(digits = c("1", "2")),
    addr_street(
      name = c("Main", "Elm"),
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(zipcode = c(NA_character_, ""))
  )
  out <- geocode(x)
  expect_equal(nrow(out), 2L)
  expect_equal(out$addr, x)
  expect_true(all(is.na(out$matched_zipcode)))
  expect_true(all(is.na(out$matched_street)))
  expect_true(all(is.na(out$matched_geography)))
  expect_true(all(is.na(out$s2_cell)))
})

test_that("geocode keeps missing zipcode rows with geocoded rows", {
  local_mocked_bindings(
    geocode_zip = function(x, offset = 0L, ...) {
      tibble::tibble(
        addr = x,
        matched_zipcode = rep("45220", length(x)),
        matched_street = x@street,
        matched_geography = s2::as_s2_geography(
          rep("POINT (-84.5 39.1)", length(x))
        )
      )
    }
  )
  x <- addr(
    addr_number(digits = c("1", "2")),
    addr_street(
      name = c("Missing", "Main"),
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(zipcode = c(NA_character_, "45220"))
  )
  out <- geocode(x)
  expect_equal(out$addr, x)
  expect_true(is.na(out$matched_zipcode[1]))
  expect_equal(out$matched_zipcode[2], "45220")
  expect_true(is.na(out$matched_street[1]))
  expect_equal(format(out$matched_street[2]), "Main")
  expect_true(is.na(out$s2_cell[1]))
  expect_false(is.na(out$s2_cell[2]))
})

test_that("geocode forwards street matching arguments to geocode_zip", {
  seen <- NULL
  local_mocked_bindings(
    geocode_zip = function(
      x,
      offset = 0L,
      name_phonetic_dist = 1L,
      name_fuzzy_dist = 2L,
      match_street_predirectional = TRUE,
      match_street_posttype = TRUE,
      match_street_pretype = TRUE,
      match_street_postdirectional = FALSE,
      ...
    ) {
      seen <<- list(
        name_phonetic_dist = name_phonetic_dist,
        name_fuzzy_dist = name_fuzzy_dist,
        match_street_predirectional = match_street_predirectional,
        match_street_posttype = match_street_posttype,
        match_street_pretype = match_street_pretype,
        match_street_postdirectional = match_street_postdirectional
      )
      tibble::tibble(
        addr = x,
        matched_zipcode = rep("45219", length(x)),
        matched_street = x@street,
        matched_geography = s2::as_s2_geography(
          rep("POINT (-84.5 39.1)", length(x))
        )
      )
    }
  )
  x <- addr(
    addr_number(digits = "1"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45219")
  )
  geocode(
    x,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_predirectional = FALSE,
    match_street_posttype = FALSE,
    match_street_pretype = FALSE,
    match_street_postdirectional = TRUE,
    progress = FALSE
  )
  expect_equal(
    seen,
    list(
      name_phonetic_dist = 0L,
      name_fuzzy_dist = 1L,
      match_street_predirectional = FALSE,
      match_street_posttype = FALSE,
      match_street_pretype = FALSE,
      match_street_postdirectional = TRUE
    )
  )
})

test_that("geocode_zip forwards street matching arguments to match_addr_street", {
  seen <- NULL
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE) {
      tibble::tibble(
        ZIP = zipcode,
        addr_street = addr_street(name = "Main", posttype = "St"),
        FROMHN = 1L,
        TOHN = 99L,
        PARITY = "B",
        s2_geography = s2::as_s2_geography(
          "LINESTRING (-84 39, -84.1 39.1)"
        )
      )
    },
    match_addr_street = function(x, y, ...) {
      seen <<- list(...)
      y[1]
    }
  )
  x <- addr(
    addr_number(digits = ""),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45219")
  )
  geocode_zip(
    x,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_predirectional = FALSE,
    match_street_posttype = FALSE,
    match_street_pretype = FALSE,
    match_street_postdirectional = TRUE
  )
  expect_equal(
    seen,
    list(
      name_phonetic_dist = 0L,
      name_fuzzy_dist = 1L,
      match_street_predirectional = FALSE,
      match_street_posttype = FALSE,
      match_street_pretype = FALSE,
      match_street_postdirectional = TRUE
    )
  )
})

test_that("geocode progress text shows ZIP and addr_street count", {
  expect_equal(
    geocode_progress_text("45219", 1234L, 56789L),
    "geocoding 45219 (1,234 addr to 56,789 addr_street)"
  )
})

test_that("geocode progress gets addr_street count from geocode_zip", {
  local_mocked_bindings(
    geocode_zip = function(x, offset = 0L, progress_callback = NULL, ...) {
      progress_callback(4599L)
      tibble::tibble(
        addr = x,
        matched_zipcode = rep("45219", length(x)),
        matched_street = x@street,
        matched_geography = s2::as_s2_geography(
          rep("POINT (-84.5 39.1)", length(x))
        )
      )
    }
  )
  x <- addr(
    addr_number(digits = c("1", "2")),
    addr_street(
      name = c("Main", "Elm"),
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(zipcode = c("45219", "45219"))
  )
  progress_output <- capture.output(geocode(x, progress = TRUE))
  progress_text <- paste(progress_output, collapse = "\n")
  progress_text <- gsub("\033\\[[0-9;]*[[:alpha:]]", "", progress_text)
  expect_true(
    grepl("geocoding 45219 \\(2 addr to 4,599 addr_street\\)", progress_text)
  )
})

test_that("geocode progress can be disabled", {
  local_mocked_bindings(
    geocode_zip = function(x, offset = 0L, progress_callback = NULL, ...) {
      tibble::tibble(
        addr = x,
        matched_zipcode = rep("45219", length(x)),
        matched_street = x@street,
        matched_geography = s2::as_s2_geography(
          rep("POINT (-84.5 39.1)", length(x))
        )
      )
    }
  )
  x <- addr(
    addr_number(digits = "1"),
    addr_street(
      name = "Main",
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(zipcode = "45219")
  )
  progress_output <- capture.output(invisible(geocode(x, progress = FALSE)))
  expect_equal(progress_output, character())
})
