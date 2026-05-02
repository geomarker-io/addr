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
      zip_variants = TRUE,
      zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
      ...
    ) {
      seen <<- list(
        name_phonetic_dist = name_phonetic_dist,
        name_fuzzy_dist = name_fuzzy_dist,
        match_street_predirectional = match_street_predirectional,
        match_street_posttype = match_street_posttype,
        match_street_pretype = match_street_pretype,
        match_street_postdirectional = match_street_postdirectional,
        zip_variants = zip_variants,
        zip_variant = zip_variant
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
    zip_variants = FALSE,
    zip_variant = "swap",
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
      match_street_postdirectional = TRUE,
      zip_variants = FALSE,
      zip_variant = "swap"
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

test_that("geocode_zip respects zipcode variant controls", {
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE) {
      if (identical(zipcode, "45220")) {
        return(tibble::tibble(
          ZIP = "45220",
          addr_street = addr_street(name = "Elm", posttype = "St"),
          FROMHN = 1L,
          TOHN = 99L,
          PARITY = "B",
          s2_geography = s2::as_s2_geography(
            "LINESTRING (-84 39, -84.1 39.1)"
          )
        ))
      }
      if (identical(zipcode, "42520")) {
        return(tibble::tibble(
          ZIP = "42520",
          addr_street = addr_street(name = "Main", posttype = "St"),
          FROMHN = 1L,
          TOHN = 99L,
          PARITY = "B",
          s2_geography = s2::as_s2_geography(
            "LINESTRING (-84 39, -84.1 39.1)"
          )
        ))
      }
      stop("unexpected ZIP")
    },
    match_addr_street = function(x, y, ...) {
      out <- y[format(y) == format(x)]
      if (length(out) == 0L) {
        return(addr_street(NA_character_))
      }
      out
    }
  )
  x <- addr(
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )

  out_swap <- geocode_zip(x, offset = 0, zip_variant = "swap")
  out_none <- geocode_zip(x, offset = 0, zip_variants = FALSE)

  expect_equal(out_swap$matched_zipcode, "42520")
  expect_equal(format(out_swap$matched_street), "Main St")
  expect_true(is.na(out_none$matched_zipcode))
  expect_true(is.na(out_none$matched_street))
})

test_that("geocode_zip offsets matched points by TIGER side", {
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE) {
      tibble::tibble(
        ZIP = rep("45219", 2),
        addr_street = addr_street(
          predirectional = c("", ""),
          premodifier = c("", ""),
          pretype = c("", ""),
          name = c("Main", "Main"),
          posttype = c("St", "St"),
          postdirectional = c("", "")
        ),
        side = c("L", "R"),
        FROMHN = c(1L, 2L),
        TOHN = c(99L, 100L),
        PARITY = c("O", "E"),
        OFFSET = c("", ""),
        s2_geography = s2::as_s2_geography(
          rep("LINESTRING (0 0, 0.01 0)", 2)
        )
      )
    },
    match_addr_street = function(x, y, ...) {
      y[rep(1L, length(x))]
    }
  )

  x <- addr(
    addr_number(digits = c("1", "2")),
    addr_street(name = c("Main", "Main"), posttype = c("St", "St")),
    addr_place(zipcode = c("45219", "45219"))
  )

  on_line <- geocode_zip(x, offset = 0)
  offset <- geocode_zip(x, offset = 10)

  expect_equal(s2::s2_y(on_line$matched_geography), c(0, 0), tolerance = 1e-8)
  expect_gt(s2::s2_y(offset$matched_geography[1]), 0)
  expect_lt(s2::s2_y(offset$matched_geography[2]), 0)
  expect_equal(
    as.numeric(s2::s2_distance(
      on_line$matched_geography,
      offset$matched_geography
    )),
    c(10, 10),
    tolerance = 1e-5
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
