test_geocode_needed_counties <- function(
  county_fips = "39061",
  ZIP = "45220",
  source_zip = ZIP,
  source_zip_variant = "exact"
) {
  tibble::tibble(
    county_fips = county_fips,
    ZIP = ZIP,
    zip3 = substr(ZIP, 1, 3),
    zip2 = substr(ZIP, 4, 5),
    n_ranges = rep(10L, length(ZIP)),
    source_zip = source_zip,
    source_zip_variant = source_zip_variant
  )
}

test_geocode_manifest <- function(
  county_fips = character(),
  ZIP = character()
) {
  tibble::tibble(
    county_fips = county_fips,
    ZIP = ZIP,
    zip3 = substr(ZIP, 1, 3),
    zip2 = substr(ZIP, 4, 5),
    n_ranges = rep(10L, length(ZIP)),
    installed_at = rep("2026-07-02T00:00:00Z", length(ZIP))
  )
}

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
  out <- geocode(x, taf_install = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$addr, x)
  expect_true(all(is.na(out$matched_zipcode)))
  expect_true(all(is.na(out$matched_street)))
  expect_true(all(is.na(out$matched_geography)))
  expect_true(all(is.na(out$s2_cell)))
})

test_that("geocode warns when taf_install is FALSE and needed counties are missing", {
  local_mocked_bindings(
    taf_needed_counties = function(...) test_geocode_needed_counties(),
    taf_read_county_zip_manifest = function(...) test_geocode_manifest(),
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
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )

  expect_warning(
    geocode(x, taf_install = FALSE, progress = FALSE),
    "TAF files are missing for 1 county/counties needed for geocoding"
  )
})

test_that("geocode installs and verifies TAF before ZIP geocoding", {
  installed <- FALSE
  read_started <- FALSE
  local_mocked_bindings(
    taf_with_install_lock = function(year, version, expr) {
      eval(substitute(expr), parent.frame())
    },
    taf_install = function(...) {
      expect_false(read_started)
      installed <<- TRUE
      invisible("39061")
    },
    taf_needed_counties = function(...) test_geocode_needed_counties(),
    taf_read_county_zip_manifest = function(...) {
      if (installed) {
        return(test_geocode_manifest("39061", "45220"))
      }
      test_geocode_manifest()
    },
    geocode_zip = function(x, offset = 0L, ...) {
      expect_true(installed)
      read_started <<- TRUE
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
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )

  geocode(x, progress = FALSE)

  expect_true(read_started)
})

test_that("geocode stops before ZIP geocoding when TAF remains missing", {
  read_started <- FALSE
  local_mocked_bindings(
    taf_with_install_lock = function(year, version, expr) {
      eval(substitute(expr), parent.frame())
    },
    taf_install = function(...) invisible("39061"),
    taf_needed_counties = function(...) test_geocode_needed_counties(),
    taf_read_county_zip_manifest = function(...) test_geocode_manifest(),
    geocode_zip = function(...) {
      read_started <<- TRUE
      stop("should not read TAF ZIP files")
    }
  )
  x <- addr(
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )

  expect_error(
    geocode(x, progress = FALSE),
    "TAF files are still missing after installation"
  )
  expect_false(read_started)
})

test_that("geocode computes TAF needs once for the top-level plan", {
  needed_calls <- 0L
  manifest_calls <- 0L
  zip_calls <- 0L
  local_mocked_bindings(
    taf_needed_counties = function(x, ...) {
      needed_calls <<- needed_calls + 1L
      expect_equal(length(x), 2L)
      test_geocode_needed_counties(
        county_fips = c("39061", "39061"),
        ZIP = c("45219", "45220")
      )
    },
    taf_read_county_zip_manifest = function(...) {
      manifest_calls <<- manifest_calls + 1L
      test_geocode_manifest("39061", "45219")
    },
    geocode_zip = function(x, offset = 0L, ...) {
      zip_calls <<- zip_calls + 1L
      tibble::tibble(
        addr = x,
        matched_zipcode = x@place@zipcode,
        matched_street = x@street,
        matched_geography = s2::as_s2_geography(
          rep("POINT (-84.5 39.1)", length(x))
        )
      )
    }
  )
  x <- addr(
    addr_number(digits = c("1", "2", "1")),
    addr_street(
      name = c("Main", "Elm", "Main"),
      posttype = c("St", "St", "St"),
      map_posttype = FALSE
    ),
    addr_place(zipcode = c("45219", "45220", "45219"))
  )

  out <- geocode(x, taf_install = FALSE, progress = FALSE)

  expect_equal(needed_calls, 1L)
  expect_equal(manifest_calls, 1L)
  expect_equal(zip_calls, 2L)
  expect_equal(nrow(out), 3L)
  expect_equal(out$addr, x)
})

test_that("geocode keeps missing zipcode rows with geocoded rows", {
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
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
  out <- geocode(x, taf_install = FALSE)
  expect_equal(out$addr, x)
  expect_true(is.na(out$matched_zipcode[1]))
  expect_equal(out$matched_zipcode[2], "45220")
  expect_true(is.na(out$matched_street[1]))
  expect_equal(format(out$matched_street[2]), "Main")
  expect_true(is.na(out$s2_cell[1]))
  expect_false(is.na(out$s2_cell[2]))
})

test_that("geocode_stage classifies staged geocode results", {
  x <- as_addr(c(
    "10 Main St Cincinnati OH 45220",
    "11 Main St Cincinnati OH 45220",
    "12 Main St Cincinnati OH 45220",
    "13 Main St Cincinnati OH 45220",
    "14 Main St Cincinnati OH 45220",
    "15 Main St"
  ))
  matched_street <- addr_street(
    name = c("Main", "Main", "Main", "Main", NA_character_, "Main"),
    posttype = c("St", "St", "St", "St", NA_character_, "St")
  )
  matched_geography <- s2::as_s2_geography(c(
    "POINT (-84.5 39.1)",
    NA_character_,
    "POINT (-84.5 39.1)",
    NA_character_,
    NA_character_,
    "POINT (-84.5 39.1)"
  ))
  gcd <- tibble::tibble(
    addr = x,
    matched_zipcode = c(
      "45220",
      "45220",
      "45221",
      "45221",
      NA_character_,
      NA_character_
    ),
    matched_street = matched_street,
    matched_geography = matched_geography
  )

  expect_equal(
    geocode_stage(gcd),
    ordered(
      c("range", "street", "range_variant", "street_variant", "none", "none"),
      levels = c("none", "street_variant", "street", "range_variant", "range")
    )
  )
})

test_that("geocode_table returns a JSON-safe flat table", {
  x <- as_addr(c(
    "10 Main St Cincinnati OH 45220",
    "11 Oak Rd Cincinnati OH 45221"
  ))
  matched_street <- addr_street(
    name = c("Main", NA_character_),
    posttype = c("St", NA_character_)
  )
  s2_cell <- s2::as_s2_cell(c(
    "808fbfffffffffff",
    NA_character_
  ))
  matched_geography <- s2::as_s2_geography(c(
    "POINT (-84.5 39.1)",
    NA_character_
  ))
  gcd <- tibble::tibble(
    addr = x,
    matched_zipcode = c("45220", NA_character_),
    matched_street = matched_street,
    matched_geography = matched_geography,
    s2_cell = s2_cell
  )

  out <- geocode_table(gcd)
  expected_names <- c(
    "addr",
    "geocode_stage",
    "matched_zipcode",
    "matched_street",
    "s2_cell"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), expected_names)
  expect_equal(out$addr, as.character(x))
  expect_equal(out$geocode_stage, c("range", "none"))
  expect_equal(out$matched_zipcode, c("45220", NA_character_))
  expect_equal(out$matched_street, as.character(matched_street))
  expect_type(out$s2_cell, "character")
  expect_equal(out$s2_cell, as.character(s2_cell))
  expect_true(is.na(out$s2_cell[[2]]))
  expect_true(all(vapply(out, is.atomic, logical(1))))
  expect_false(any(vapply(out, inherits, logical(1), "addr")))
  expect_false(any(vapply(out, inherits, logical(1), "addr_street")))
  expect_false(any(vapply(out, inherits, logical(1), "s2_geography")))
  expect_false(any(vapply(out, inherits, logical(1), "s2_cell")))
})

test_that("geocode_table omits S2 cell when geocode output omits it", {
  x <- as_addr(c(
    "10 Main St Cincinnati OH 45220",
    "11 Oak Rd Cincinnati OH 45221"
  ))
  matched_street <- addr_street(
    name = c("Main", NA_character_),
    posttype = c("St", NA_character_)
  )
  gcd <- tibble::tibble(
    addr = x,
    matched_zipcode = c("45220", NA_character_),
    matched_street = matched_street,
    matched_geography = s2::as_s2_geography(c(
      "POINT (-84.5 39.1)",
      NA_character_
    ))
  )

  out <- geocode_table(gcd)

  expect_equal(
    names(out),
    c("addr", "geocode_stage", "matched_zipcode", "matched_street")
  )
  expect_equal(out$geocode_stage, c("range", "none"))
  expect_false("s2_cell" %in% names(out))
})

test_that("geocode forwards street matching arguments to geocode_zip", {
  seen <- NULL
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
    geocode_zip = function(
      x,
      offset = 0L,
      name_phonetic_dist = 1L,
      name_fuzzy_dist = 2L,
      match_street_type = c("exact", "compatible", "ignore"),
      match_street_directional = c("exact", "swap", "ignore"),
      zip_variants = TRUE,
      zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
      ...
    ) {
      seen <<- list(
        name_phonetic_dist = name_phonetic_dist,
        name_fuzzy_dist = name_fuzzy_dist,
        match_street_type = match_street_type,
        match_street_directional = match_street_directional,
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
    match_street_type = "ignore",
    match_street_directional = "swap",
    zip_variants = FALSE,
    zip_variant = "swap",
    taf_install = FALSE,
    progress = FALSE
  )
  expect_equal(
    seen,
    list(
      name_phonetic_dist = 0L,
      name_fuzzy_dist = 1L,
      match_street_type = "ignore",
      match_street_directional = "swap",
      zip_variants = FALSE,
      zip_variant = "swap"
    )
  )
})

test_that("geocode deduplicates duplicate addr before ZIP geocoding", {
  seen_lengths <- integer()
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
    geocode_zip = function(x, offset = 0L, ...) {
      seen_lengths <<- c(seen_lengths, length(x))
      tibble::tibble(
        addr = x,
        matched_zipcode = x@place@zipcode,
        matched_street = x@street,
        matched_geography = s2::as_s2_geography(
          rep("POINT (-84.5 39.1)", length(x))
        )
      )
    }
  )
  x <- addr(
    addr_number(digits = c("1", "1", "2", "1")),
    addr_street(
      name = c("Main", "Main", "Elm", "Main"),
      posttype = c("St", "St", "St", "St"),
      map_posttype = FALSE
    ),
    addr_place(zipcode = rep("45219", 4))
  )

  out <- geocode(x, taf_install = FALSE, progress = FALSE)

  expect_equal(seen_lengths, 2L)
  expect_equal(nrow(out), 4L)
  expect_equal(out$addr, x)
  expect_equal(out$matched_zipcode, rep("45219", 4))
  expect_equal(format(out$matched_street), format(x@street))
})

test_that("geocode can skip S2 cell output", {
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
    geocode_zip = function(x, offset = 0L, ...) {
      tibble::tibble(
        addr = x,
        matched_zipcode = x@place@zipcode,
        matched_street = x@street,
        matched_geography = s2::as_s2_geography(
          rep("POINT (-84.5 39.1)", length(x))
        )
      )
    }
  )
  x <- addr(
    addr_number(digits = c("1", "1", "2")),
    addr_street(
      name = c("Main", "Main", "Elm"),
      posttype = c("St", "St", "St"),
      map_posttype = FALSE
    ),
    addr_place(zipcode = rep("45219", 3))
  )

  out <- geocode(
    x,
    taf_install = FALSE,
    add_s2_cell = FALSE,
    progress = FALSE
  )

  expect_equal(nrow(out), 3L)
  expect_equal(out$addr, x)
  expect_false("s2_cell" %in% names(out))
  expect_equal(
    geocode_stage(out),
    ordered(
      rep("range", 3),
      levels = c("none", "street_variant", "street", "range_variant", "range")
    )
  )
  expect_false("s2_cell" %in% names(geocode_table(out)))
})

test_that("geocode_zip forwards street matching arguments to match_addr_street", {
  seen <- NULL
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE, ...) {
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
    taf_install = FALSE,
    taf_check = FALSE,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_type = "ignore",
    match_street_directional = "swap"
  )
  expect_equal(
    seen,
    list(
      name_phonetic_dist = 0L,
      name_fuzzy_dist = 1L,
      match_street_type = "ignore",
      match_street_directional = "swap"
    )
  )
})

test_that("geocode_zip warns when taf_install is FALSE and needed counties are missing", {
  local_mocked_bindings(
    taf_needed_counties = function(...) test_geocode_needed_counties(),
    taf_read_county_zip_manifest = function(...) test_geocode_manifest(),
    taf_zip = function(zipcode, map = TRUE, ...) {
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
    match_addr_street = function(x, y, ...) y[1]
  )
  x <- addr(
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )

  expect_warning(
    geocode_zip(x, offset = 0, taf_install = FALSE),
    "TAF files are missing for 1 county/counties needed for geocoding"
  )
})

test_that("geocode_zip serializes TAF install before reading ZIP files", {
  lock_used <- FALSE
  installed <- FALSE
  local_mocked_bindings(
    taf_with_install_lock = function(year, version, expr) {
      lock_used <<- TRUE
      eval(substitute(expr), parent.frame())
    },
    taf_install = function(...) {
      installed <<- TRUE
      invisible("39061")
    },
    taf_needed_counties = function(...) test_geocode_needed_counties(),
    taf_read_county_zip_manifest = function(...) {
      if (installed) {
        return(test_geocode_manifest("39061", "45220"))
      }
      test_geocode_manifest()
    },
    taf_zip = function(zipcode, map = TRUE, ...) {
      expect_true(lock_used)
      expect_true(installed)
      tibble::tibble(
        ZIP = zipcode,
        addr_street = addr_street(name = "Main", posttype = "St"),
        side = "L",
        FROMHN = 1L,
        TOHN = 99L,
        PARITY = "B",
        OFFSET = 0,
        s2_geography = s2::as_s2_geography(
          "LINESTRING (-84 39, -84.1 39.1)"
        )
      )
    },
    match_addr_street = function(x, y, ...) y[1]
  )
  x <- addr(
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )

  geocode_zip(x, offset = 0)

  expect_true(lock_used)
})

test_that("geocode_zip respects zipcode variant controls", {
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE, ...) {
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

  out_swap <- geocode_zip(
    x,
    offset = 0,
    zip_variant = "swap",
    taf_install = FALSE,
    taf_check = FALSE
  )
  out_none <- geocode_zip(
    x,
    offset = 0,
    zip_variants = FALSE,
    taf_install = FALSE,
    taf_check = FALSE
  )

  expect_equal(out_swap$matched_zipcode, "42520")
  expect_equal(format(out_swap$matched_street), "Main St")
  expect_true(is.na(out_none$matched_zipcode))
  expect_true(is.na(out_none$matched_street))
})

test_that("geocode_zip matches duplicate TAF street ranges once", {
  matched_against <- NULL
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE, ...) {
      tibble::tibble(
        ZIP = rep("45219", 4),
        addr_street = addr_street(
          predirectional = rep("", 4),
          premodifier = rep("", 4),
          pretype = rep("", 4),
          name = c("Main", "Main", "Elm", "Elm"),
          posttype = c("St", "St", "St", "St"),
          postdirectional = rep("", 4)
        ),
        side = c("L", "R", "L", "R"),
        FROMHN = c(1L, 2L, 1L, 2L),
        TOHN = c(99L, 100L, 99L, 100L),
        PARITY = c("O", "E", "O", "E"),
        OFFSET = c("", "", "", ""),
        s2_geography = s2::as_s2_geography(
          rep("LINESTRING (-84 39, -84.1 39.1)", 4)
        )
      )
    },
    match_addr_street = function(x, y, ...) {
      matched_against <<- length(y)
      y[match(format(x), format(y))]
    }
  )
  x <- addr(
    addr_number(digits = c("11", "12")),
    addr_street(
      predirectional = c("", ""),
      premodifier = c("", ""),
      pretype = c("", ""),
      name = c("Main", "Main"),
      posttype = c("St", "St"),
      postdirectional = c("", "")
    ),
    addr_place(zipcode = c("45219", "45219"))
  )

  out <- geocode_zip(
    x,
    offset = 0,
    zip_variants = FALSE,
    taf_install = FALSE,
    taf_check = FALSE
  )

  expect_equal(matched_against, 2L)
  expect_equal(out$matched_zipcode, c("45219", "45219"))
  expect_equal(format(out$matched_street), c("Main St", "Main St"))
  expect_false(any(is.na(out$matched_geography)))
})

test_that("geocode_zip skips generated invalid zipcode variants", {
  taf_zip_calls <- list()
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE, ...) {
      taf_zip_calls <<- c(taf_zip_calls, list(zipcode))
      expect_false(any(is_invalid_zipcode(zipcode)))
      tibble::tibble(
        ZIP = character(),
        addr_street = addr_street()[0],
        FROMHN = integer(),
        TOHN = integer(),
        PARITY = character(),
        side = character(),
        OFFSET = numeric(),
        s2_geography = s2::as_s2_geography(character())
      )
    },
    match_addr_street = function(x, y, ...) {
      addr_street(rep(NA_character_, length(x)))
    }
  )
  x <- addr(
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "00100")
  )

  out <- geocode_zip(
    x,
    offset = 0,
    zip_variant = "minus1",
    taf_install = FALSE,
    taf_check = FALSE
  )

  expect_length(taf_zip_calls, 1L)
  expect_equal(taf_zip_calls[[1]], "00100")
  expect_true(is.na(out$matched_zipcode))
  expect_true(is.na(out$matched_street))
})

test_that("geocode reports ZIP and address context for ZIP-level errors", {
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
    geocode_use_mirai = function() FALSE,
    geocode_zip = function(...) {
      stop("boom", call. = FALSE)
    }
  )
  x <- addr(
    addr_number(digits = "10"),
    addr_street(name = "Main", posttype = "St"),
    addr_place(zipcode = "45220")
  )

  err <- tryCatch(
    geocode(x, taf_install = FALSE, progress = FALSE),
    error = function(cnd) cnd
  )
  msg <- conditionMessage(err)

  expect_s3_class(err, "error")
  expect_match(msg, "geocoding failed for ZIP 45220", fixed = TRUE)
  expect_match(msg, "Affected address examples:", fixed = TRUE)
  expect_match(msg, "Original error: boom", fixed = TRUE)
})

test_that("geocode_zip offsets matched points by TIGER side", {
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE, ...) {
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

  on_line <- geocode_zip(x, offset = 0, taf_install = FALSE, taf_check = FALSE)
  offset <- geocode_zip(x, offset = 10, taf_install = FALSE, taf_check = FALSE)

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

test_that("geocode_zip suppresses default offset for TIGER offset ranges", {
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE, ...) {
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
        OFFSET = c("Y", "N"),
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

  on_line <- geocode_zip(x, offset = 0, taf_install = FALSE, taf_check = FALSE)
  offset <- geocode_zip(x, offset = 10, taf_install = FALSE, taf_check = FALSE)

  expect_equal(s2::s2_y(offset$matched_geography[1]), 0, tolerance = 1e-8)
  expect_lt(s2::s2_y(offset$matched_geography[2]), 0)
  expect_equal(
    as.numeric(s2::s2_distance(
      on_line$matched_geography,
      offset$matched_geography
    )),
    c(0, 10),
    tolerance = 1e-5
  )
})

test_that("geocode_zip ignores matched TAF ranges with missing endpoints", {
  local_mocked_bindings(
    taf_zip = function(zipcode, map = TRUE, ...) {
      tibble::tibble(
        ZIP = rep(zipcode, 4),
        addr_street = addr_street(
          predirectional = rep("", 4),
          premodifier = rep("", 4),
          pretype = rep("", 4),
          name = rep("Barren River", 4),
          posttype = rep("Dr", 4),
          postdirectional = rep("", 4)
        ),
        side = rep("L", 4),
        FROMHN = c(101L, NA_integer_, NA_integer_, 147L),
        TOHN = c(143L, NA_integer_, NA_integer_, 153L),
        PARITY = rep("O", 4),
        OFFSET = rep("N", 4),
        s2_geography = s2::as_s2_geography(
          rep("LINESTRING (-84 39, -84.1 39.1)", 4)
        )
      )
    },
    match_addr_street = function(x, y, ...) y[1]
  )
  x <- addr(
    addr_number(digits = "145"),
    addr_street(name = "Barren River", posttype = "Dr"),
    addr_place(zipcode = "41018")
  )

  out <- geocode_zip(
    x,
    offset = 0,
    zip_variants = FALSE,
    taf_install = FALSE,
    taf_check = FALSE
  )

  expect_equal(out$matched_zipcode, "41018")
  expect_equal(format(out$matched_street), "Barren River Dr")
  expect_true(is.na(out$matched_geography))
})

test_that("geocode progress text shows ZIP and addr_street count", {
  expect_equal(
    geocode_progress_text("45219", 1234L, 56789L),
    "geocoding 45219 (1,234 addr to 56,789 addr_street)"
  )
})

test_that("geocode progress gets addr_street count from geocode_zip", {
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
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
  progress_output <- capture.output(geocode(
    x,
    progress = TRUE,
    taf_install = FALSE
  ))
  progress_text <- paste(progress_output, collapse = "\n")
  progress_text <- gsub("\033\\[[0-9;]*[[:alpha:]]", "", progress_text)
  expect_true(
    grepl("geocoding 45219 \\(2 addr to 4,599 addr_street\\)", progress_text)
  )
})

test_that("geocode uses mirai mapping when daemons are configured", {
  used_mirai <- FALSE
  seen_progress <- NULL
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
    geocode_use_mirai = function() TRUE,
    geocode_map_mirai = function(x, FUN, progress, ...) {
      used_mirai <<- TRUE
      seen_progress <<- progress
      lapply(x, FUN, ...)
    },
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
    addr_number(digits = c("1", "2")),
    addr_street(
      name = c("Main", "Elm"),
      map_posttype = FALSE,
      map_pretype = FALSE,
      map_directional = FALSE
    ),
    addr_place(zipcode = c("45219", "45220"))
  )
  progress_output <- capture.output(invisible(geocode(
    x,
    progress = TRUE,
    taf_install = FALSE
  )))
  expect_true(used_mirai)
  expect_true(seen_progress)
  progress_text <- paste(progress_output, collapse = "\n")
  expect_match(progress_text, "preparing geocoding input \\(2 addr\\)")
  expect_match(
    progress_text,
    "checking TIGER address feature files for 2 ZIP codes plus ZIP variants"
  )
  expect_match(progress_text, "grouped 2 unique addr into 2 ZIP groups")
  expect_match(
    progress_text,
    "dispatching 2 ZIP groups across mirai workers \\(2 unique addr\\)"
  )
  expect_match(progress_text, "combining geocode results from 2 result groups")
  expect_match(progress_text, "restoring geocode output to 2 input addr")
  expect_match(progress_text, "computing S2 cells")
  expect_match(progress_text, "geocoding complete")
})

test_that("geocode mirai status text reports counts without ETA", {
  status <- geocode_mirai_status_text(
    completed_groups = 3L,
    total_groups = 10L,
    completed_addr = 25L,
    total_addr = 200L,
    last_zip = "45219",
    last_addr = 7L,
    elapsed = 62
  )

  expect_equal(
    status,
    paste(
      "mirai geocoding: 3/10 ZIP groups complete;",
      "25/200 unique addr complete;",
      "last completed: 45219 (7 unique addr);",
      "elapsed 1m 02s"
    )
  )
  expect_false(grepl("ETA", status, fixed = TRUE))
})

test_that("geocode works with mirai daemons on voter addresses", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("pkgload")

  temp_root <- tempfile("addr-mirai-geocode-")
  dir.create(temp_root, recursive = TRUE, showWarnings = FALSE)
  withr::local_envvar(c(R_USER_DATA_DIR = temp_root))
  withr::local_options(
    addr.taf_catalog_dir = file.path(temp_root, "inst", "extdata")
  )

  year <- "2025"
  version <- "v1"
  county <- "39061"
  taf_dataset_path <- getFromNamespace("taf_dataset_path", ns = "addr")
  taf_write_catalog <- getFromNamespace("taf_write_catalog", ns = "addr")
  taf_write_county_zip_manifest <- getFromNamespace(
    "taf_write_county_zip_manifest",
    ns = "addr"
  )
  taf_county_zip_manifest_rows <- getFromNamespace(
    "taf_county_zip_manifest_rows",
    ns = "addr"
  )

  x <- as_addr(voter_addresses()[1:100])
  zip_ok <- !is.na(x@place@zipcode) & x@place@zipcode != ""
  x_ref <- x[zip_ok]
  ref_df <- unique(data.frame(
    FULLNAME = format(x_ref@street),
    ZIP = x_ref@place@zipcode,
    street_predirectional = x_ref@street@predirectional,
    street_premodifier = x_ref@street@premodifier,
    street_pretype = x_ref@street@pretype,
    street_name = x_ref@street@name,
    street_posttype = x_ref@street@posttype,
    street_postdirectional = x_ref@street@postdirectional,
    stringsAsFactors = FALSE
  ))

  n_ref <- nrow(ref_df)
  ref_df$LINEARID <- sprintf("L%05d", seq_len(n_ref))
  ref_df$side <- rep("L", n_ref)
  ref_df$FROMHN <- rep(1L, n_ref)
  ref_df$TOHN <- rep(99999L, n_ref)
  ref_df$PARITY <- rep("B", n_ref)
  ref_df$OFFSET <- rep(0, n_ref)
  ref_df$geometry_wkt <- rep(
    "LINESTRING (-84.5 39.1, -84.49 39.11)",
    n_ref
  )
  ref_df$street_tag_parsed <- rep(FALSE, n_ref)
  ref_df$county_fips <- county
  ref_df$zip3 <- substr(ref_df$ZIP, 1, 3)
  ref_df$zip2 <- substr(ref_df$ZIP, 4, 5)
  ref_df <- ref_df[
    c(
      "LINEARID",
      "FULLNAME",
      "side",
      "ZIP",
      "FROMHN",
      "TOHN",
      "PARITY",
      "OFFSET",
      "geometry_wkt",
      "street_predirectional",
      "street_premodifier",
      "street_pretype",
      "street_name",
      "street_posttype",
      "street_postdirectional",
      "street_tag_parsed",
      "county_fips",
      "zip3",
      "zip2"
    )
  ]
  ref_tbl <- tibble::as_tibble(ref_df)

  by_zip <- split(ref_tbl, ref_tbl$ZIP)
  dataset_path <- taf_dataset_path(year = year, version = version)
  for (zip in names(by_zip)) {
    zip_rows <- by_zip[[zip]]
    out_dir <- file.path(
      dataset_path,
      sprintf("zip3=%s", zip_rows$zip3[[1]]),
      sprintf("zip2=%s", zip_rows$zip2[[1]])
    )
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    nanoparquet::write_parquet(
      zip_rows[, setdiff(names(zip_rows), c("zip3", "zip2")), drop = FALSE],
      file.path(out_dir, sprintf("%s.parquet", county))
    )
  }

  manifest <- taf_county_zip_manifest_rows(ref_tbl, county = county)
  taf_write_county_zip_manifest(manifest, year = year, version = version)
  taf_write_catalog(manifest, year = year, version = version, root = temp_root)

  mirai::daemons(2)
  on.exit(mirai::daemons(0), add = TRUE)

  out <- geocode(
    x,
    year = year,
    version = version,
    taf_install = FALSE,
    progress = FALSE
  )

  has_number <- !is.na(x@number@digits) & x@number@digits != ""
  has_street_name <- !is.na(x@street@name) & x@street@name != ""
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 100L)
  expect_equal(out$addr, x)
  expect_equal(sum(!is.na(out$matched_zipcode)), sum(has_street_name))
  expect_equal(sum(!is.na(out$matched_street)), sum(has_street_name))
  expect_equal(
    sum(!is.na(out$matched_geography)),
    sum(has_number & has_street_name)
  )
  expect_equal(sum(!is.na(out$s2_cell)), sum(has_number & has_street_name))
})

test_that("geocode falls back to the default progress bar when mirai is unavailable", {
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
    geocode_use_mirai = function() FALSE,
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
  progress_output <- capture.output(invisible(geocode(
    x,
    progress = TRUE,
    taf_install = FALSE
  )))
  progress_text <- paste(progress_output, collapse = "\n")
  progress_text <- gsub("\033\\[[0-9;]*[[:alpha:]]", "", progress_text)
  expect_true(
    grepl("geocoding 45219 \\(2 addr to 4,599 addr_street\\)", progress_text)
  )
})

test_that("geocode progress can be disabled", {
  local_mocked_bindings(
    taf_needed_counties = function(...) taf_empty_needed_counties(),
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
  progress_output <- capture.output(invisible(geocode(
    x,
    progress = FALSE,
    taf_install = FALSE
  )))
  expect_equal(progress_output, character())
})
