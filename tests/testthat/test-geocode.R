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
    taf_missing_counties = function(...) {
      tibble::tibble(
        county_fips = "39061",
        ZIP = "45220",
        zip3 = "452",
        zip2 = "20",
        n_ranges = 10L,
        source_zip = "45220",
        source_zip_variant = "exact"
      )
    },
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

test_that("geocode keeps missing zipcode rows with geocoded rows", {
  local_mocked_bindings(
    taf_missing_counties = function(...) taf_empty_needed_counties(),
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

test_that("geocode forwards street matching arguments to geocode_zip", {
  seen <- NULL
  local_mocked_bindings(
    taf_missing_counties = function(...) taf_empty_needed_counties(),
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
    taf_install = FALSE,
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

test_that("geocode_zip warns when taf_install is FALSE and needed counties are missing", {
  local_mocked_bindings(
    taf_missing_counties = function(...) {
      tibble::tibble(
        county_fips = "39061",
        ZIP = "45220",
        zip3 = "452",
        zip2 = "20",
        n_ranges = 10L,
        source_zip = "45220",
        source_zip_variant = "exact"
      )
    },
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

test_that("geocode progress text shows ZIP and addr_street count", {
  expect_equal(
    geocode_progress_text("45219", 1234L, 56789L),
    "geocoding 45219 (1,234 addr to 56,789 addr_street)"
  )
})

test_that("geocode progress gets addr_street count from geocode_zip", {
  local_mocked_bindings(
    taf_missing_counties = function(...) taf_empty_needed_counties(),
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
  local_mocked_bindings(
    taf_missing_counties = function(...) taf_empty_needed_counties(),
    geocode_use_mirai = function() TRUE,
    geocode_map_mirai = function(x, FUN, ...) {
      used_mirai <<- TRUE
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
  expect_equal(progress_output, character())
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
    taf_missing_counties = function(...) taf_empty_needed_counties(),
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
    taf_missing_counties = function(...) taf_empty_needed_counties(),
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
