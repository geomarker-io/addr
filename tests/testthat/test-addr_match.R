test_that("addr_match stages zipcode street and number matching", {
  y <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229",
    "10 ELM ST CINCINNATI OH 45220"
  ))
  x <- as_addr(c(
    "10 MAINE STREET CINCINNATI OH 45220",
    "99 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229",
    "10 OAK ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45103",
    NA_character_
  ))
  out <- addr_match(x, y, progress = FALSE)
  expect_true(inherits(out, "addr"))
  expect_equal(format(out[1]), format(y[1]))
  expect_true(is.na(out[2]@number))
  expect_equal(out[2]@street@name, "MAIN")
  expect_equal(out[2]@place@zipcode, "45220")
  expect_equal(format(out[3]), format(y[3]))
  expect_true(is.na(out[4]@number))
  expect_true(is.na(out[4]@street))
  expect_equal(out[4]@place@zipcode, "45220")
  expect_true(is.na(out[5]))
  expect_true(is.na(out[6]))
})

test_that("addr_match progress output uses zipcode text and 80-char bars", {
  y <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229"
  ))
  x <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229"
  ))
  expect_equal(nchar(addr_progress_bar(1, 3), type = "width"), 80)
  progress_output <- capture.output({
    out <- addr_match(x, y, progress = TRUE)
  })
  progress_text <- paste(progress_output, collapse = "\n")
  progress_text <- gsub("\033\\[[0-9;]*[[:alpha:]]", "", progress_text)
  bar_lines <- regmatches(
    progress_text,
    gregexpr("\\[[=.]+\\]", progress_text)
  )[[1]]
  expect_true(grepl(
    "preparing reference addr vector",
    progress_text
  ))
  expect_true(grepl(
    "prepared reference addr vector in [0-9]+\\.[0-9]{2} seconds",
    progress_text
  ))
  expect_true(grepl(
    "matching addr vectors in 45220 \\(2 to 2\\)",
    progress_text
  ))
  expect_true(grepl(
    "matching addr vectors in 45229 \\(1 to 1\\)",
    progress_text
  ))
  expect_true(grepl(
    "matched addr vectors in [0-9]+\\.[0-9]{2} seconds",
    progress_text
  ))
  expect_true(length(bar_lines) > 0L)
  expect_true(all(nchar(bar_lines, type = "width") <= 80))
  expect_true(inherits(out, "addr"))
})

test_that("addr_match_prepare can show its own progress bar", {
  y <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229"
  ))

  progress_output <- capture.output({
    prepared <- addr_match_prepare(y, progress = TRUE)
  })
  progress_text <- paste(progress_output, collapse = "\n")
  progress_text <- gsub("\033\\[[0-9;]*[[:alpha:]]", "", progress_text)
  bar_lines <- regmatches(
    progress_text,
    gregexpr("\\[[=.]+\\]", progress_text)
  )[[1]]

  expect_true(grepl("preparing reference addr vector", progress_text))
  expect_true(grepl(
    "preparing reference addr vector in 45220 \\(2 addrs\\)",
    progress_text
  ))
  expect_true(grepl(
    "preparing reference addr vector in 45229 \\(1 addrs\\)",
    progress_text
  ))
  expect_true(grepl(
    "prepared reference addr vector in [0-9]+\\.[0-9]{2} seconds",
    progress_text
  ))
  expect_true(length(bar_lines) > 0L)
  expect_s3_class(prepared, "addr_match_index")
})

test_that("addr_match skips preparation message for prepared references", {
  y <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229"
  ))
  x <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229"
  ))
  progress_output <- capture.output({
    out <- addr_match(x, addr_match_prepare(y), progress = TRUE)
  })
  progress_text <- paste(progress_output, collapse = "\n")
  progress_text <- gsub("\033\\[[0-9;]*[[:alpha:]]", "", progress_text)
  expect_false(grepl("preparing reference addr vector", progress_text))
  expect_true(inherits(out, "addr"))
})

test_that("addr_match progress text formats counts with commas", {
  expect_equal(
    addr_progress_text("45219", 143L, 4599L),
    "matching addr vectors in 45219 (143 to 4,599)"
  )
  expect_equal(
    addr_progress_text("45219", 1234L, 56789L),
    "matching addr vectors in 45219 (1,234 to 56,789)"
  )
})

test_that("addr_match_prepare caches reference data for repeated matching", {
  y <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229",
    "10 MAIN ST CINCINNATI OH 45220"
  ))
  x <- as_addr(c(
    "10 MAINE STREET CINCINNATI OH 45220",
    "99 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229"
  ))
  prepared <- addr_match_prepare(y)
  out_raw <- addr_match(x, y, progress = FALSE)
  out_prepared <- addr_match(x, prepared, progress = FALSE)
  expect_s3_class(prepared, "addr_match_index")
  expect_equal(prepared$n_unique, 3L)
  expect_equal(sort(prepared$zipcodes), c("45220", "45229"))
  expect_equal(format(out_prepared), format(out_raw))
})

test_that("addr_match_stage classifies staged addr_match results", {
  y <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229",
    "10 ELM ST CINCINNATI OH 45220"
  ))
  x <- as_addr(c(
    "10 MAINE STREET CINCINNATI OH 45220",
    "99 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229",
    "10 OAK ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45103",
    NA_character_
  ))
  out <- addr_match(x, y, progress = FALSE)
  expect_equal(
    addr_match_stage(out),
    ordered(
      c("number", "street", "number", "zip", "none", "none"),
      levels = c("none", "zip", "street", "number")
    )
  )
})

test_that("addr_match_stage rejects non-addr_match structures in strict mode", {
  x <- addr(
    number = addr_number(digits = "10"),
    street = addr_street(),
    place = addr_place(zipcode = "45220")
  )
  expect_error(
    addr_match_stage(x),
    "x does not look like an addr_match result"
  )
  expect_equal(
    addr_match_stage(x, strict = FALSE),
    ordered("number", levels = c("none", "zip", "street", "number"))
  )
})

test_that("addr_match works with packaged prepared example data", {
  x <- as_addr(voter_addresses()[1:100])
  y <- nad_example_data(match_prepare = TRUE)
  out <- addr_match(x, y, progress = FALSE)
  stage <- addr_match_stage(out)
  expect_equal(sum(!is.na(out@number)), 89)
  expect_equal(sum(!is.na(out@street)), 94)
  expect_equal(sum(!is.na(out@place@zipcode)), 100)
  expect_equal(
    as.integer(table(stage)),
    c(0L, 6L, 5L, 89L)
  )
  expect_true(inherits(out, "addr"))
  expect_length(out, 100L)
  expect_equal(sum(!is.na(out)), 89)
  expect_equal(
    format(out[1:10]),
    c(
      "3359 QUEEN CITY Ave CINCINNATI OH 45238",
      "1040 KREIS Ln CINCINNATI OH 45205",
      "9960 DALY Rd CINCINNATI OH 45231",
      "413 VOLKERT Pl CINCINNATI OH 45219",
      "8519 LINDERWOOD Ln CINCINNATI OH 45255",
      "6361 BEECHMONT Ave CINCINNATI OH 45230",
      "10466 ADVENTURE Ln CINCINNATI OH 45242",
      "3156 LOOKOUT Cir CINCINNATI OH 45208",
      "310 WYOMING Ave CINCINNATI OH 45215",
      "118 SPRINGFIELD Pike CINCINNATI OH 45215"
    )
  )
})

test_that("addr_match honors matching tuning arguments", {
  y <- nad_example_data(match_prepare = TRUE)
  demo_addr <- function(number, street, type, zipcode, map_ordinal = TRUE) {
    addr(
      addr_number(prefix = "", digits = number, suffix = ""),
      addr_street(
        predirectional = "",
        premodifier = "",
        pretype = "",
        name = street,
        posttype = type,
        postdirectional = "",
        map_ordinal = map_ordinal
      ),
      addr_place(name = "", state = "", zipcode = zipcode)
    )
  }

  zip_true <- addr_match(
    demo_addr("2700", "Alice", "St", "45222"),
    y,
    zip_variants = TRUE,
    progress = FALSE
  )
  zip_false <- addr_match(
    demo_addr("2700", "Alice", "St", "45222"),
    y,
    zip_variants = FALSE,
    progress = FALSE
  )
  expect_identical(
    format(zip_true),
    "2700 ALICE St CINCINNATI OH 45221"
  )
  expect_identical(as.character(addr_match_stage(zip_true)), "number")
  expect_identical(format(zip_false), "")
  expect_identical(as.character(addr_match_stage(zip_false)), "none")

  fuzzy_name_on <- addr_match(
    demo_addr("10623", "Srpingfield", "Pike", "45215"),
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    progress = FALSE
  )
  fuzzy_name_off <- addr_match(
    demo_addr("10623", "Srpingfield", "Pike", "45215"),
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 0L,
    progress = FALSE
  )
  expect_identical(
    format(fuzzy_name_on),
    "10623 SPRINGFIELD Pike CINCINNATI OH 45215"
  )
  expect_identical(as.character(addr_match_stage(fuzzy_name_on)), "number")
  expect_identical(format(fuzzy_name_off), "45215")
  expect_identical(as.character(addr_match_stage(fuzzy_name_off)), "zip")

  phonetic_exact <- addr_match(
    demo_addr("173", "Wuhlper", "Ave", "45220"),
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 0L,
    progress = FALSE
  )
  expect_identical(
    format(phonetic_exact),
    "173 WOOLPER Ave CINCINNATI OH 45220"
  )
  expect_identical(as.character(addr_match_stage(phonetic_exact)), "number")

  ordinal_phonetic_on <- addr_match(
    demo_addr("12176", "8TH", "Ave", "45249"),
    y,
    name_phonetic_dist = 1L,
    name_fuzzy_dist = 0L,
    progress = FALSE
  )
  ordinal_phonetic_off <- addr_match(
    demo_addr("12176", "8TH", "Ave", "45249"),
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 0L,
    progress = FALSE
  )
  expect_identical(
    format(ordinal_phonetic_on),
    "12176 7TH Ave CINCINNATI OH 45249"
  )
  expect_identical(
    as.character(addr_match_stage(ordinal_phonetic_on)),
    "number"
  )
  expect_identical(format(ordinal_phonetic_off), "45249")
  expect_identical(as.character(addr_match_stage(ordinal_phonetic_off)), "zip")

  ordinal_fuzzy_on <- addr_match(
    demo_addr("12176", "7HT", "Ave", "45249", map_ordinal = FALSE),
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    progress = FALSE
  )
  ordinal_fuzzy_off <- addr_match(
    demo_addr("12176", "7HT", "Ave", "45249", map_ordinal = FALSE),
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 0L,
    progress = FALSE
  )
  expect_identical(
    format(ordinal_fuzzy_on),
    "12176 7TH Ave CINCINNATI OH 45249"
  )
  expect_identical(as.character(addr_match_stage(ordinal_fuzzy_on)), "number")
  expect_identical(format(ordinal_fuzzy_off), "45249")
  expect_identical(as.character(addr_match_stage(ordinal_fuzzy_off)), "zip")

  fuzzy_number_on <- addr_match(
    demo_addr("10622", "Springfield", "Pike", "45215"),
    y,
    number_fuzzy_dist = 1L,
    progress = FALSE
  )
  fuzzy_number_off <- addr_match(
    demo_addr("10622", "Springfield", "Pike", "45215"),
    y,
    number_fuzzy_dist = 0L,
    progress = FALSE
  )
  expect_identical(
    format(fuzzy_number_on),
    "10623 SPRINGFIELD Pike CINCINNATI OH 45215"
  )
  expect_identical(as.character(addr_match_stage(fuzzy_number_on)), "number")
  expect_identical(format(fuzzy_number_off), "SPRINGFIELD Pike 45215")
  expect_identical(as.character(addr_match_stage(fuzzy_number_off)), "street")
})

test_that("addr_match honors pretype and postdirectional matching toggles", {
  demo_addr <- function(number, name, pretype = "", postdirectional = "") {
    addr(
      addr_number(prefix = "", digits = number, suffix = ""),
      addr_street(
        predirectional = "",
        premodifier = "",
        pretype = pretype,
        name = name,
        posttype = "Rd",
        postdirectional = postdirectional,
        map_pretype = FALSE,
        map_posttype = FALSE,
        map_directional = FALSE,
        map_ordinal = FALSE
      ),
      addr_place(name = "Testville", state = "OH", zipcode = "45220")
    )
  }

  y <- vctrs::vec_c(
    demo_addr("10", "Main"),
    demo_addr("10", "Main", pretype = "US Hwy", postdirectional = "E")
  )
  x <- demo_addr("10", "Mian", pretype = "US Hwy", postdirectional = "E")

  out_pretype_optional <- addr_match(
    x,
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_pretype = FALSE,
    progress = FALSE
  )
  out_default <- addr_match(
    x,
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    progress = FALSE
  )
  out_postdir_required <- addr_match(
    x,
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_postdirectional = TRUE,
    progress = FALSE
  )

  expect_identical(format(out_pretype_optional), "10 Main Rd Testville OH 45220")
  expect_identical(
    format(out_default),
    "10 US Hwy Main Rd E Testville OH 45220"
  )
  expect_identical(
    format(out_postdir_required),
    "10 US Hwy Main Rd E Testville OH 45220"
  )
})

test_that("addr_match can make predirectional and posttype optional", {
  demo_addr <- function(name, type = "Rd", predirectional = "") {
    addr(
      addr_number(prefix = "", digits = "10", suffix = ""),
      addr_street(
        predirectional = predirectional,
        premodifier = "",
        pretype = "",
        name = name,
        posttype = type,
        postdirectional = "",
        map_pretype = FALSE,
        map_posttype = FALSE,
        map_directional = FALSE,
        map_ordinal = FALSE
      ),
      addr_place(name = "Testville", state = "OH", zipcode = "45220")
    )
  }

  y_pred <- demo_addr("14th", type = "St", predirectional = "E")
  x_pred <- demo_addr("14th", type = "St")

  out_pred_required <- addr_match(x_pred, y_pred, progress = FALSE)
  out_pred_optional <- addr_match(
    x_pred,
    y_pred,
    match_street_predirectional = FALSE,
    progress = FALSE
  )

  expect_identical(format(out_pred_required), "45220")
  expect_identical(format(out_pred_optional), "10 E 14TH St Testville OH 45220")

  y_type <- demo_addr("Oak", type = "Rd")
  x_type <- demo_addr("Oka", type = "Ave")

  out_type_required <- addr_match(
    x_type,
    y_type,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    progress = FALSE
  )
  out_type_optional <- addr_match(
    x_type,
    y_type,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_posttype = FALSE,
    progress = FALSE
  )

  expect_identical(format(out_type_required), "45220")
  expect_identical(format(out_type_optional), "10 Oak Rd Testville OH 45220")
})

test_that("nad_example_data can return prepared match data", {
  prepared <- nad_example_data(match_prepare = TRUE)
  expect_s3_class(prepared, "addr_match_index")
})
