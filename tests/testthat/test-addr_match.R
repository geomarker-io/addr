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

test_that("addr_match works with packaged example data", {
  x <- suppressWarnings(as_addr(voter_addresses()[1:1000]))
  y <- nad_example_data()$nad_addr

  out <- addr_match(x, y, progress = FALSE)
  stage <- addr_match_stage(out)

  expect_equal(sum(!is.na(out@number)), 875)
  expect_equal(sum(!is.na(out@street)), 903)
  expect_equal(sum(!is.na(out@place@zipcode)), 1000)
  expect_equal(
    as.integer(table(stage)),
    c(0L, 97L, 28L, 875L)
  )

  expect_true(inherits(out, "addr"))
  expect_length(out, 1000L)
  expect_equal(sum(!is.na(out)), 875)
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
