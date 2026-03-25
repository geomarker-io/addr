test_that("addr_match stages zipcode street and number matching", {
  y <- as_addr(c(
    "10 MAIN ST CINCINNATI OH 45220",
    "11 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229",
    "10 ELM ST CINCINNATI OH 45220"
  ))
  x <- as_addr(c(
    "10 MAINE STREET CINCINNATI OH 45220",
    "12 MAIN ST CINCINNATI OH 45220",
    "10 MAIN ST CINCINNATI OH 45229",
    "10 MAIN ST CINCINNATI OH 45103",
    NA_character_
  ))

  out <- addr_match(x, y, progress = FALSE)

  expect_true(inherits(out, "addr"))
  expect_equal(format(out[1:3]), format(y[c(1, 2, 3)]))
  expect_true(is.na(out[4]))
  expect_true(is.na(out[5]))
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

test_that("addr_match works with packaged example data", {
  x <- suppressWarnings(as_addr(voter_addresses()[1:1000]))
  y <- nad_example_data()$nad_addr

  out <- addr_match(x, y, progress = FALSE)

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
