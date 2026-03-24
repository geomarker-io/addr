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

test_that("match_addr_street works", {
  my_streets <- addr_street(
    predirectional = "",
    premodifier = "",
    pretype = "",
    name = c(
      "Beechview",
      "Vivian",
      "Springfield",
      "Round Bottom",
      "Pfeiffer",
      "Beachview",
      "Vevan",
      "Srpingfield",
      "Square Top",
      "Pfeffer",
      "Wuhlper",
      ""
    ),
    posttype = c(
      "Cir",
      "Pl",
      "Pike",
      "Rd",
      "Rd",
      "Cir",
      "Pl",
      "Pike",
      "Rd",
      "Rd",
      "Ave",
      ""
    ),
    postdirectional = ""
  )
  the_streets <- nad_example_data()$nad_addr@street
  out <- match_addr_street(my_streets, the_streets)
  expect_identical(
    out@name,
    c(
      "BEECHVIEW",
      "VIVIAN",
      "SPRINGFIELD",
      "ROUND BOTTOM",
      "PFEIFFER",
      "BEECHVIEW",
      "VIVIAN",
      "SPRINGFIELD",
      NA,
      "PFEIFFER",
      "WOOLPER",
      NA
    )
  )
  expect_identical(
    out@posttype,
    c("Cir", "Pl", "Pike", "Rd", "Rd", "Cir", "Pl", "Pike", NA, "Rd", "Ave", NA)
  )
})

test_that("match_addr_street uses predirectional to resolve East 14th Street", {
  the_streets <- nad_example_data()$nad_addr@street
  x <- addr_street(
    predirectional = c("E", ""),
    premodifier = "",
    pretype = "",
    name = "14th",
    posttype = "Street",
    postdirectional = ""
  )

  out <- match_addr_street(x, the_streets)

  expect_identical(out@predirectional, c("E", NA_character_))
  expect_identical(out@name, c("14TH", NA_character_))
  expect_identical(out@posttype, c("St", NA_character_))
  expect_false(is.na(out[1]))
  expect_true(is.na(out[2]))
})

test_that("match_addr_street leaves NA and all-empty addr_street values unmatched", {
  the_streets <- nad_example_data()$nad_addr@street
  x <- addr_street(
    predirectional = c("", "", NA_character_),
    premodifier = c("", "", NA_character_),
    pretype = c("", "", NA_character_),
    name = c("", NA_character_, ""),
    posttype = c("", "Street", NA_character_),
    postdirectional = c("", "", NA_character_)
  )

  out <- match_addr_street(x, the_streets)

  expect_true(all(is.na(out)))
  expect_equal(
    as.data.frame(out),
    as.data.frame(
      addr_street(
        predirectional = rep(NA_character_, 3),
        premodifier = rep(NA_character_, 3),
        pretype = rep(NA_character_, 3),
        name = rep(NA_character_, 3),
        posttype = rep(NA_character_, 3),
        postdirectional = rep(NA_character_, 3),
        map_posttype = FALSE,
        map_directional = FALSE,
        map_pretype = FALSE,
        map_ordinal = FALSE
      )
    )
  )
})

test_that("match_addr_street matches typo and phonetic street-name variants", {
  the_streets <- nad_example_data()$nad_addr@street
  x <- addr_street(
    predirectional = "",
    premodifier = "",
    pretype = "",
    name = c("Srpingfield", "Wuhlper", "Pfeffer", "Vevan", "Beechveiw"),
    posttype = c("Pike", "Ave", "Rd", "Pl", "Cir"),
    postdirectional = ""
  )

  out <- match_addr_street(x, the_streets)

  expect_identical(
    out@name,
    c("SPRINGFIELD", "WOOLPER", "PFEIFFER", "VIVIAN", "BEECHVIEW")
  )
  expect_identical(out@posttype, c("Pike", "Ave", "Rd", "Pl", "Cir"))
  expect_false(any(is.na(out)))
})

# test_that("match_addr_street edge cases for Hamilton County, OH", {
#   the_streets <- nad_example_data()$nad_addr@street
#   match_addr_street(
#     addr_street(
#       predirectional = "",
#       premodifier = "",
#       pretype = "",
#       name = "Burnett",
#       posttype = "Ave",
#       postdirectional = ""
#     ),
#     the_streets
#   )
# })
