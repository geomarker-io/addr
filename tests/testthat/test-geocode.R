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

test_that("geocode forwards offset to geocode_zip", {
  seen_offset <- NULL
  local_mocked_bindings(
    geocode_zip = function(x, offset = 0L) {
      seen_offset <<- offset
      tibble::tibble(
        addr = x,
        matched_zipcode = rep(NA_character_, length(x)),
        matched_street = addr_street(rep(NA_character_, length(x))),
        matched_geography = s2::as_s2_geography(rep(NA_character_, length(x)))
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
    addr_place(zipcode = "45220")
  )

  geocode(x, offset = 25L)

  expect_identical(seen_offset, 25L)
})

test_that("geocode keeps missing zipcode rows with geocoded rows", {
  local_mocked_bindings(
    geocode_zip = function(x, offset = 0L) {
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
