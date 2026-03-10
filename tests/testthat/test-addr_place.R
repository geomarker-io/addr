test_that("addr_place() maps state and formats", {
  place <- addr_place(
    name = c("Washington", "San Juan"),
    state = c("District of Columbia", "Puerto Rico"),
    zipcode = c("20001", "00901")
  )

  expect_true(inherits(place, "addr_place"))
  expect_equal(
    as.data.frame(place),
    structure(
      list(
        place_name = c("Washington", "San Juan"),
        place_state = c("DC", "PR"),
        place_zipcode = c("20001", "00901")
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
    )
  )
  expect_equal(
    format(place),
    c("Washington DC 20001", "San Juan PR 00901")
  )
})

test_that("addr_place() validates zipcode", {
  expect_error(
    addr_place(
      name = "Cincinnati",
      state = "OH",
      zipcode = c("45220", "45220-2034")
    ),
    "@zipcode must be exactly five numeric digits"
  )
  expect_error(
    addr_place(
      name = "Cincinnati",
      state = "OH",
      zipcode = c("00021")
    ),
    "@zipcode must be exactly five numeric digits"
  )
  expect_error(
    addr_place(
      name = "Cincinnati",
      state = "OH",
      zipcode = c("00000")
    ),
    "@zipcode must be exactly five numeric digits"
  )
})

test_that("addr_place() preserves empty strings", {
  place <- addr_place(
    name = c("", "Columbus"),
    state = c("", "OH"),
    zipcode = c("", "43215")
  )

  expect_equal(
    as.data.frame(place),
    structure(
      list(
        place_name = c("", "Columbus"),
        place_state = c("", "OH"),
        place_zipcode = c("", "43215")
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
    )
  )
  expect_equal(
    format(place),
    c("", "Columbus OH 43215")
  )
})

test_that("addr_place() rejects mismatched lengths", {
  expect_error(
    addr_place(
      name = c("Cincinnati", "Columbus"),
      state = c("OH", "KY", "IN"),
      zipcode = NA_character_
    ),
    "addr_place fields must have length 1 or 3 for recycling"
  )
})
