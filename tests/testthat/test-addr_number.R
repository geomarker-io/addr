test_that("addr_number() recycles fields and formats", {
  num <- addr_number(
    prefix = "N",
    digits = c("10", "20"),
    suffix = c("A", "")
  )
  expect_s3_class(num, c("addr_number", "addr_part", "S7_object"))
  expect_equal(
    as.data.frame(num),
    structure(
      list(
        number_prefix = c("N", "N"),
        number_digits = c("10", "20"),
        number_suffix = c("A", "")
      ),
      class = "data.frame",
      row.names = c(NA, -2L)
    )
  )
  expect_equal(format(num), c("N10A", "N20"))
})

test_that("addr_number() drops empty components in format", {
  num <- addr_number(
    prefix = NA_character_,
    digits = "10",
    suffix = ""
  )

  expect_equal(format(num), "10")
})

test_that("addr_number() preserves empty strings", {
  num <- addr_number(
    prefix = "",
    digits = "",
    suffix = ""
  )
  expect_equal(
    as.data.frame(num),
    structure(
      list(
        number_prefix = "",
        number_digits = "",
        number_suffix = ""
      ),
      class = "data.frame",
      row.names = c(NA, -1L)
    )
  )
  expect_equal(format(num), "")
})

test_that("addr_number() validates digits", {
  expect_error(
    addr_number(
      prefix = NA_character_,
      digits = c("10", "A12"),
      suffix = NA_character_
    ),
    "@digits must contain only numeric characters"
  )
})

test_that("addr_number() rejects mismatched lengths", {
  expect_error(
    addr_number(
      prefix = c("N", "S"),
      digits = c("10", "20", "30"),
      suffix = "A"
    ),
    "addr_number fields must have length 1 or 3 for recycling"
  )
})
