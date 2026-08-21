test_that("packaged example fixtures stay small and deterministic", {
  nad_data <- nad_example_data()
  voter_data <- voter_addresses()

  expect_s3_class(nad_data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(nad_data), 5000L)
  expect_equal(ncol(nad_data), 7L)

  expect_type(voter_data, "character")
  expect_length(voter_data, 2000L)
  expect_equal(length(unique(voter_data)), 2000L)

  prepared <- nad_example_data(match_prepared = TRUE)
  expect_s3_class(prepared, "addr_match_index")
  expect_identical(attr(prepared, "addr_match_index_version"), 1L)
  expect_true(all(vapply(
    as.data.frame(nad_data$nad_addr),
    function(x) identical(x, toupper(x)),
    logical(1)
  )))
})
