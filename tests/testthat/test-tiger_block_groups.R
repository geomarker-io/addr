test_that("s2_join_tiger_bg works", {
  withr::local_options(timeout = 3600)
  s2_join_tiger_bg(x = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")), year = "2023") |>
    expect_identical(c("390610032001", "210370519034"))
})

test_that("s2_join_tiger_bg works with NAs", {
  withr::local_options(timeout = 3600)
  s2_join_tiger_bg(x = s2::as_s2_cell(c("8841b39a7c46e25f", NA, "8841a45555555555")), year = "2023") |>
    expect_identical(c("390610032001", NA, "210370519034"))
})
