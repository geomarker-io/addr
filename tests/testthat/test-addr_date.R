test_that("imputing date ranges works", {
  impute_date_ranges(c("2024-01-01", "2024-03-17", "2024-09-21")) |>
    expect_equal(
      list(
        start = as.Date(c("2024-01-01", "2024-02-08", "2024-06-19")),
        end = as.Date(c("2024-02-08", "2024-06-19", "2024-09-21"))
      )
    )

  impute_date_ranges(c("2024-01-01", "2024-03-17", "2024-09-21"),
    start_early = 30,
    end_late = 60
  ) |>
    expect_equal(
      list(
        start = as.Date(c("2023-12-02", "2024-02-08", "2024-06-19")),
        end = as.Date(c("2024-02-08", "2024-06-19", "2024-11-20"))
      )
    )

  impute_date_ranges("2024-06-02") |>
    expect_equal(list(
      start = as.Date("2024-06-02"),
      end = as.Date("2024-06-02")
    ))

  impute_date_ranges("2024-06-02", start_early = 14, end_late = 22) |>
    expect_equal(list(
      start = as.Date("2024-06-02") - 14,
      end = as.Date("2024-06-02") + 22
    ))

})

test_that("impute date ranges works with grouped df", {

  d <-
    tibble::tribble(
      ~id, ~encounter, ~date,
      "A", 1, "2024-01-01",
      "A", 2, "2024-03-17",
      "A", 3, "2024-09-21",
      "B", 1, "2023-11-29",
      "B", 2, "2024-09-22",
      "B", 3, "2024-09-29"
    ) |>
    dplyr::mutate(
      imputed_start_date = impute_date_ranges(date)$start,
      imputed_end_date = impute_date_ranges(date)$end,
      .by = "id"
    )

  expect_equal(nrow(d), 6)
  expect_equal(ncol(d), 5)
  expect_equal(d$imputed_start_date,
               as.Date(c("2024-01-01", "2024-02-08", "2024-06-19", "2023-11-29", "2024-04-26", "2024-09-25")))
  expect_equal(d$imputed_end_date,
               as.Date(c("2024-02-08", "2024-06-19", "2024-09-21", "2024-04-26", "2024-09-25", "2024-09-29")))

})
