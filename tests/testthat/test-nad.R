test_that("nad() uses included cached object for Hamilton, OH", {
  d <- nad("Hamilton", "OH", refresh = "no")
  expect_s3_class(d, "data.frame")

  nad("Haimlton", "OH", refresh = "no") |>
    expect_error("to install from source NAD,")
})

test_that("nad() read from gdb on disk", {
  nad_db <- file.path(
    tools::R_user_dir("addr", "data"),
    "NAD_r21_FGDB.zip"
  )
  if (!file.exists(nad_db)) {
    skip("nad gdb not installed")
  }
  skip("it takes forever")
  d <- nad("King", "TX", refresh = "force")
})
