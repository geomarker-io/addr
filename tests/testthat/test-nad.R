test_that("nad() uses included cached object for Hamilton, OH", {
  withr::local_envvar(list("R_USER_CACHE_DIR" = tempfile()))
  d <- nad("Hamilton", "OH", refresh_source = "no", refresh_binary = "no")
  expect_s3_class(d, "data.frame")
  d_fips <- nad("39061", refresh_source = "no", refresh_binary = "no")
  expect_s3_class(d_fips, "data.frame")

  nad("Haimlton", "OH", refresh_source = "no", refresh_binary = "no") |>
    expect_error("was not found in `OH`")
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
  d <- nad("King", "TX", release = "NAD_r21_FGDB.zip", refresh_binary = "force")
})
