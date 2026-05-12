test_that("nad() uses included cached object for Hamilton, OH", {
  withr::local_envvar(list("R_USER_CACHE_DIR" = tempfile()))
  d <- nad("Hamilton", "OH")
  expect_s3_class(d, "data.frame")
  d_fips <- nad("39061")
  expect_s3_class(d_fips, "data.frame")

  nad("Haimlton", "OH", refresh_source = "no", refresh_binary = "no") |>
    expect_error("was not found in `OH`")
})

test_that("nad() read from gdb on disk", {
  nad_db <- nad_data_path("NAD_r22.zip")
  if (!file.exists(nad_db)) {
    skip("nad gdb not installed")
  }
  skip("it takes forever")
  d <- nad("King", "TX", release = "NAD_r22.zip", refresh_binary = "force")
})


test_that("nad_download() reports manual placement guidance after failures", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  release <- "NAD_r22.zip"
  dest <- file.path(tools::R_user_dir("addr", "data"), release)

  local_mocked_bindings(
    fetch_nad_metadata = function() {
      list(
        flnm = release,
        dlurl = "https://example.com/NAD_r22_FGDB.zip"
      )
    },
    nad_download_archive = function(url, dest) {
      stop("network broke", call. = FALSE)
    }
  )

  expect_error(
    nad_download(release = "latest", refresh_source = "yes"),
    paste0(
      "If you can download it another way, place it at `",
      dest,
      "` or set `R_USER_DATA_DIR`"
    ),
    fixed = TRUE
  )
})
