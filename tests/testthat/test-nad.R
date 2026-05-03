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
    "NAD_r22_FGDB.zip"
  )
  if (!file.exists(nad_db)) {
    skip("nad gdb not installed")
  }
  skip("it takes forever")
  d <- nad("King", "TX", release = "NAD_r22_FGDB.zip", refresh_binary = "force")
})

test_that("nad_download() resumes interrupted downloads from a .part file", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  release <- "NAD_r22_FGDB.zip"
  dest <- file.path(tools::R_user_dir("addr", "data"), release)
  partial_dest <- paste0(dest, ".part")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("abc"), partial_dest)

  local_mocked_bindings(
    fetch_nad_metadata = function() {
      list(
        flnm = release,
        dlurl = "https://example.com/NAD_r22_FGDB.zip"
      )
    },
    nad_download_archive = function(url, dest) {
      expect_equal(url, "https://example.com/NAD_r22_FGDB.zip")
      expect_equal(dest, partial_dest)
      expect_true(file.exists(dest))
      expect_equal(file.info(dest)$size[[1]], 3)
      con <- file(dest, open = "ab")
      on.exit(close(con), add = TRUE)
      writeBin(charToRaw("def"), con)
      invisible(dest)
    }
  )

  out <- nad_download(release = "latest", refresh_source = "yes")

  expect_equal(
    out,
    file.path("/vsizip", dest, sub("(_FGDB)?\\.zip$", ".gdb", release))
  )
  expect_true(file.exists(dest))
  expect_false(file.exists(partial_dest))
  expect_equal(rawToChar(readBin(dest, "raw", n = 6L)), "abcdef")
})

test_that("nad_download() force refresh clears completed and partial downloads", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  release <- "NAD_r22_FGDB.zip"
  dest <- file.path(tools::R_user_dir("addr", "data"), release)
  partial_dest <- paste0(dest, ".part")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("old"), dest)
  writeBin(charToRaw("partial"), partial_dest)

  local_mocked_bindings(
    fetch_nad_metadata = function() {
      list(
        flnm = release,
        dlurl = "https://example.com/NAD_r22_FGDB.zip"
      )
    },
    nad_download_archive = function(url, dest) {
      expect_equal(url, "https://example.com/NAD_r22_FGDB.zip")
      expect_equal(dest, partial_dest)
      expect_false(file.exists(dest))
      writeBin(charToRaw("new"), dest)
      invisible(dest)
    }
  )

  nad_download(release = "latest", refresh_source = "force")

  expect_true(file.exists(dest))
  expect_false(file.exists(partial_dest))
  expect_equal(rawToChar(readBin(dest, "raw", n = 3L)), "new")
})

test_that("nad_download() reports manual placement guidance after failures", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  release <- "NAD_r22_FGDB.zip"
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
