test_that("nad() uses included cached object for Hamilton, OH", {
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  d <- nad("Hamilton", "OH", refresh_source = "no", refresh_binary = "no")
  expect_s3_class(d, "data.frame")
  d_fips <- nad("39061", refresh_source = "no", refresh_binary = "no")
  expect_s3_class(d_fips, "data.frame")

  nad("Haimlton", "OH", refresh_source = "no", refresh_binary = "no") |>
    expect_error("was not found in `OH`")
})

test_that("nad() read from gdb on disk", {
  nad_db <- nad_data_path(22L)
  if (!file.exists(nad_db)) {
    skip("nad gdb not installed")
  }
  skip("it takes forever")
  d <- nad("King", "TX", version = 22L, refresh_binary = "force")
})

test_that("nad version metadata is local and validates versions", {
  expect_equal(nad_version_metadata(22L)$flnm, "NAD_r22.zip")
  expect_equal(eval(formals(nad)$refresh_source), c("no", "yes", "force"))
  expect_equal(eval(formals(nad_read)$refresh_source), c("no", "yes", "force"))
  expect_error(
    nad_version_metadata("latest"),
    "version must be an integer vector"
  )
  expect_error(
    nad_version_metadata(23L),
    "NAD version `23` is not supported"
  )
})

test_that("nad_download() resumes interrupted downloads from a .part file", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  nad_md <- nad_version_metadata(22L)
  dest <- nad_data_path(22L)
  partial_dest <- paste0(dest, ".part")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("abc"), partial_dest)

  local_mocked_bindings(
    nad_download_archive = function(url, dest) {
      expect_equal(url, nad_md$dlurl)
      expect_equal(dest, partial_dest)
      expect_true(file.exists(dest))
      expect_equal(file.info(dest)$size[[1]], 3)
      con <- file(dest, open = "ab")
      on.exit(close(con), add = TRUE)
      writeBin(charToRaw("def"), con)
      invisible(dest)
    }
  )

  out <- nad_download(version = 22L, refresh_source = "yes")

  expect_equal(
    out,
    file.path("/vsizip", dest, sub("(_FGDB)?\\.zip$", ".gdb", nad_md$flnm))
  )
  expect_true(file.exists(dest))
  expect_false(file.exists(partial_dest))
  expect_equal(rawToChar(readBin(dest, "raw", n = 6L)), "abcdef")
})

test_that("nad_download() force refresh clears completed and partial downloads", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  nad_md <- nad_version_metadata(22L)
  dest <- nad_data_path(22L)
  partial_dest <- paste0(dest, ".part")
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("old"), dest)
  writeBin(charToRaw("partial"), partial_dest)

  local_mocked_bindings(
    nad_download_archive = function(url, dest) {
      expect_equal(url, nad_md$dlurl)
      expect_equal(dest, partial_dest)
      expect_false(file.exists(dest))
      writeBin(charToRaw("new"), dest)
      invisible(dest)
    }
  )

  nad_download(version = 22L, refresh_source = "force")

  expect_true(file.exists(dest))
  expect_false(file.exists(partial_dest))
  expect_equal(rawToChar(readBin(dest, "raw", n = 3L)), "new")
})

test_that("nad_download() reports manual placement guidance after failures", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  dest <- nad_data_path(22L)

  local_mocked_bindings(
    nad_download_archive = function(url, dest) {
      stop("network broke", call. = FALSE)
    }
  )

  expect_error(
    nad_download(version = 22L, refresh_source = "yes"),
    paste0(
      "If you can download it another way, place it at `",
      dest,
      "` or set `R_USER_DATA_DIR`"
    ),
    fixed = TRUE
  )
})
