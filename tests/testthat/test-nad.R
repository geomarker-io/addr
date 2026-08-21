legacy_nad_cache_fixture <- function() {
  x <- addr(
    addr_number(prefix = "N", digits = "10", suffix = "A"),
    addr_street(name = "MAIN", posttype = "ST"),
    addr_place(name = "CINCINNATI", state = "OH", zipcode = "45220")
  )
  number <- x@number
  street <- x@street
  place <- x@place
  attr(number, "prefix") <- "n"
  attr(street, "name") <- "Main"
  attr(street, "posttype") <- "St"
  attr(place, "name") <- "Cincinnati"
  attr(x, "number") <- number
  attr(x, "street") <- street
  attr(x, "place") <- place
  tibble::tibble(nad_addr = x, untouched = "Keep This")
}

test_that("nad() requires a cached binary when refresh_binary is no", {
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))

  expect_error(
    nad("Hamilton", "OH", refresh_source = "no", refresh_binary = "no"),
    "does not exist; set `refresh_binary = 'yes'`",
    fixed = TRUE
  )
  expect_error(
    nad("39061", refresh_source = "no", refresh_binary = "no"),
    "does not exist; set `refresh_binary = 'yes'`",
    fixed = TRUE
  )
  expect_error(
    nad("Haimlton", "OH", refresh_source = "no", refresh_binary = "no"),
    "was not found in `OH`",
    fixed = TRUE
  )
})

test_that("nad() rewrites legacy cached addr values to uppercase once", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  path <- nad_sd_path("Hamilton", "OH", 22L)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(legacy_nad_cache_fixture(), path)

  expect_message(
    out <- nad(
      "Hamilton",
      "OH",
      refresh_binary = "no",
      refresh_source = "no"
    ),
    "updated cached NAD addr values to uppercase"
  )

  expect_identical(format(out$nad_addr), "N10A MAIN ST CINCINNATI OH 45220")
  expect_identical(out$untouched, "Keep This")
  expect_identical(attr(out, "addr_nad_cache_case_version"), 1L)

  cached <- readRDS(path)
  expect_identical(format(cached$nad_addr), format(out$nad_addr))
  expect_identical(cached$untouched, "Keep This")
  expect_identical(attr(cached, "addr_nad_cache_case_version"), 1L)
  expect_silent(
    again <- nad(
      "Hamilton",
      "OH",
      refresh_binary = "no",
      refresh_source = "no"
    )
  )
  expect_identical(again, cached)
})

test_that("nad() returns uppercase values if cache rewriting fails", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  path <- nad_sd_path("Hamilton", "OH", 22L)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(legacy_nad_cache_fixture(), path)
  local_mocked_bindings(
    nad_cache_write_safely = function(...) stop("read-only cache")
  )

  expect_warning(
    out <- nad(
      "Hamilton",
      "OH",
      refresh_binary = "no",
      refresh_source = "no"
    ),
    "Returning uppercase values in memory"
  )

  expect_identical(format(out$nad_addr), "N10A MAIN ST CINCINNATI OH 45220")
  cached <- readRDS(path)
  expect_identical(format(cached$nad_addr), "n10A Main St Cincinnati OH 45220")
  expect_null(attr(cached, "addr_nad_cache_case_version", exact = TRUE))
})

test_that("nad() read from gdb on disk", {
  skip("it takes forever")
  nad_db <- nad_data_path(22L)
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

test_that("NAD source and derived paths use the stow workspace", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  managed_root <- stow::stow_path(package = "addr")
  workspace <- stow::stow_path(package = "addr", subdir = "nad")

  expect_identical(basename(managed_root), "stow")
  expect_identical(dirname(workspace), managed_root)
  expect_identical(nad_workspace_path(), workspace)
  expect_identical(dirname(nad_data_path(22L)), workspace)
  expect_identical(
    dirname(dirname(dirname(nad_sd_path("Hamilton", "OH", 22L)))),
    file.path(workspace, "v1")
  )
})

test_that("nad_download moves the legacy source instead of downloading it", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  nad_md <- nad_version_metadata(22L)
  legacy_root <- dirname(stow::stow_path(package = "addr"))
  legacy <- file.path(legacy_root, nad_md$flnm)
  writeBin(charToRaw("legacy"), legacy)

  local_mocked_bindings(
    nad_download_archive = function(...) {
      stop("should not download", call. = FALSE)
    }
  )

  expect_message(
    out <- nad_download(version = 22L, refresh_source = "yes"),
    "moved legacy NAD data path"
  )
  migrated <- nad_data_path(22L)
  expect_false(file.exists(legacy))
  expect_true(file.exists(migrated))
  expect_identical(rawToChar(readBin(migrated, "raw", n = 6L)), "legacy")
  expect_identical(
    out,
    file.path("/vsizip", migrated, sub("(_FGDB)?\\.zip$", ".gdb", nad_md$flnm))
  )
})

test_that("nad() migrates legacy derived county data", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  legacy_root <- dirname(stow::stow_path(package = "addr"))
  legacy <- file.path(
    legacy_root,
    "v1",
    "NAD_r22",
    "OH",
    "Hamilton.rds"
  )
  dir.create(dirname(legacy), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(source = "legacy"), legacy)

  expect_message(
    out <- nad(
      "Hamilton",
      "OH",
      refresh_binary = "no",
      refresh_source = "no"
    ),
    "moved legacy NAD data path"
  )
  migrated <- nad_sd_path("Hamilton", "OH", 22L)
  expect_false(file.exists(legacy))
  expect_true(file.exists(migrated))
  expect_identical(out, list(source = "legacy"))
})

test_that("NAD does not search the transient stow 0.2 workspace", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  managed_root <- stow::stow_path(package = "addr")
  old_workspace <- file.path(dirname(managed_root), "nad")
  old_source <- file.path(old_workspace, "NAD_r22.zip")
  dir.create(old_workspace, recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("stow 0.2"), old_source)

  expect_error(
    nad_download(version = 22L, refresh_source = "no"),
    "does not exist; set `refresh_source = 'yes'`",
    fixed = TRUE
  )
  expect_true(file.exists(old_source))
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
      "` or set `R_USER_DATA_DIR` so ",
      "`stow::stow_path(package = \"addr\", subdir = \"nad\")`"
    ),
    fixed = TRUE
  )
})
