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
  path <- nad_sd_path("Hamilton", "OH")
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
  path <- nad_sd_path("Hamilton", "OH")
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
  nad_db <- nad_data_path(23L)
  d <- nad("King", "TX", version = 23L, refresh_binary = "force")
})

test_that("nad version metadata is local and validates versions", {
  nad_22 <- nad_version_metadata(22L)
  nad_23 <- nad_version_metadata(23L)

  expect_equal(nad_22$flnm, "NAD_r22.zip")
  expect_null(nad_22$dlurl)
  expect_equal(nad_23$flnm, "NAD_r23.zip")
  expect_equal(nad_23$flid, "1fb39f25-503e-4c5a-b4e5-c01e09179302")
  expect_equal(nad_23$flsz, "9.1 Gb")
  expect_equal(
    nad_23$fldt,
    as.POSIXct("2026-07-01 19:29:55", tz = "UTC")
  )
  expect_equal(
    nad_23$dlurl,
    paste0(
      "https://data.transportation.gov/api/views/yw36-suxr/files/",
      "1fb39f25-503e-4c5a-b4e5-c01e09179302",
      "?download=true&filename=NAD_r23.zip"
    )
  )
  expect_identical(eval(formals(nad)$version), 23L)
  expect_identical(eval(formals(nad_read)$version), 23L)
  expect_identical(eval(formals(nad_download)$version), 23L)
  expect_identical(eval(formals(nad_version_metadata)$version), 23L)
  expect_identical(eval(formals(nad_sd_path)$version), 23L)
  expect_identical(eval(formals(nad_data_path)$version), 23L)
  expect_equal(eval(formals(nad)$refresh_source), c("no", "yes", "force"))
  expect_equal(eval(formals(nad_read)$refresh_source), c("no", "yes", "force"))
  expect_error(
    nad_version_metadata("latest"),
    "version must be an integer vector"
  )
  expect_error(
    nad_version_metadata(24L),
    "NAD version `24` is not supported; supported versions: 22, 23",
    fixed = TRUE
  )
})

test_that("NAD source and derived paths use the stow workspace", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  managed_root <- stow::stow_path(package = "addr")
  workspace <- stow::stow_path(package = "addr", subdir = "nad")

  expect_identical(basename(managed_root), "stow")
  expect_identical(dirname(workspace), managed_root)
  expect_identical(nad_workspace_path(), workspace)
  expect_identical(basename(nad_data_path()), "NAD_r23.zip")
  expect_identical(dirname(nad_data_path(22L)), workspace)
  expect_identical(dirname(nad_data_path(23L)), workspace)
  expect_false(identical(nad_data_path(22L), nad_data_path(23L)))
  expect_identical(
    dirname(dirname(dirname(nad_sd_path("Hamilton", "OH", 22L)))),
    file.path(workspace, "v1")
  )
  expect_false(identical(
    nad_sd_path("Hamilton", "OH", 22L),
    nad_sd_path("Hamilton", "OH", 23L)
  ))
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

test_that("default NAD migration preserves legacy release 22 source", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  legacy_root <- dirname(stow::stow_path(package = "addr"))
  legacy <- file.path(legacy_root, "NAD_r22.zip")
  writeBin(charToRaw("legacy"), legacy)

  expect_message(
    workspace <- nad_migrate_legacy_data(),
    "moved legacy NAD data path"
  )

  migrated <- nad_data_path(22L)
  expect_identical(workspace, nad_workspace_path())
  expect_false(file.exists(legacy))
  expect_true(file.exists(migrated))
  expect_identical(rawToChar(readBin(migrated, "raw", n = 6L)), "legacy")
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
      version = 22L,
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
    "If you can download it another way, place it at `",
    fixed = TRUE
  )
  expect_true(file.exists(old_source))
})

test_that("nad_download() resumes release 23 downloads from a .part file", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  nad_md <- nad_version_metadata(23L)
  dest <- nad_data_path(23L)
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

  out <- nad_download(version = 23L, refresh_source = "yes")

  expect_equal(
    out,
    file.path("/vsizip", dest, sub("(_FGDB)?\\.zip$", ".gdb", nad_md$flnm))
  )
  expect_true(file.exists(dest))
  expect_false(file.exists(partial_dest))
  expect_equal(rawToChar(readBin(dest, "raw", n = 6L)), "abcdef")
})

test_that("nad_download() force refreshes release 23 source files", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  nad_md <- nad_version_metadata(23L)
  dest <- nad_data_path(23L)
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

  nad_download(version = 23L, refresh_source = "force")

  expect_true(file.exists(dest))
  expect_false(file.exists(partial_dest))
  expect_equal(rawToChar(readBin(dest, "raw", n = 3L)), "new")
})

test_that("nad_download() preserves unavailable release 22 source files", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  dest <- nad_data_path(22L)
  partial_dest <- nad_partial_path(dest)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("old"), dest)
  writeBin(charToRaw("partial"), partial_dest)

  expect_identical(
    nad_download(version = 22L, refresh_source = "no"),
    file.path("/vsizip", dest, "NAD_r22.gdb")
  )
  expect_identical(
    nad_download(version = 22L, refresh_source = "yes"),
    file.path("/vsizip", dest, "NAD_r22.gdb")
  )
  expect_error(
    nad_download(version = 22L, refresh_source = "force"),
    "the pinned source asset is no longer available from USDOT",
    fixed = TRUE
  )
  expect_equal(rawToChar(readBin(dest, "raw", n = 3L)), "old")
  expect_equal(rawToChar(readBin(partial_dest, "raw", n = 7L)), "partial")
})

test_that("nad_download() guides manual placement for missing release 22", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  dest <- nad_data_path(22L)

  expect_error(
    nad_download(version = 22L, refresh_source = "yes"),
    paste0(
      "the pinned source asset is no longer available from USDOT. ",
      "If you can download it another way, place it at `",
      dest
    ),
    fixed = TRUE
  )
  expect_error(
    nad_download(version = 22L, refresh_source = "no"),
    "If you can download it another way, place it at `",
    fixed = TRUE
  )
})

test_that("nad_download() reports manual placement guidance after failures", {
  data_root <- tempfile()
  withr::local_envvar(list("R_USER_DATA_DIR" = data_root))

  dest <- nad_data_path(23L)

  local_mocked_bindings(
    nad_download_archive = function(url, dest) {
      stop("network broke", call. = FALSE)
    }
  )

  expect_error(
    nad_download(version = 23L, refresh_source = "yes"),
    paste0(
      "If you can download it another way, place it at `",
      dest,
      "` or set `R_USER_DATA_DIR` so ",
      "`stow::stow_path(package = \"addr\", subdir = \"nad\")`"
    ),
    fixed = TRUE
  )
})
