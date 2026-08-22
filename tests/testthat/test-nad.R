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

test_that("nad version metadata is local and validates versions", {
  nad_23 <- nad_version_metadata()

  expect_equal(nad_23$source_size, 7601412707)
  expect_equal(
    nad_23$source_members,
    c(
      "TXT/NAD_r23.txt",
      "TXT/NationalAddressDatabaseMetadata.xml"
    )
  )
  expect_equal(
    nad_23$dlurl,
    paste0(
      "https://data.transportation.gov/api/views/fc2s-wawr/files/",
      "b189f78b-2262-44e8-b3b6-5c4094c12da5"
    )
  )
  expect_equal(eval(formals(nad)$refresh_source), c("no", "yes", "force"))
  expect_equal(eval(formals(nad_read)$refresh_source), c("no", "yes", "force"))
  expect_identical(eval(formals(nad)$version), 23L)
  expect_identical(eval(formals(nad_read)$version), 23L)
  expect_identical(eval(formals(nad_download)$version), 23L)
  expect_identical(eval(formals(nad_version_metadata)$version), 23L)
  expect_error(
    nad_version_metadata("latest"),
    "version must be an integer vector"
  )
  expect_error(
    nad_version_metadata(24L),
    "NAD version `24` is not supported; supported version: 23",
    fixed = TRUE
  )
})

test_that("native flat extraction filters records and selected columns", {
  root <- tempfile("nad-flat-extract-fixture-")
  dir.create(file.path(root, "TXT"), recursive = TRUE)
  writeLines(
    c(
      "OID_,State,County,Post_City",
      "1,IN,Ripley,Versailles",
      "2,OH,Hamilton,Cincinnati",
      "3,IN,Ripley,"
    ),
    file.path(root, "TXT", "NAD_r23.txt")
  )
  archive <- paste0(tempfile("nad-flat-extract-"), ".zip")
  withr::local_dir(root)
  utils::zip(
    zipfile = archive,
    files = "TXT/NAD_r23.txt",
    flags = "-q"
  )

  out <- nad_flat_extract(
    archive,
    "TXT/NAD_r23.txt",
    "IN",
    "Ripley",
    c("State", "County", "Post_City")
  )
  expect_named(out, c("State", "County", "Post_City"))
  expect_identical(out$State, c("IN", "IN"))
  expect_identical(out$County, c("Ripley", "Ripley"))
  expect_identical(out$Post_City, c("Versailles", NA_character_))
  expect_error(
    nad_flat_extract(
      archive,
      "TXT/NAD_r23.txt",
      "IN",
      "Ripley",
      "missing"
    ),
    "NAD CSV is missing field `missing`",
    fixed = TRUE
  )
})

test_that("nad_read routes through the flat extractor", {
  fields <- nad_source_fields()
  flat <- stats::setNames(rep(list(NA_character_), length(fields)), fields)
  flat$Add_Number <- "10"
  flat$St_Name <- "MAIN"
  flat$County <- "Ripley"
  flat$Post_City <- "VERSAILLES"
  flat$State <- "IN"
  flat$Zip_Code <- "47042"
  flat$UUID <- "fixture-uuid"
  flat$Latitude <- "39.071"
  flat$Longitude <- "-85.251"
  flat$DateUpdate <- "2026-06-30 00:00:00"
  calls <- list()

  local_mocked_bindings(
    nad_download = function(version, refresh_source) "managed-flat-source",
    nad_flat_extract = function(path, member, state, county, fields) {
      calls[[1L]] <<- list(
        path = path,
        member = member,
        state = state,
        county = county,
        fields = fields
      )
      flat
    },
    .package = "addr"
  )

  out <- nad_read("18137", refresh_source = "no")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_identical(out$uuid, "fixture-uuid")
  expect_identical(calls[[1L]]$path, "managed-flat-source")
  expect_identical(calls[[1L]]$member, "TXT/NAD_r23.txt")
  expect_identical(calls[[1L]]$state, "IN")
  expect_identical(calls[[1L]]$county, "Ripley")
  expect_identical(calls[[1L]]$fields, fields)
})

test_that("nad creates processed county data outside stow and reuses it offline", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  expected <- tibble::tibble(source = "flat")
  source_workspace <- stow::stow_path(package = "addr", subdir = "nad")
  calls <- 0L
  allow_source <- TRUE
  local_mocked_bindings(
    nad_read = function(county, state, version, refresh_source) {
      if (!allow_source) {
        stop("source was accessed after the county was cached")
      }
      calls <<- calls + 1L
      expect_identical(county, "18137")
      expect_null(state)
      expect_identical(version, 23L)
      expect_identical(refresh_source, "no")
      expected
    },
    .package = "addr"
  )

  expect_message(
    out <- nad("18137", refresh_binary = "yes", refresh_source = "no"),
    "installing from source"
  )
  path <- nad_sd_path("Ripley", "IN")
  expect_identical(
    path,
    file.path(
      tools::R_user_dir("addr", "data"),
      "v1",
      "nad",
      "23",
      "IN",
      "Ripley.rds"
    )
  )
  expect_true(file.exists(path))
  expect_identical(out, expected)
  expect_identical(readRDS(path), expected)
  expect_length(
    list.files(source_workspace, all.files = TRUE, no.. = TRUE),
    0L
  )
  allow_source <- FALSE
  expect_identical(
    nad("18137", refresh_binary = "no", refresh_source = "no"),
    expected
  )
  expect_identical(calls, 1L)
  expect_length(
    list.files(source_workspace, all.files = TRUE, no.. = TRUE),
    0L
  )
})

test_that("nad_download maps refresh modes to stow", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  managed <- file.path(
    stow::stow_path(package = "addr", subdir = "nad"),
    "managed-nad-flat"
  )
  calls <- list()

  local_mocked_bindings(
    stow = function(
      url,
      package,
      subdir,
      overwrite,
      offline,
      etag,
      validate
    ) {
      calls[[length(calls) + 1L]] <<- list(
        url = url,
        package = package,
        subdir = subdir,
        overwrite = overwrite,
        offline = offline,
        etag = etag,
        validate = validate
      )
      managed
    },
    .package = "stow"
  )

  expect_identical(
    nad_download(refresh_source = "yes"),
    managed
  )
  expect_identical(
    nad_download(version = 23L, refresh_source = "no"),
    managed
  )
  expect_identical(
    nad_download(version = 23L, refresh_source = "force"),
    managed
  )

  expect_length(calls, 3L)
  expect_true(all(vapply(
    calls,
    function(x) {
      identical(x$url, nad_version_metadata(23L)$dlurl) &&
        identical(x$package, "addr") &&
        identical(x$subdir, "nad") &&
        identical(x$etag, FALSE) &&
        is.function(x$validate)
    },
    logical(1)
  )))
  expect_identical(calls[[1L]]$overwrite, FALSE)
  expect_identical(calls[[1L]]$offline, FALSE)
  expect_identical(calls[[2L]]$overwrite, FALSE)
  expect_identical(calls[[2L]]$offline, TRUE)
  expect_identical(calls[[3L]]$overwrite, TRUE)
  expect_identical(calls[[3L]]$offline, FALSE)
})

test_that("NAD source validation requires both archive members", {
  root <- tempfile("nad-flat-fixture-")
  dir.create(file.path(root, "TXT"), recursive = TRUE)
  writeLines("address data", file.path(root, "TXT", "NAD_r23.txt"))
  writeLines(
    "<metadata />",
    file.path(root, "TXT", "NationalAddressDatabaseMetadata.xml")
  )
  archive_zip <- paste0(tempfile("nad-flat-archive-"), ".zip")
  withr::local_dir(root)
  utils::zip(
    zipfile = archive_zip,
    files = c(
      "TXT/NAD_r23.txt",
      "TXT/NationalAddressDatabaseMetadata.xml"
    ),
    flags = "-q"
  )
  archive <- sub("\\.zip$", "", archive_zip)
  expect_true(file.rename(archive_zip, archive))

  expect_true(nad_validate_flat_source(
    archive,
    nad_version_metadata()$source_members
  ))
  expect_false(nad_validate_flat_source(
    archive,
    c(nad_version_metadata()$source_members, "TXT/missing.txt")
  ))
  expect_false(nad_validate_flat_source(
    tempfile("missing-flat-source-"),
    nad_version_metadata()$source_members
  ))
  expect_false(nad_validate_flat_source(
    archive,
    nad_version_metadata()$source_members,
    file.info(archive)$size + 1
  ))

  local_mocked_bindings(
    unzip = function(...) stop("ZIP64 listing unavailable"),
    .package = "utils"
  )
  expect_true(nad_validate_flat_source(
    archive,
    nad_version_metadata()$source_members,
    file.info(archive)$size
  ))
})

test_that("NAD and TIGER separate managed sources from processed data", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  data_root <- tools::R_user_dir("addr", "data")
  managed_root <- stow::stow_path(package = "addr")
  source_workspace <- stow::stow_path(package = "addr", subdir = "nad")
  processed_path <- nad_sd_path("Hamilton", "OH")

  expect_identical(basename(managed_root), "stow")
  expect_identical(dirname(source_workspace), managed_root)
  expect_identical(
    processed_path,
    file.path(data_root, "v1", "nad", "23", "OH", "Hamilton.rds")
  )
  expect_false(startsWith(
    processed_path,
    paste0(source_workspace, .Platform$file.sep)
  ))
  expect_identical(
    taf_dataset_path(year = 2025L, version = "v1"),
    file.path(data_root, "v1", "tiger_addr_feat", "2025")
  )
})

test_that("NAD ignores processed data under the development stow layout", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  development_path <- file.path(
    stow::stow_path(package = "addr", subdir = "nad"),
    "v1",
    "NAD_r23",
    "IN",
    "Ripley.rds"
  )
  dir.create(dirname(development_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(tibble::tibble(source = "development stow layout"), development_path)

  processed_path <- nad_sd_path("Ripley", "IN")
  expect_error(
    nad("18137", refresh_binary = "no", refresh_source = "no"),
    paste0(processed_path, " does not exist"),
    fixed = TRUE
  )
  expect_true(file.exists(development_path))
  expect_false(file.exists(processed_path))
})
