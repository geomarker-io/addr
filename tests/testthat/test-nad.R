nad_test_storage <- function(county = "Ripley", n = 1L) {
  tibble::tibble(
    address_number_prefix = rep.int("", n),
    address_number = as.character(seq_len(n) + 9L),
    address_number_suffix = rep.int("", n),
    street_predirectional = rep.int("", n),
    street_premodifier = rep.int("", n),
    street_pretype = rep.int("", n),
    street_name = rep.int("MAIN", n),
    street_posttype = rep.int("ST", n),
    street_postdirectional = rep.int("", n),
    subaddress = rep.int("", n),
    county = rep.int(county, n),
    place_name = rep.int("VERSAILLES", n),
    zipcode = rep.int("47042", n),
    uuid = paste0("fixture-", seq_len(n)),
    date_update = rep.int(as.Date("2026-06-30"), n),
    latitude = rep.int(39.071, n),
    longitude = rep.int(-85.251, n),
    national_grid = rep.int("", n),
    placement = rep.int("", n),
    address_class = rep.int("", n),
    address_type = rep.int("", n),
    parcel_id = rep.int("", n)
  )
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

test_that("nad version metadata is local and validates versions", {
  nad_23 <- nad_version_metadata()

  expect_false("nad_read" %in% getNamespaceExports("addr"))

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
  expect_equal(
    eval(formals(nad_install)$refresh_source),
    c("no", "yes", "force")
  )
  expect_equal(eval(formals(nad_read)$refresh_source), c("no", "yes", "force"))
  expect_identical(eval(formals(nad)$version), 23L)
  expect_identical(eval(formals(nad_install)$version), 23L)
  expect_identical(eval(formals(nad_dataset)$version), 23L)
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
  catalog <- tibble::as_tibble(nad_flat_catalog(
    archive,
    "TXT/NAD_r23.txt"
  ))
  expect_identical(
    catalog,
    tibble::tibble(
      state = c("IN", "OH"),
      source_county = c("Ripley", "Hamilton"),
      source_row_count = c(2, 1)
    )
  )
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

test_that("NAD source inventory maps county and independent-city labels", {
  source <- tibble::tibble(
    state = c("CT", "IL", "IN", "MD"),
    source_county = c(
      "Lower Connecticut River Valley Planning",
      "DeWitt",
      "Ripley",
      "Baltimore"
    ),
    source_row_count = c(30, 20, 10, 40)
  )

  catalog <- nad_catalog_rows(source)
  expect_identical(
    catalog$county_fips,
    c("09130", "17039", "18137", "24005")
  )
  expect_identical(
    catalog$source_county,
    c(
      "Lower Connecticut River Valley Planning",
      "DeWitt",
      "Ripley",
      "Baltimore"
    )
  )
  expect_error(
    nad_catalog_rows(tibble::tibble(
      state = "IN",
      source_county = "Not A County",
      source_row_count = 1
    )),
    "cannot be mapped to one county FIPS",
    fixed = TRUE
  )
})

test_that("packaged NAD revision 23 catalog is complete and valid", {
  catalog_path <- system.file(
    "extdata",
    "v2",
    "nad_catalog",
    "23",
    "counties.parquet",
    package = "addr"
  )
  expect_true(nzchar(catalog_path))

  catalog <- nad_catalog()
  expect_named(catalog, nad_catalog_required_columns())
  expect_equal(nrow(catalog), 2259L)
  expect_equal(sum(catalog$source_row_count), 97928946)
  expect_identical(unique(catalog$nad_revision), 23L)
  expect_true(all(catalog$source_row_count > 0))
  expect_true(all(c(
    "24005",
    "24510",
    "29189",
    "29510",
    "51059",
    "51600",
    "51067",
    "51620",
    "51159",
    "51760",
    "51161",
    "51770"
  ) %in% catalog$county_fips))
  expect_true("nad_catalog" %in% getNamespaceExports("addr"))
})

test_that("nad_read routes through the flat extractor", {
  fields <- nad_source_fields()
  flat <- stats::setNames(rep(list(NA_character_), length(fields)), fields)
  flat$Add_Number <- "10"
  flat$St_Name <- "MAIN"
  flat$County <- "DeWitt"
  flat$Post_City <- "CLINTON"
  flat$State <- "IL"
  flat$Zip_Code <- "61727"
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

  out <- nad_read("17039", refresh_source = "no")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_identical(out$uuid, "fixture-uuid")
  expect_identical(calls[[1L]]$path, "managed-flat-source")
  expect_identical(calls[[1L]]$member, "TXT/NAD_r23.txt")
  expect_identical(calls[[1L]]$state, "IL")
  expect_identical(calls[[1L]]$county, "DeWitt")
  expect_identical(calls[[1L]]$fields, fields)
})

test_that("nad creates processed county data outside stow and reuses it offline", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  storage <- nad_test_storage()
  expected <- nad_storage_to_nad(storage, state = "IN")
  source_workspace <- stow::stow_path(package = "addr", subdir = "nad")
  calls <- 0L
  allow_source <- TRUE
  local_mocked_bindings(
    nad_read_storage = function(county, state, version, refresh_source) {
      if (!allow_source) {
        stop("source was accessed after the county was cached")
      }
      calls <<- calls + 1L
      expect_identical(county, "18137")
      expect_null(state)
      expect_identical(version, 23L)
      expect_identical(refresh_source, "no")
      storage
    },
    .package = "addr"
  )

  expect_message(
    out <- nad("18137", refresh_binary = "yes", refresh_source = "no"),
    "installing from source"
  )
  path <- nad_county_path("18137", "IN")
  expect_identical(
    path,
    file.path(
      tools::R_user_dir("addr", "data"),
      "v2",
      "nad",
      "23",
      "state=IN",
      "county_fips=18137",
      "part-0.parquet"
    )
  )
  expect_true(file.exists(path))
  expect_equal(out, expected)
  expect_equal(nad_read_county_parquet(path), storage)
  manifest_path <- nad_manifest_path()
  expect_identical(
    manifest_path,
    file.path(
      tools::R_user_dir("addr", "data"),
      "v2",
      "nad_manifest",
      "23",
      "counties.parquet"
    )
  )
  expect_true(file.exists(manifest_path))
  manifest <- nad_manifest(validate = TRUE)
  expect_named(manifest, nad_manifest_required_columns())
  expect_equal(nrow(manifest), 1L)
  expect_identical(manifest$county_fips, "18137")
  expect_identical(manifest$state, "IN")
  expect_identical(manifest$county, "Ripley")
  expect_identical(manifest$row_count, 1L)
  expect_identical(manifest$size_bytes, unname(file.info(path)$size))
  expect_identical(manifest$sha256, nad_file_sha256(path))
  expect_identical(manifest$nad_revision, 23L)
  expect_match(
    manifest$installed_at,
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
  )
  expect_length(
    list.files(source_workspace, all.files = TRUE, no.. = TRUE),
    0L
  )
  allow_source <- FALSE
  expect_equal(
    nad("18137", refresh_binary = "no", refresh_source = "no"),
    expected
  )
  expect_identical(calls, 1L)
  expect_length(
    list.files(source_workspace, all.files = TRUE, no.. = TRUE),
    0L
  )
})

test_that("nad uses an existing county Parquet before repairing its manifest", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-nad-data-"))
  storage <- nad_test_storage(n = 2L)
  expected <- nad_storage_to_nad(storage, state = "IN")
  path <- nad_county_path("18137", "IN")
  nad_write_county_parquet(storage, path)
  expect_false(file.exists(nad_manifest_path()))

  local_mocked_bindings(
    nad_read_storage = function(...) stop("the national source was accessed"),
    .package = "addr"
  )

  expect_equal(
    nad("18137", refresh_binary = "no", refresh_source = "no"),
    expected
  )
  manifest <- nad_manifest(validate = TRUE)
  expect_identical(manifest$county_fips, "18137")
  expect_identical(manifest$row_count, 2L)
})

test_that("nad manifest validation detects inventory and file corruption", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-nad-data-"))
  value <- nad_test_storage()
  path <- nad_county_path("18137", "IN")
  nad_write_county_parquet(value, path)
  nad_upsert_manifest(
    path,
    value,
    nad_county_info("18137"),
    version = 23L
  )
  expect_silent(nad_manifest(validate = TRUE))

  extra <- nad_county_path("21037", "KY")
  nad_write_county_parquet(nad_test_storage(county = "Campbell"), extra)
  expect_error(
    nad_manifest(validate = TRUE),
    "does not match the installed Parquet inventory",
    fixed = TRUE
  )
  unlink(extra)

  writeLines("not Parquet", path)
  expect_error(
    nad_manifest(validate = TRUE),
    "size does not match manifest",
    fixed = TRUE
  )
})

test_that("an empty NAD manifest validates against an empty dataset", {
  data_root <- tempfile("empty-nad-data-")
  expect_silent(nad_validate_manifest(
    nad_empty_manifest(),
    data_root = data_root,
    version = 23L,
    verify_files = TRUE
  ))
})

test_that("nad force refresh replaces one manifest row", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-nad-data-"))
  call <- 0L
  local_mocked_bindings(
    nad_read_storage = function(...) {
      call <<- call + 1L
      nad_test_storage(n = call)
    },
    .package = "addr"
  )

  nad("18137", refresh_binary = "yes", refresh_source = "no")
  first <- nad_manifest()
  nad("18137", refresh_binary = "force", refresh_source = "no")
  second <- nad_manifest(validate = TRUE)

  expect_equal(nrow(second), 1L)
  expect_identical(second$county_fips, "18137")
  expect_identical(second$row_count, 2L)
  expect_false(identical(second$sha256, first$sha256))
})

test_that("nad_install accepts exactly one county", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-nad-data-"))
  calls <- character()
  local_mocked_bindings(
    nad_read_storage = function(county, ...) {
      calls <<- c(calls, county)
      nad_test_storage()
    },
    .package = "addr"
  )

  expect_error(
    nad_install(c("18137", "39061")),
    "county must be length one",
    fixed = TRUE
  )
  expect_identical(
    withVisible(nad_install("18137", refresh_source = "no")),
    list(value = "18137", visible = FALSE)
  )
  expect_identical(calls, "18137")
  expect_true(file.exists(nad_county_path("18137", "IN")))
})

test_that("nad_install rejects a zero-row county extraction", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-nad-data-"))
  local_mocked_bindings(
    nad_read_storage = function(...) nad_test_storage(n = 0L),
    .package = "addr"
  )

  expect_error(
    nad_install("18137", refresh_source = "no"),
    paste0(
      "NAD revision 23 extraction returned zero rows for county `18137` ",
      "(Ripley, IN). This is an installation error, not a valid empty ",
      "county"
    ),
    fixed = TRUE
  )
  expect_false(file.exists(nad_county_path("18137", "IN")))
  expect_false(file.exists(nad_manifest_path()))
  expect_equal(nrow(nad_manifest()), 0L)
})

test_that("nad_install rejects counties absent from the packaged catalog", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-nad-data-"))
  source_accessed <- FALSE
  local_mocked_bindings(
    nad_download = function(...) {
      source_accessed <<- TRUE
      stop("the national source was accessed")
    },
    .package = "addr"
  )

  expect_error(
    nad_install("06001", refresh_source = "no"),
    paste0(
      "county `06001` (Alameda, CA) is not available in the packaged ",
      "NAD revision 23 catalog"
    ),
    fixed = TRUE
  )
  expect_false(source_accessed)
})

test_that("nad_dataset opens installed counties as one Hive dataset", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("dplyr")
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-nad-data-"))
  nad_write_county_parquet(
    nad_test_storage(county = "Ripley"),
    nad_county_path("18137", "IN")
  )
  nad_write_county_parquet(
    nad_test_storage(county = "Butler"),
    nad_county_path("39017", "OH")
  )

  dataset <- nad_dataset()
  expect_s3_class(dataset, "FileSystemDataset")
  expect_true(all(c("state", "county_fips") %in% dataset$schema$names))
  butler <- dataset |>
    dplyr::filter(state == "OH", county_fips == "39017") |>
    dplyr::collect()
  expect_equal(nrow(butler), 1L)
  expect_identical(butler$county, "Butler")
  expect_identical(butler$state, "OH")
  expect_identical(butler$county_fips, "39017")
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
  processed_path <- nad_county_path("39061", "OH")
  manifest_path <- nad_manifest_path()

  expect_identical(basename(managed_root), "stow")
  expect_identical(dirname(source_workspace), managed_root)
  expect_identical(
    processed_path,
    file.path(
      data_root,
      "v2",
      "nad",
      "23",
      "state=OH",
      "county_fips=39061",
      "part-0.parquet"
    )
  )
  expect_identical(
    manifest_path,
    file.path(data_root, "v2", "nad_manifest", "23", "counties.parquet")
  )
  expect_false(startsWith(
    processed_path,
    paste0(source_workspace, .Platform$file.sep)
  ))
  expect_false(startsWith(
    manifest_path,
    paste0(source_workspace, .Platform$file.sep)
  ))
  expect_identical(
    taf_dataset_path(year = 2025L, version = "v2"),
    file.path(data_root, "v2", "tiger_addr_feat", "2025")
  )
})

test_that("NAD ignores processed data under the development stow layout", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  development_path <- file.path(
    stow::stow_path(package = "addr", subdir = "nad"),
    "v2",
    "NAD_r23",
    "IN",
    "Ripley.rds"
  )
  dir.create(dirname(development_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(tibble::tibble(source = "development stow layout"), development_path)

  processed_path <- nad_county_path("18137", "IN")
  expect_error(
    nad("18137", refresh_binary = "no", refresh_source = "no"),
    paste0(processed_path, " does not exist"),
    fixed = TRUE
  )
  expect_true(file.exists(development_path))
  expect_false(file.exists(processed_path))
})

test_that("NAD ignores the former processed RDS layout", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  former_path <- file.path(
    tools::R_user_dir("addr", "data"),
    "v2",
    "nad",
    "23",
    "IN",
    "Ripley.rds"
  )
  dir.create(dirname(former_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(tibble::tibble(source = "former RDS layout"), former_path)

  processed_path <- nad_county_path("18137", "IN")
  expect_error(
    nad("18137", refresh_binary = "no", refresh_source = "no"),
    paste0(processed_path, " does not exist"),
    fixed = TRUE
  )
  expect_true(file.exists(former_path))
  expect_false(file.exists(processed_path))
})
