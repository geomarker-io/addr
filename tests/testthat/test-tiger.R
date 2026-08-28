test_that("can download files from tiger", {
  skip_live_tiger_downloads()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  dl_file <- tiger_download(
    "TIGER2024/FEATNAMES/tl_2024_39061_featnames.zip",
    subdir = "tiger_feat_names"
  )
  expect_true(file.exists(dl_file))
})

make_tiger_test_zip <- function(path) {
  source_dir <- tempfile("tiger-zip-source-")
  dir.create(source_dir)
  source_file <- file.path(source_dir, "fixture.txt")
  writeLines("fixture", source_file)
  old_dir <- setwd(source_dir)
  on.exit(setwd(old_dir), add = TRUE)
  utils::zip(path, basename(source_file), flags = "-q")
  invisible(path)
}

taf_test_storage <- function(zip = "45220", county = "39061", n = 1L) {
  tibble::tibble(
    LINEARID = sprintf("fixture-line-%d", seq_len(n)),
    FULLNAME = rep.int("MAIN ST", n),
    side = rep.int("L", n),
    ZIP = rep.int(zip, n),
    FROMHN = rep.int(1L, n),
    TOHN = rep.int(99L, n),
    PARITY = rep.int("B", n),
    OFFSET = rep.int(0, n),
    geometry_wkt = rep.int(
      "LINESTRING (-84.5 39.1, -84.49 39.11)",
      n
    ),
    street_predirectional = rep.int("", n),
    street_premodifier = rep.int("", n),
    street_pretype = rep.int("", n),
    street_name = rep.int("MAIN", n),
    street_posttype = rep.int("ST", n),
    street_postdirectional = rep.int("", n),
    street_tag_parsed = rep.int(FALSE, n),
    county_fips = rep.int(county, n)
  )
}

taf_install_test_file <- function(
  zip = "45220",
  county = "39061",
  n = 1L,
  year = "2025",
  version = "v2"
) {
  storage <- taf_test_storage(zip = zip, county = county, n = n)
  path <- file.path(
    taf_dataset_path(year = year, version = version),
    sprintf("zip3=%s", substr(zip, 1L, 3L)),
    sprintf("zip2=%s", substr(zip, 4L, 5L)),
    sprintf("%s.parquet", county)
  )
  taf_write_county_parquet(
    storage,
    path,
    county = county,
    ZIP = zip
  )
  source <- storage
  source$zip3 <- substr(zip, 1L, 3L)
  source$zip2 <- substr(zip, 4L, 5L)
  taf_county_zip_manifest_rows(
    source,
    county = county,
    year = year,
    version = version
  )
}

test_that("tiger_download fixes ownership and forwards managed-copy controls", {
  tiger_path <- "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  expected_url <- paste0("https://www2.census.gov/geo/tiger/", tiger_path)
  received <- NULL
  withr::local_options(list(addr.tiger_download_interval = 0))
  tiger_download_state$last_request <- NULL

  local_mocked_bindings(
    stow = function(...) {
      args <- list(...)
      if (isTRUE(args$offline)) {
        stop("No managed local copy is available in offline mode")
      }
      received <<- args
      "/managed/canonical.zip"
    },
    .package = "stow"
  )

  dl_file <- tiger_download(
    tiger_path,
    subdir = "tiger_addr_feat",
    overwrite = TRUE,
    offline = FALSE
  )

  expect_identical(dl_file, "/managed/canonical.zip")
  expect_identical(received[[1]], expected_url)
  expect_identical(received$package, "addr")
  expect_identical(received$subdir, "tiger_addr_feat")
  expect_identical(received$overwrite, TRUE)
  expect_identical(received$offline, FALSE)
  expect_identical(received$etag, FALSE)
  expect_identical(received$validate, tiger_validate_zip)
})

test_that("tiger_download returns cached files without request pacing", {
  tiger_path <- "TIGER2025/FEATNAMES/tl_2025_39061_featnames.zip"
  calls <- 0L
  tiger_download_state$last_request <- 123

  local_mocked_bindings(
    stow = function(...) {
      calls <<- calls + 1L
      expect_true(list(...)$offline)
      "/managed/cached.zip"
    },
    .package = "stow"
  )

  expect_identical(
    tiger_download(tiger_path, subdir = "tiger_feat_names"),
    "/managed/cached.zip"
  )
  expect_identical(calls, 1L)
  expect_identical(tiger_download_state$last_request, 123)
})

test_that("tiger_download retries rejected HTML and explains the failure", {
  tiger_path <- "TIGER2025/ADDRFEAT/tl_2025_12003_addrfeat.zip"
  download_attempts <- 0L
  withr::local_options(list(
    addr.tiger_download_attempts = 2L,
    addr.tiger_download_interval = 0,
    addr.tiger_download_retry_base = 0,
    addr.tiger_download_retry_jitter = 0
  ))
  tiger_download_state$last_request <- NULL

  local_mocked_bindings(
    stow = function(...) {
      args <- list(...)
      if (isTRUE(args$offline)) {
        stop("No managed local copy is available in offline mode")
      }
      download_attempts <<- download_attempts + 1L
      rejected <- tempfile()
      writeLines(
        "<html>Request Rejected. Support ID: ABC-123</html>",
        rejected
      )
      expect_false(args$validate(rejected))
      stop("Downloaded content failed validation and was not committed.")
    },
    .package = "stow"
  )

  expect_error(
    tiger_download(tiger_path, subdir = "tiger_addr_feat"),
    "server rejection, not an empty or valid TIGER archive",
    fixed = TRUE
  )
  expect_identical(download_attempts, 2L)
  expect_identical(tiger_download_state$rejection$support_id, "ABC-123")
})

test_that("tiger_download does not retry a missing TIGER file", {
  tiger_path <- "TIGER2025/ADDRFEAT/tl_2025_99999_addrfeat.zip"
  download_attempts <- 0L
  withr::local_options(list(
    addr.tiger_download_attempts = 4L,
    addr.tiger_download_interval = 0
  ))
  tiger_download_state$last_request <- NULL

  local_mocked_bindings(
    stow = function(...) {
      args <- list(...)
      if (isTRUE(args$offline)) {
        stop("No managed local copy is available in offline mode")
      }
      download_attempts <<- download_attempts + 1L
      stop("Download failed. Original error: HTTP response code said error [404]")
    },
    .package = "stow"
  )

  expect_error(
    tiger_download(tiger_path, subdir = "tiger_addr_feat"),
    "failed after 1 attempt",
    fixed = TRUE
  )
  expect_identical(download_attempts, 1L)
})

test_that("tiger_download_url supports only explicit HTTPS or FTP", {
  tiger_path <- "TIGER2025/FEATNAMES/tl_2025_39061_featnames.zip"

  expect_identical(
    tiger_download_url(tiger_path),
    paste0("https://www2.census.gov/geo/tiger/", tiger_path)
  )
  withr::local_options(list(addr.tiger_download_protocol = "ftp"))
  expect_identical(
    tiger_download_url(tiger_path),
    paste0("ftp://ftp2.census.gov/geo/tiger/", tiger_path)
  )
  options(addr.tiger_download_protocol = "file")
  expect_error(
    tiger_download_url(tiger_path),
    'must be either "https" or "ftp"',
    fixed = TRUE
  )
})

test_that("public TIGER consumers own separate stow subdirectories", {
  received <- list()
  local_mocked_bindings(
    tiger_download = function(...) {
      received[[length(received) + 1L]] <<- list(...)
      "/managed/canonical.zip"
    }
  )

  expect_identical(
    tiger_feat_names_download("39061", "2025", TRUE),
    "/managed/canonical.zip"
  )
  expect_identical(
    tiger_addr_feat_download("39061", "2025", FALSE),
    "/managed/canonical.zip"
  )

  expect_identical(received[[1]]$subdir, "tiger_feat_names")
  expect_identical(received[[1]]$overwrite, TRUE)
  expect_identical(received[[2]]$subdir, "tiger_addr_feat")
  expect_identical(received[[2]]$overwrite, FALSE)
})

test_that("tiger ZIP validator accepts ZIPs and rejects other content", {
  zip_file <- tempfile(fileext = ".zip")
  make_tiger_test_zip(zip_file)
  invalid_file <- tempfile()
  writeLines("not a ZIP", invalid_file)

  expect_identical(tiger_validate_zip(zip_file), TRUE)
  expect_identical(tiger_validate_zip(invalid_file), FALSE)
})

test_that("tiger ZIP validator extracts Census rejection support IDs", {
  invalid_file <- tempfile()
  writeLines(
    "<html>The requested URL was rejected. Support ID is: 987654321</html>",
    invalid_file
  )

  expect_false(tiger_validate_zip(invalid_file))
  expect_identical(
    tiger_download_state$rejection$support_id,
    "987654321"
  )
})

test_that("tiger_download supports offline managed local copies", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  tiger_path <- "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  url <- tiger_download_url(tiger_path)
  managed_root <- stow::stow_path(package = "addr")
  managed_dir <- stow::stow_path(
    package = "addr",
    subdir = "tiger_feat_names"
  )
  expect_identical(basename(managed_root), "stow")
  expect_identical(dirname(managed_dir), managed_root)
  stow_filename <- getFromNamespace(".stow_url_to_filename", "stow")
  managed_copy <- file.path(managed_dir, stow_filename(url))
  make_tiger_test_zip(managed_copy)

  dl_file <- tiger_download(
    tiger_path,
    subdir = "tiger_feat_names",
    offline = TRUE
  )

  expect_identical(dl_file, normalizePath(managed_copy, winslash = "/"))
  expect_error(
    tiger_download(
      sub("internationalboundary", "coastline", tiger_path),
      subdir = "tiger_feat_names",
      offline = TRUE
    ),
    "No managed local copy is available in offline mode",
    fixed = TRUE
  )
})

test_that("tiger_download ignores the former unmanaged TIGER layout", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  tiger_path <- "TIGER2024/FEATNAMES/tl_2024_39061_featnames.zip"
  legacy_root <- dirname(stow::stow_path(package = "addr"))
  legacy <- file.path(legacy_root, tiger_path)
  dir.create(dirname(legacy), recursive = TRUE, showWarnings = FALSE)
  make_tiger_test_zip(legacy)

  expect_error(
    tiger_download(
      tiger_path,
      subdir = "tiger_feat_names",
      offline = TRUE
    ),
    "No managed local copy is available in offline mode",
    fixed = TRUE
  )
  expect_true(file.exists(legacy))
})

test_that("tiger_download does not discover stow 0.2 managed copies", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-stow-data-"))
  tiger_path <- "TIGER2024/FEATNAMES/tl_2024_39061_featnames.zip"
  url <- tiger_download_url(tiger_path)
  managed_root <- stow::stow_path(package = "addr")
  old_managed_dir <- file.path(dirname(managed_root), "tiger_feat_names")
  dir.create(old_managed_dir, recursive = TRUE, showWarnings = FALSE)
  stow_filename <- getFromNamespace(".stow_url_to_filename", "stow")
  old_copy <- file.path(old_managed_dir, stow_filename(url))
  make_tiger_test_zip(old_copy)

  expect_error(
    tiger_download(
      tiger_path,
      subdir = "tiger_feat_names",
      offline = TRUE
    ),
    "No managed local copy is available in offline mode",
    fixed = TRUE
  )
  expect_true(file.exists(old_copy))
})

test_that("tiger_addr_feat() can download addr feat from tiger", {
  skip_live_tiger_downloads()
  skip_on_ci()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})

test_that("taf_dataset requires arrow for the dataset interface", {
  local_mocked_bindings(
    check_installed = function(pkg, reason = NULL) {
      stop(paste(pkg, reason), call. = FALSE)
    }
  )

  expect_error(taf_dataset("2025"), "arrow.*multi-file taf dataset")
})

test_that("TAF install locks are reentrant within one R process", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-taf-lock-"))
  calls <- 0L

  taf_with_install_lock("2025", "v2", {
    taf_with_install_lock("2025", "v2", {
      calls <- calls + 1L
    })
  })

  expect_equal(calls, 1L)
  expect_false(dir.exists(taf_install_lock_dir("2025", "v2")))
})

test_that("taf_install writes and repairs a validated county manifest", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-taf-install-"))
  year <- "2025"
  version <- "v2"
  county <- "39061"
  linear_id <- c("fixture-45220", "fixture-45229")
  feature_names <- tibble::tibble(
    LINEARID = linear_id,
    addr_street = addr_street(
      name = c("MAIN", "BURNET"),
      posttype = "ST"
    )
  )
  address_features <- tibble::tibble(
    LINEARID = linear_id,
    FULLNAME = c("MAIN ST", "BURNET ST"),
    side = "L",
    ZIP = c("45220", "45229"),
    FROMHN = 1L,
    TOHN = 999L,
    PARITY = "B",
    OFFSET = 0,
    s2_geography = s2::as_s2_geography(c(
      "LINESTRING (-84.5 39.1, -84.49 39.11)",
      "LINESTRING (-84.6 39.2, -84.59 39.21)"
    ))
  )
  source_calls <- 0L
  local_mocked_bindings(
    tiger_feat_names = function(...) {
      source_calls <<- source_calls + 1L
      feature_names
    },
    tiger_addr_feat = function(...) {
      source_calls <<- source_calls + 1L
      address_features
    }
  )

  expect_identical(
    taf_install(county, year = year, version = version),
    county
  )
  manifest <- taf_manifest(year = year, version = version, validate = TRUE)
  expect_equal(nrow(manifest), 2L)
  expect_setequal(manifest$ZIP, c("45220", "45229"))
  expect_equal(source_calls, 2L)

  taf_install(county, year = year, version = version)
  expect_equal(source_calls, 2L)

  missing_path <- taf_manifest_file_paths(
    manifest[manifest$ZIP == "45220", , drop = FALSE],
    data_root = taf_dataset_path(year = year, version = version)
  )
  unlink(missing_path, force = TRUE)
  taf_install(county, year = year, version = version)
  expect_equal(source_calls, 4L)
  expect_silent(taf_manifest(year = year, version = version, validate = TRUE))
})

test_that("taf reads installed Parquet files by ZIP code", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-taf-data-"))
  year <- "2025"
  version <- "v2"
  manifest <- taf_install_test_file(year = year, version = version)
  taf_write_county_zip_manifest(
    manifest,
    year = year,
    version = version
  )

  out <- taf(c("45220", "45220"), year = year, version = version)
  expect_equal(nrow(out), 1L)
  expect_identical(out$county_fips, "39061")
  expect_identical(format(out$addr_street), "MAIN ST")
  expect_s3_class(out$s2_geography, "s2_geography")
})

test_that("TAF public reader names are hard renamed", {
  exports <- getNamespaceExports("addr")
  expect_true(all(c("taf", "taf_dataset", "taf_manifest") %in% exports))
  expect_false("taf_zip" %in% exports)
  expect_true(all(vapply(
    list(
      taf,
      taf_dataset,
      taf_catalog,
      taf_manifest,
      taf_needed_counties,
      taf_ensure,
      taf_install
    ),
    function(fun) identical(eval(formals(fun)$version), "v2"),
    logical(1)
  )))
})

test_that("taf_manifest inventories and validates installed county ZIP files", {
  withr::local_envvar(R_USER_DATA_DIR = tempfile("addr-taf-manifest-"))
  year <- "2025"
  version <- "v2"
  rows <- vctrs::vec_rbind(
    taf_install_test_file(
      zip = "45220",
      county = "39061",
      n = 2L,
      year = year,
      version = version
    ),
    taf_install_test_file(
      zip = "45249",
      county = "39017",
      n = 3L,
      year = year,
      version = version
    )
  )
  taf_write_county_zip_manifest(rows, year = year, version = version)

  manifest <- taf_manifest(year = year, version = version, validate = TRUE)
  expect_named(manifest, taf_manifest_required_columns())
  expect_equal(manifest$n_ranges, c(2L, 3L))
  expect_true(all(manifest$size_bytes > 0))
  expect_true(all(grepl("^[0-9a-f]{64}$", manifest$sha256)))

  manifest$sha256[[1L]] <- paste(rep.int("0", 64L), collapse = "")
  taf_write_county_zip_manifest(manifest, year = year, version = version)
  expect_error(
    taf_manifest(year = year, version = version, validate = TRUE),
    "SHA-256 mismatch"
  )
})

test_that("TAF bake and installer carry the schema-v2 manifest contract", {
  bake <- system.file("bake_taf.R", package = "addr")
  installer <- system.file(
    "exec",
    "install-addr-taf-fuel.sh",
    package = "addr"
  )
  expect_true(nzchar(bake))
  expect_true(nzchar(installer))
  expect_no_error(parse(bake))
  bake_text <- readLines(bake, warn = FALSE)
  installer_text <- readLines(installer, warn = FALSE)
  expect_true(any(grepl("schema_version = 2L", bake_text, fixed = TRUE)))
  expect_true(any(grepl('taf_version <- "v2"', bake_text, fixed = TRUE)))
  expect_true(any(grepl("manifest_row_count", bake_text, fixed = TRUE)))
  expect_true(any(grepl("taf_validate_manifest", bake_text, fixed = TRUE)))
  expect_true(any(grepl("pack_taf_fuel", bake_text, fixed = TRUE)))
  expect_true(any(grepl(
    'SCHEMA_VERSION" = "2"',
    installer_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    'TAF_VERSION" = "v2"',
    installer_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "taf_validate_manifest",
    installer_text,
    fixed = TRUE
  )))
  expect_true(any(grepl("taf_manifest", installer_text, fixed = TRUE)))
})

test_that("TAF bake packer writes a validated archive and sidecar", {
  skip_if(!nzchar(Sys.which("zstd")))
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  year <- "2025"
  version <- "v2"
  manifest <- taf_install_test_file(year = year, version = version)
  taf_write_county_zip_manifest(manifest, year = year, version = version)

  bake <- parse(system.file("bake_taf.R", package = "addr"))
  is_packer <- vapply(bake, function(expression) {
    is.call(expression) &&
      identical(expression[[1L]], quote(`<-`)) &&
      identical(expression[[2L]], quote(pack_taf_fuel))
  }, logical(1))
  pack_environment <- new.env(parent = asNamespace("addr"))
  eval(bake[[which(is_packer)[[1L]]]], envir = pack_environment)

  out_dir <- tempfile()
  assets <- pack_environment$pack_taf_fuel(
    year = year,
    version = version,
    out_dir = out_dir,
    package_version = "2.0.0",
    data_root = tools::R_user_dir("addr", "data")
  )
  expect_true(all(file.exists(assets)))
  expect_gt(unname(file.info(assets[["archive"]])$size), 0)
  sidecar <- paste(
    readLines(assets[["metadata"]], warn = FALSE),
    collapse = "\n"
  )
  expect_match(sidecar, '"schema_version": 2', fixed = TRUE)
  expect_match(
    sidecar,
    '"addr_package_version_required": "2.0.0"',
    fixed = TRUE
  )
  expect_match(
    sidecar,
    digest::digest(
      algo = "sha256",
      serialize = FALSE,
      file = assets[["archive"]]
    ),
    fixed = TRUE
  )

  tar_file <- tempfile(fileext = ".tar")
  expect_identical(
    system2(
      Sys.which("zstd"),
      c("-dc", shQuote(assets[["archive"]])),
      stdout = tar_file
    ),
    0L
  )
  archive_members <- utils::untar(tar_file, list = TRUE)
  expect_true(any(grepl(
    "v2/tiger_addr_feat/2025/.*/39061[.]parquet$",
    archive_members
  )))
  expect_true(
    "v2/tiger_addr_feat_manifest/2025/county_zip.parquet" %in%
      archive_members
  )
  expect_equal(sum(grepl("[.]parquet$", archive_members)), 2L)
})

test_that("taf_catalog reads installed ZIP county catalog", {
  catalog_root <- tempfile()
  withr::local_options(list(
    addr.taf_catalog_dir = file.path(catalog_root, "inst", "extdata")
  ))

  manifest <- tibble::tibble(
    county_fips = c("39061", "39017"),
    ZIP = c("45220", "45249"),
    zip3 = c("452", "452"),
    zip2 = c("20", "49"),
    n_ranges = c(10L, 4L),
    installed_at = c("2026-01-01 UTC", "2026-01-01 UTC")
  )
  taf_write_catalog(
    manifest,
    year = "2025",
    version = "v2",
    root = catalog_root
  )

  expect_equal(
    taf_catalog(year = "2025", version = "v2"),
    tibble::tibble(
      county_fips = c("39061", "39017"),
      ZIP = c("45220", "45249"),
      zip3 = c("452", "452"),
      zip2 = c("20", "49"),
      n_ranges = c(10L, 4L)
    )
  )
})

test_that("taf_needed_counties uses catalog and selected ZIP variants", {
  catalog_root <- tempfile()
  withr::local_options(list(
    addr.taf_catalog_dir = file.path(catalog_root, "inst", "extdata")
  ))

  taf_write_catalog(
    tibble::tibble(
      county_fips = c("39061", "39061", "21117", "39017"),
      ZIP = c("45220", "45219", "42520", "45221"),
      zip3 = c("452", "452", "425", "452"),
      zip2 = c("20", "19", "20", "21"),
      n_ranges = c(10L, 3L, 2L, 4L),
      installed_at = rep("2026-01-01 UTC", 4)
    ),
    year = "2025",
    version = "v2",
    root = catalog_root
  )

  needed <- taf_needed_counties(
    "45220",
    year = "2025",
    version = "v2",
    zip_variant = c("minus1", "swap")
  )

  expect_equal(
    needed[c("county_fips", "ZIP", "source_zip", "source_zip_variant")],
    tibble::tibble(
      county_fips = c("39061", "39061", "21117"),
      ZIP = c("45220", "45219", "42520"),
      source_zip = c("45220", "45220", "45220"),
      source_zip_variant = c("exact", "minus1", "swap")
    )
  )
})

test_that("taf_needed_counties includes geographic and typographical variants", {
  catalog_root <- tempfile()
  withr::local_options(list(
    addr.taf_catalog_dir = file.path(catalog_root, "inst", "extdata")
  ))

  taf_write_catalog(
    tibble::tibble(
      county_fips = c("39061", "39017", "21117"),
      ZIP = c("45220", "45226", "45219"),
      zip3 = c("452", "452", "452"),
      zip2 = c("20", "26", "19"),
      n_ranges = c(10L, 4L, 3L)
    ),
    year = "2025",
    version = "v2",
    root = catalog_root
  )

  needed <- taf_needed_counties(
    as_addr("10 MAIN ST ANDERSON OH 45220"),
    year = "2025",
    version = "v2",
    zip_variant = "minus1",
    place_zip_variant = "county-sub"
  )

  expect_equal(
    needed[c("county_fips", "ZIP", "source_zip_variant")],
    tibble::tibble(
      county_fips = c("39061", "39017", "21117"),
      ZIP = c("45220", "45226", "45219"),
      source_zip_variant = c("exact", "county-sub", "minus1")
    )
  )
})

test_that("taf_ensure installs only missing needed counties", {
  catalog_root <- tempfile()
  withr::local_options(list(
    addr.taf_catalog_dir = file.path(catalog_root, "inst", "extdata")
  ))
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))

  taf_write_catalog(
    tibble::tibble(
      county_fips = c("39061", "39017"),
      ZIP = c("45220", "45220"),
      zip3 = c("452", "452"),
      zip2 = c("20", "20"),
      n_ranges = c(10L, 4L),
      installed_at = c("2026-01-01 UTC", "2026-01-01 UTC")
    ),
    year = "2025",
    version = "v2",
    root = catalog_root
  )
  manifest <- taf_install_test_file(n = 10L)
  taf_write_county_zip_manifest(manifest, year = "2025", version = "v2")

  installed <- character()
  local_mocked_bindings(
    taf_install = function(county, ...) {
      installed <<- c(installed, county)
      invisible(county)
    }
  )

  missing <- taf_ensure(
    "45220",
    year = "2025",
    version = "v2",
    zip_variants = FALSE
  )

  expect_equal(installed, "39017")
  expect_equal(missing$county_fips, "39017")
})

test_that("tiger_addr_feat() works with existing user data dir", {
  skip_live_tiger_downloads()
  skip_on_ci()
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})
