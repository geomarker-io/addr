#' Read National Address Database (NAD) tables into R
#'
#' @description
#' The U.S. Department of Transportation partners with address programs from
#' state, local, and tribal governments to compile their authoritative data
#' into a database. Find more information in the source data portal:
#' <https://data.transportation.gov/d/fc2s-wawr>
#'
#' `nad_read()` reads NAD source data by county,
#' using source data already downloaded with `nad_download()` or downloading
#' it when `refresh_source = "yes"`, and readies it for R.
#' Counties can be identified either by county name plus state, or by a
#' 5-digit county FIPS identifier. County names and state abbreviations are
#' resolved internally and determine the processed-data path and source filter.
#' The revision 23 source is a roughly 7.6 GB compressed archive containing a
#' roughly 41 GB comma-delimited text member.
#'
#' Processed county Parquet files are created one county at a time with
#' `nad_install()` or on first use with `nad()`.
#' The compressed national source is managed exclusively by `stow()` beneath
#' `stow::stow_path(package = "addr", subdir = "nad")`. Derived county Parquet
#' files are a separate Hive-partitioned dataset beneath
#' `file.path(tools::R_user_dir("addr", "data"), "v1", "nad", "23")`, organized
#' by `state` and `county_fips`. `nad_manifest()` inventories those county files
#' from `v1/nad_manifest/23/counties.parquet`. Set `R_USER_DATA_DIR` to
#' relocate both areas while retaining their source-versus-processed-data
#' separation.
#' @param county character, length one; county name or 5-digit county FIPS
#'   identifier
#' @param state character, length one; name or abbreviation of state. Required
#'   when `county` is a county name; ignored when `county` is a 5-digit county
#'   FIPS identifier
#' @param version integer, length one; NAD revision to use. Only revision 23 is
#'   supported.
#' @param refresh_binary character, length one; choose how to refresh a
#' processed NAD county Parquet file if not already present; `"yes"` creates a
#' missing file, `"no"` requires it to exist, and `"force"` recreates it
#' @param overwrite logical, length one; overwrite an existing processed county
#'   Parquet file?
#' @param refresh_source character, length one; choose how to refresh the
#'   compressed national source; `"no"` requires the stow-managed source to
#'   exist, `"yes"` downloads it if missing, and `"force"` redownloads it
#' @returns `nad()` and `nad_read()` return a tibble for one county.
#'   `nad_install()` invisibly returns the installed county FIPS identifier.
#'
#' @details
#' The revision 23 comma-delimited flat source archive is downloaded from the
#' transportation.gov data portal:
#' <https://data.transportation.gov/d/fc2s-wawr>.
#' `nad_download(version = 23L)` installs the pinned compressed archive as a
#' durable managed local copy using `stow::stow()`. County installation streams
#' the nationwide text member directly from that archive, retains the requested
#' county, and writes a separate processed Parquet file.
#' The roughly 41 GB text member is never unpacked on disk.
#' Before downloading, review the source metadata and disclaimer in the data
#' portal.
#'
#' Investigate individual address points in the online viewer:
#' <https://usdot.maps.arcgis.com/apps/instant/portfolio/index.html?appid=59f7e4fb71994d13b61f424e21a6cffe>
#'
#' The NAD does not distinguish between empty and missing address components.
#' When reading into R, all missing address components are replaced with an
#' empty string (`""`) *except* for address number (digits), street name,
#' and ZIP code.
#' Addresses with malformed ZIP codes are removed.
#'
#' @export
#' @examples
#' # explicitly download source data, then create county output on first read
#' \dontrun{
#'   # install the compressed revision 23 flat source archive, then build a
#'   # county Parquet file from it
#'   nad_download()
#'   nad_install("39017")
#'   nad("Butler", "OH")
#'   nad("39017")
#' }
#'
#' # small packaged revision 23 fixture derived from Hamilton County, OH
#' nad_example_data()
nad <- function(
  county,
  state = NULL,
  version = 23L,
  refresh_binary = c("yes", "no", "force"),
  refresh_source = c("no", "yes", "force")
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state),
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version),
    "refresh_binary must be a character vector" = is.character(refresh_binary),
    "refresh_binary must not contain missing values" = !any(is.na(
      refresh_binary
    )),
    "refresh_source must be a character vector" = is.character(refresh_source),
    "refresh_source must not contain missing values" = !any(is.na(
      refresh_source
    ))
  )
  refresh_binary <- match.arg(refresh_binary)
  refresh_source <- match.arg(refresh_source)
  county_info <- nad_county_info(county, state)
  nad_version_metadata(version)
  nad_path <- nad_county_path(
    county_fips = county_info$county_fips,
    state = county_info$state,
    version = version
  )
  if (!file.exists(nad_path) || refresh_binary == "force") {
    if (refresh_binary == "no") {
      stop(
        nad_path,
        " does not exist; set `refresh_binary = 'yes'`",
        " to install from source NAD"
      )
    }
    nad_install(
      county = county,
      state = state,
      version = version,
      overwrite = identical(refresh_binary, "force"),
      refresh_source = refresh_source
    )
  }
  storage <- nad_read_county_parquet(nad_path)
  nad_upsert_manifest(
    path = nad_path,
    data = storage,
    county_info = county_info,
    version = version
  )
  nad_storage_to_nad(storage, state = county_info$state)
}

#' @rdname nad
#' @export
nad_install <- function(
  county,
  state = NULL,
  version = 23L,
  overwrite = FALSE,
  refresh_source = c("no", "yes", "force")
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state),
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version),
    "overwrite must be logical" = is.logical(overwrite),
    "overwrite must be length one" = length(overwrite) == 1L,
    "overwrite must not be missing" = !is.na(overwrite),
    "refresh_source must be a character vector" = is.character(refresh_source),
    "refresh_source must not contain missing values" = !any(is.na(
      refresh_source
    ))
  )
  refresh_source <- match.arg(refresh_source)
  county_info <- nad_county_info(county, state)
  nad_path <- nad_county_path(
    county_fips = county_info$county_fips,
    state = county_info$state,
    version = version
  )
  if (file.exists(nad_path) && !overwrite) {
    storage <- nad_read_county_parquet(nad_path)
    nad_upsert_manifest(
      path = nad_path,
      data = storage,
      county_info = county_info,
      version = version
    )
    return(invisible(county_info$county_fips))
  }
  if (overwrite) {
    message("forcing county Parquet install from source...")
  } else {
    message(nad_path, " does not exist; installing from source...")
  }
  storage <- nad_read_storage(
    county = county,
    state = state,
    version = version,
    refresh_source = refresh_source
  )
  nad_write_county_parquet(storage, nad_path)
  nad_upsert_manifest(
    path = nad_path,
    data = storage,
    county_info = county_info,
    version = version
  )
  invisible(county_info$county_fips)
}

nad_version_metadata <- function(version = 23L) {
  stopifnot(
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )

  if (version != 23L) {
    stop(
      "NAD version `",
      version,
      "` is not supported; supported version: 23",
      call. = FALSE
    )
  }

  list(
    source_size = 7601412707,
    source_members = c(
      "TXT/NAD_r23.txt",
      "TXT/NationalAddressDatabaseMetadata.xml"
    ),
    dlurl = paste0(
      "https://data.transportation.gov/api/views/fc2s-wawr/files/",
      "b189f78b-2262-44e8-b3b6-5c4094c12da5"
    )
  )
}

nad_county_info <- function(county, state = NULL) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state)
  )

  if (!grepl("^[0-9]{5}$", county) && is.null(state)) {
    stop(
      "state must be supplied when county is not a 5-digit FIPS identifier",
      call. = FALSE
    )
  }

  county_info <- county_fips_lookup(county, state)
  list(
    county_fips = county_info$county_fips[[1]],
    county = county_info$county[[1]],
    state = county_info$state[[1]]
  )
}

nad_county_path <- function(county_fips, state, version = 23L) {
  stopifnot(
    "county_fips must be a character vector" = is.character(county_fips),
    "county_fips must be length one" = length(county_fips) == 1L,
    "county_fips must not be missing" = !is.na(county_fips),
    "county_fips must be a 5-digit FIPS identifier" =
      grepl("^[0-9]{5}$", county_fips),
    "state must be a character vector" = is.character(state),
    "state must be length one" = length(state) == 1L,
    "state must not be missing" = !is.na(state)
  )
  nad_version_metadata(version)
  file.path(
    nad_data_path(version = version),
    nad_county_relative_path(county_fips = county_fips, state = state)
  )
}

nad_county_relative_path <- function(county_fips, state) {
  file.path(
    sprintf("state=%s", state),
    sprintf("county_fips=%s", county_fips),
    "part-0.parquet"
  )
}

#' Open installed National Address Database counties as an Arrow dataset
#'
#' `nad_dataset()` opens every installed county Parquet file as one lazy Arrow
#' dataset. The storage schema contains primitive columns suitable for
#' filtering, projection, and collection with dplyr. `state` and `county_fips`
#' are Hive partition columns. Use `nad()` when reconstructed `addr` and `s2`
#' columns are needed for one county.
#'
#' @inheritParams nad
#' @returns An Arrow `FileSystemDataset`.
#' @export
#' @examples
#' \dontrun{
#' nad_dataset() |>
#'   dplyr::filter(state == "OH", county_fips == "39017") |>
#'   dplyr::select(uuid, address_number, street_name, longitude, latitude) |>
#'   dplyr::collect()
#' }
nad_dataset <- function(version = 23L) {
  check_installed("arrow", "to open the multi-file NAD dataset")
  data_path <- nad_data_path(version = version)
  county_files <- if (dir.exists(data_path)) {
    list.files(
      data_path,
      pattern = "[.]parquet$",
      recursive = TRUE,
      full.names = TRUE
    )
  } else {
    character()
  }
  if (length(county_files) == 0L) {
    stop(
      "no installed NAD county Parquet files were found under `",
      data_path,
      "`",
      call. = FALSE
    )
  }
  arrow::open_dataset(
    data_path,
    format = "parquet",
    partitioning = arrow::hive_partition(
      state = arrow::string(),
      county_fips = arrow::string()
    )
  )
}

#' Inventory installed National Address Database counties
#'
#' `nad_manifest()` reads the local manifest written as county Parquet files are
#' installed. The Parquet path remains the primary existence check used by
#' `nad()`; the manifest provides a compact inventory and integrity metadata for
#' validation and portable NAD fuel bundles.
#'
#' @param version integer, length one; NAD revision to inventory. Only revision
#'   23 is supported.
#' @param validate logical, length one; validate every manifest row against its
#'   county Parquet file, including its schema, row count, byte size, and SHA-256
#'   digest? This can be slow for a large inventory.
#' @returns A tibble with one row per installed county and columns
#'   `county_fips`, `state`, `county`, `row_count`, `size_bytes`, `sha256`,
#'   `nad_revision`, and `installed_at`.
#' @export
#' @examples
#' nad_manifest()
nad_manifest <- function(version = 23L, validate = FALSE) {
  nad_version_metadata(version)
  stopifnot(
    "validate must be logical" = is.logical(validate),
    "validate must be length one" = length(validate) == 1L,
    "validate must not be missing" = !is.na(validate)
  )
  manifest <- nad_read_manifest(version = version)
  if (validate) {
    nad_validate_manifest(
      manifest,
      data_root = nad_data_path(version = version),
      version = version,
      verify_files = TRUE
    )
  }
  manifest
}

nad_data_path <- function(version = 23L) {
  nad_version_metadata(version)
  file.path(
    tools::R_user_dir("addr", "data"),
    "v1",
    "nad",
    as.character(version)
  )
}

nad_manifest_path <- function(version = 23L) {
  nad_version_metadata(version)
  file.path(
    tools::R_user_dir("addr", "data"),
    "v1",
    "nad_manifest",
    as.character(version),
    "counties.parquet"
  )
}

nad_empty_manifest <- function() {
  tibble::tibble(
    county_fips = character(),
    state = character(),
    county = character(),
    row_count = integer(),
    size_bytes = numeric(),
    sha256 = character(),
    nad_revision = integer(),
    installed_at = character()
  )
}

nad_read_manifest <- function(version = 23L) {
  manifest_path <- nad_manifest_path(version = version)
  if (!file.exists(manifest_path)) {
    return(nad_empty_manifest())
  }
  manifest <- nanoparquet::read_parquet(manifest_path) |>
    tibble::as_tibble()
  nad_assert_manifest_schema(manifest, version = version)
  manifest
}

nad_write_manifest <- function(x, version = 23L) {
  nad_assert_manifest_schema(x, version = version)
  if (nrow(x) > 0L) {
    x <- x[order(x$state, x$county, x$county_fips), , drop = FALSE]
    row.names(x) <- NULL
  }
  manifest_path <- nad_manifest_path(version = version)
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(
    pattern = ".counties-",
    tmpdir = dirname(manifest_path),
    fileext = ".parquet"
  )
  on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
  nanoparquet::write_parquet(x, tmp_path)
  written <- nanoparquet::read_parquet(tmp_path) |>
    tibble::as_tibble()
  nad_assert_manifest_schema(written, version = version)
  nad_atomic_replace(tmp_path, manifest_path)
  invisible(manifest_path)
}

nad_write_county_parquet <- function(x, path) {
  nad_assert_storage_schema(x)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(path)), "-"),
    tmpdir = dirname(path),
    fileext = ".parquet"
  )
  on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
  nanoparquet::write_parquet(x, tmp_path, compression = "snappy")
  written <- tryCatch(
    nad_read_county_parquet(tmp_path),
    error = identity
  )
  if (inherits(written, "error") || nrow(written) != nrow(x)) {
    stop(
      "temporary NAD county Parquet file failed validation: ",
      tmp_path,
      call. = FALSE
    )
  }
  nad_atomic_replace(tmp_path, path)
  invisible(path)
}

nad_read_county_parquet <- function(path) {
  value <- nanoparquet::read_parquet(path) |>
    tibble::as_tibble()
  nad_assert_storage_schema(value)
  value
}

nad_storage_required_columns <- function() {
  c(
    "address_number_prefix",
    "address_number",
    "address_number_suffix",
    "street_predirectional",
    "street_premodifier",
    "street_pretype",
    "street_name",
    "street_posttype",
    "street_postdirectional",
    "subaddress",
    "county",
    "place_name",
    "zipcode",
    "uuid",
    "date_update",
    "latitude",
    "longitude",
    "national_grid",
    "placement",
    "address_class",
    "address_type",
    "parcel_id"
  )
}

nad_assert_storage_schema <- function(x) {
  required <- nad_storage_required_columns()
  if (!is.data.frame(x) || !identical(names(x), required)) {
    stop(
      "NAD county Parquet data must contain exactly: ",
      paste(required, collapse = ", "),
      call. = FALSE
    )
  }
  character_columns <- setdiff(
    required,
    c("date_update", "latitude", "longitude")
  )
  if (
    !all(vapply(x[character_columns], is.character, logical(1))) ||
      !inherits(x$date_update, "Date") ||
      !is.numeric(x$latitude) ||
      !is.numeric(x$longitude)
  ) {
    stop("NAD county Parquet data has invalid column types", call. = FALSE)
  }
  invisible(TRUE)
}

nad_atomic_replace <- function(tmp_path, path) {
  if (isTRUE(suppressWarnings(file.rename(tmp_path, path)))) {
    return(invisible(path))
  }
  if (!file.exists(path)) {
    stop("could not atomically install file: ", path, call. = FALSE)
  }

  backup_path <- tempfile(
    pattern = paste0(".", basename(path), "-backup-"),
    tmpdir = dirname(path)
  )
  if (!isTRUE(suppressWarnings(file.rename(path, backup_path)))) {
    stop(
      "could not prepare existing file for replacement: ",
      path,
      call. = FALSE
    )
  }
  replaced <- FALSE
  on.exit(
    {
      if (!replaced && file.exists(backup_path) && !file.exists(path)) {
        suppressWarnings(file.rename(backup_path, path))
      }
    },
    add = TRUE
  )
  if (!isTRUE(suppressWarnings(file.rename(tmp_path, path)))) {
    stop("could not atomically replace file: ", path, call. = FALSE)
  }
  replaced <- TRUE
  unlink(backup_path, force = TRUE)
  invisible(path)
}

nad_upsert_manifest <- function(path, data, county_info, version = 23L) {
  nad_with_manifest_lock(version, {
    manifest <- nad_read_manifest(version = version)
    file_size <- unname(file.info(path)$size)
    existing <- manifest[
      manifest$county_fips == county_info$county_fips,
      ,
      drop = FALSE
    ]
    current <- nrow(existing) == 1L &&
      identical(existing$state[[1L]], county_info$state) &&
      identical(existing$county[[1L]], county_info$county) &&
      identical(as.integer(existing$row_count[[1L]]), as.integer(nrow(data))) &&
      identical(as.numeric(existing$size_bytes[[1L]]), as.numeric(file_size)) &&
      identical(as.integer(existing$nad_revision[[1L]]), version)
    if (!current) {
      manifest <- manifest[
        manifest$county_fips != county_info$county_fips,
        ,
        drop = FALSE
      ]
      row <- tibble::tibble(
        county_fips = county_info$county_fips,
        state = county_info$state,
        county = county_info$county,
        row_count = as.integer(nrow(data)),
        size_bytes = as.numeric(file_size),
        sha256 = nad_file_sha256(path),
        nad_revision = version,
        installed_at = format(
          Sys.time(),
          tz = "UTC",
          format = "%Y-%m-%dT%H:%M:%SZ"
        )
      )
      nad_write_manifest(
        vctrs::vec_rbind(manifest, row),
        version = version
      )
    }
    invisible(path)
  })
  invisible(path)
}

nad_file_sha256 <- function(path) {
  digest::digest(
    algo = "sha256",
    serialize = FALSE,
    file = path
  )
}

nad_manifest_required_columns <- function() {
  c(
    "county_fips",
    "state",
    "county",
    "row_count",
    "size_bytes",
    "sha256",
    "nad_revision",
    "installed_at"
  )
}

nad_assert_manifest_schema <- function(x, version = 23L) {
  nad_version_metadata(version)
  required <- nad_manifest_required_columns()
  if (!is.data.frame(x) || !identical(names(x), required)) {
    stop(
      "NAD county manifest must contain exactly: ",
      paste(required, collapse = ", "),
      call. = FALSE
    )
  }
  if (
    !is.character(x$county_fips) ||
      !is.character(x$state) ||
      !is.character(x$county) ||
      !is.numeric(x$row_count) ||
      !is.numeric(x$size_bytes) ||
      !is.character(x$sha256) ||
      !is.numeric(x$nad_revision) ||
      !is.character(x$installed_at)
  ) {
    stop("NAD county manifest has invalid column types", call. = FALSE)
  }
  if (nrow(x) == 0L) {
    return(invisible(TRUE))
  }
  if (anyNA(x)) {
    stop("NAD county manifest must not contain missing values", call. = FALSE)
  }
  if (any(!grepl("^[0-9]{5}$", x$county_fips))) {
    stop(
      "NAD county manifest contains invalid county FIPS codes",
      call. = FALSE
    )
  }
  if (any(!grepl("^[A-Z]{2}$", x$state))) {
    stop(
      "NAD county manifest contains invalid state abbreviations",
      call. = FALSE
    )
  }
  unsafe_county <- x$county == "" |
    x$county %in% c(".", "..") |
    grepl("[/\\\\]", x$county)
  if (any(unsafe_county)) {
    stop("NAD county manifest contains unsafe county names", call. = FALSE)
  }
  whole_nonnegative <- function(value) {
    is.finite(value) & value >= 0 & value == floor(value)
  }
  if (any(!whole_nonnegative(x$row_count))) {
    stop("NAD county manifest contains invalid row counts", call. = FALSE)
  }
  if (
    any(
      !is.finite(x$size_bytes) |
        x$size_bytes <= 0 |
        x$size_bytes != floor(x$size_bytes)
    )
  ) {
    stop("NAD county manifest contains invalid file sizes", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{64}$", x$sha256))) {
    stop("NAD county manifest contains invalid SHA-256 digests", call. = FALSE)
  }
  if (any(x$nad_revision != version)) {
    stop(
      "NAD county manifest contains an unexpected NAD revision",
      call. = FALSE
    )
  }
  if (
    any(
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
        x$installed_at
      )
    )
  ) {
    stop(
      "NAD county manifest contains invalid installation timestamps",
      call. = FALSE
    )
  }
  if (anyDuplicated(x$county_fips)) {
    stop("NAD county manifest contains duplicate counties", call. = FALSE)
  }

  resolved <- lapply(x$county_fips, nad_county_info)
  resolved_state <- vapply(resolved, `[[`, character(1), "state")
  resolved_county <- vapply(resolved, `[[`, character(1), "county")
  if (
    !identical(x$state, resolved_state) ||
      !identical(x$county, resolved_county)
  ) {
    stop(
      "NAD county manifest county metadata does not match its FIPS code",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

nad_validate_manifest <- function(
  x,
  data_root,
  version = 23L,
  verify_files = TRUE
) {
  nad_assert_manifest_schema(x, version = version)
  stopifnot(
    "data_root must be a character vector" = is.character(data_root),
    "data_root must be length one" = length(data_root) == 1L,
    "data_root must not be missing" = !is.na(data_root),
    "verify_files must be logical" = is.logical(verify_files),
    "verify_files must be length one" = length(verify_files) == 1L,
    "verify_files must not be missing" = !is.na(verify_files)
  )
  if (!verify_files) {
    return(invisible(TRUE))
  }

  relative_paths <- if (nrow(x) == 0L) {
    character()
  } else {
    mapply(
      nad_county_relative_path,
      county_fips = x$county_fips,
      state = x$state,
      USE.NAMES = FALSE
    )
  }
  expected_paths <- file.path(data_root, relative_paths)
  actual_paths <- if (dir.exists(data_root)) {
    list.files(
      data_root,
      pattern = "[.]parquet$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character()
  }
  normalize <- function(path) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  if (
    !identical(sort(normalize(actual_paths)), sort(normalize(expected_paths)))
  ) {
    stop(
      "NAD county manifest does not match the installed Parquet inventory",
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(x))) {
    path <- expected_paths[[i]]
    info <- file.info(path)
    if (!isTRUE(info$isdir == FALSE)) {
      stop("NAD county Parquet file is missing: ", path, call. = FALSE)
    }
    if (!identical(as.numeric(info$size), as.numeric(x$size_bytes[[i]]))) {
      stop(
        "NAD county Parquet size does not match manifest: ",
        path,
        call. = FALSE
      )
    }
    value <- tryCatch(nad_read_county_parquet(path), error = identity)
    if (inherits(value, "error")) {
      stop(
        "NAD county Parquet file is not readable with the required schema: ",
        path,
        call. = FALSE
      )
    }
    if (!identical(as.numeric(nrow(value)), as.numeric(x$row_count[[i]]))) {
      stop(
        "NAD county Parquet row count does not match manifest: ",
        path,
        call. = FALSE
      )
    }
    if (nrow(value) > 0L && any(value$county != x$county[[i]])) {
      stop(
        "NAD county Parquet county name does not match manifest: ",
        path,
        call. = FALSE
      )
    }
    if (!identical(nad_file_sha256(path), x$sha256[[i]])) {
      stop(
        "NAD county Parquet SHA-256 does not match manifest: ",
        path,
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

nad_with_manifest_lock <- function(version = 23L, expr) {
  lock_dir <- nad_manifest_lock_dir(version = version)
  timeout <- as.numeric(getOption("addr.nad_manifest_lock_timeout", 600))
  poll <- as.numeric(getOption("addr.nad_manifest_lock_poll", 0.25))
  stale_after <- as.numeric(
    getOption("addr.nad_manifest_lock_stale_after", 3600)
  )
  if (is.na(timeout) || timeout < 0) {
    timeout <- 600
  }
  if (is.na(poll) || poll <= 0) {
    poll <- 0.25
  }
  if (is.na(stale_after) || stale_after <= 0) {
    stale_after <- Inf
  }

  dir.create(dirname(lock_dir), recursive = TRUE, showWarnings = FALSE)
  token <- paste(
    Sys.getpid(),
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"),
    sep = "-"
  )
  token_path <- file.path(lock_dir, "owner")
  start <- Sys.time()
  acquired <- FALSE

  repeat {
    acquired <- dir.create(lock_dir, showWarnings = FALSE)
    if (isTRUE(acquired)) {
      writeLines(token, token_path)
      break
    }
    if (!dir.exists(lock_dir)) {
      next
    }
    lock_info <- file.info(lock_dir)
    lock_age <- as.numeric(difftime(
      Sys.time(),
      lock_info$mtime,
      units = "secs"
    ))
    if (is.finite(stale_after) && !is.na(lock_age) && lock_age > stale_after) {
      unlink(lock_dir, recursive = TRUE, force = TRUE)
      next
    }
    elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    if (is.finite(timeout) && elapsed >= timeout) {
      stop(
        "timed out waiting for NAD manifest lock at `",
        lock_dir,
        "`",
        call. = FALSE
      )
    }
    Sys.sleep(poll)
  }

  on.exit(
    {
      if (acquired && file.exists(token_path)) {
        owner <- readLines(token_path, warn = FALSE, n = 1L)
        if (identical(owner, token)) {
          unlink(lock_dir, recursive = TRUE, force = TRUE)
        }
      }
    },
    add = TRUE
  )
  eval(substitute(expr), parent.frame())
}

nad_manifest_lock_dir <- function(version = 23L) {
  nad_version_metadata(version)
  file.path(
    tools::R_user_dir("addr", "data"),
    "v1",
    "nad_manifest_locks",
    as.character(version),
    "update.lock"
  )
}


# Columns retained from the NAD flat source.
nad_source_fields <- function() {
  c(
    "AddNum_Pre",
    "Add_Number",
    "AddNum_Suf",
    "St_PreDir",
    "St_PreMod",
    "St_PreTyp",
    "St_Name",
    "St_PosTyp",
    "St_PosDir",
    "St_PosMod",
    "SubAddress",
    "County",
    "Post_City", # or use Inc_Muni
    "State",
    "Zip_Code",
    "UUID",
    "Latitude",
    "Longitude",
    "Parcel_ID",
    "DateUpdate",
    "NatGrid",
    "Placement",
    "AddrClass",
    "Addr_Type"
  )
}

#' @rdname nad
nad_read <- function(
  county,
  state = NULL,
  version = 23L,
  refresh_source = c("no", "yes", "force")
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state),
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  county_info <- nad_county_info(county, state)
  storage <- nad_read_storage(
    county = county,
    state = state,
    version = version,
    refresh_source = refresh_source
  )
  nad_storage_to_nad(storage, state = county_info$state)
}

nad_read_storage <- function(
  county,
  state = NULL,
  version = 23L,
  refresh_source = c("no", "yes", "force")
) {
  county_info <- nad_county_info(county, state)
  nad_md <- nad_version_metadata(version)
  nad_fields <- nad_source_fields()
  nad_source <- nad_download(
    version = version,
    refresh_source = refresh_source
  )
  rnad <- tibble::as_tibble(nad_flat_extract(
    path = nad_source,
    member = nad_md$source_members[[1L]],
    state = county_info$state,
    county = county_info$county,
    fields = nad_fields
  ))
  nad_prepare_storage(rnad, county_info$county_fips)
}

nad_prepare_storage <- function(rnad, county_fips) {
  bad_zips <- which(nchar(rnad$Zip_Code) != 5L)
  if (length(bad_zips) > 0) {
    warning(
      "removing ",
      length(bad_zips),
      " address records in ",
      county_fips,
      " with malformed ZIP codes."
    )
    rnad <- rnad[-bad_zips, ]
  }
  tibble::tibble(
    address_number_prefix = as.character(rnad$AddNum_Pre),
    address_number = as.character(rnad$Add_Number),
    address_number_suffix = as.character(rnad$AddNum_Suf),
    street_predirectional = as.character(rnad$St_PreDir),
    street_premodifier = as.character(rnad$St_PreMod),
    street_pretype = as.character(rnad$St_PreTyp),
    street_name = as.character(rnad$St_Name),
    street_posttype = as.character(rnad$St_PosTyp),
    street_postdirectional = as.character(rnad$St_PosDir),
    subaddress = as.character(rnad$SubAddress),
    county = as.character(rnad$County),
    place_name = as.character(rnad$Post_City),
    zipcode = as.character(rnad$Zip_Code),
    uuid = as.character(rnad$UUID),
    date_update = as.Date(rnad$DateUpdate),
    latitude = as.numeric(rnad$Latitude),
    longitude = as.numeric(rnad$Longitude),
    national_grid = as.character(rnad$NatGrid),
    placement = as.character(rnad$Placement),
    address_class = as.character(rnad$AddrClass),
    address_type = as.character(rnad$Addr_Type),
    parcel_id = as.character(rnad$Parcel_ID)
  )
}

nad_storage_to_nad <- function(storage, state) {
  nad_assert_storage_schema(storage)
  na_to_empty <- \(x) ifelse(is.na(x), "", x)
  nad_addr <-
    with(storage, {
      addr(
        addr_number(
          prefix = na_to_empty(address_number_prefix),
          digits = address_number,
          suffix = na_to_empty(address_number_suffix)
        ),
        addr_street(
          predirectional = na_to_empty(street_predirectional),
          premodifier = na_to_empty(street_premodifier),
          pretype = na_to_empty(street_pretype),
          name = street_name,
          posttype = na_to_empty(street_posttype),
          postdirectional = na_to_empty(street_postdirectional)
        ),
        addr_place(
          name = na_to_empty(place_name),
          state = rep.int(state, nrow(storage)),
          zip = zipcode
        )
      )
    })
  nad_s2 <- s2::as_s2_cell(s2::s2_lnglat(
    storage$longitude,
    storage$latitude
  ))
  tibble::tibble(
    nad_addr = nad_addr,
    subaddress = storage$subaddress,
    uuid = storage$uuid,
    date_update = storage$date_update,
    s2 = nad_s2,
    national_grid = storage$national_grid,
    placement = storage$placement,
    address_class = storage$address_class,
    address_type = storage$address_type,
    parcel_id = storage$parcel_id
  )
}

#' @param refresh_source character, length one; choose how to refresh the
#' compressed national source; `"no"` requires the stow-managed source to
#' exist, `"yes"` downloads it if missing, and `"force"` redownloads it
#' @rdname nad
nad_download <- function(
  version = 23L,
  refresh_source = c("yes", "no", "force")
) {
  refresh_source <- match.arg(refresh_source)
  nad_md <- nad_version_metadata(version)
  stow::stow(
    nad_md$dlurl,
    package = "addr",
    subdir = "nad",
    overwrite = identical(refresh_source, "force"),
    offline = identical(refresh_source, "no"),
    etag = FALSE,
    validate = function(path) {
      nad_validate_flat_source(
        path,
        nad_md$source_members,
        nad_md$source_size
      )
    }
  )
}

nad_validate_flat_source <- function(
  path,
  required_members,
  expected_size = NULL
) {
  stopifnot(
    "path must be a character vector" = is.character(path),
    "path must be length one" = length(path) == 1L,
    "path must not be missing" = !is.na(path),
    "required_members must be a character vector" = is.character(
      required_members
    ),
    "required_members must not contain missing values" = !any(is.na(
      required_members
    )),
    "required_members must not be empty" = length(required_members) > 0L,
    "expected_size must be NULL or numeric" = is.null(expected_size) ||
      is.numeric(expected_size),
    "expected_size must be NULL or length one" = is.null(expected_size) ||
      length(expected_size) == 1L,
    "expected_size must be NULL or not missing" = is.null(expected_size) ||
      !is.na(expected_size)
  )
  if (!file.exists(path)) {
    return(FALSE)
  }
  actual_size <- file.info(path)$size
  if (!is.null(expected_size) && actual_size != expected_size) {
    return(FALSE)
  }
  contents <- tryCatch(
    suppressWarnings(utils::unzip(path, list = TRUE)),
    error = function(e) NULL
  )
  listed <- is.data.frame(contents) &&
    "Name" %in% names(contents) &&
    all(required_members %in% contents$Name)
  if (listed) {
    return(TRUE)
  }

  # R's bundled ZIP reader cannot list USDOT's 7.6 GB ZIP64 archive. Fall
  # back to its pinned byte size and central-directory markers without
  # decompressing the roughly 41 GB text member.
  connection <- file(path, open = "rb")
  on.exit(close(connection))
  header <- readBin(connection, what = "raw", n = 4L)
  tail_size <- min(actual_size, 65557)
  seek(connection, where = actual_size - tail_size, origin = "start")
  archive_tail <- readBin(connection, what = "raw", n = tail_size)
  required_patterns <- lapply(required_members, charToRaw)
  all(
    identical(header, charToRaw("PK\003\004")),
    nad_raw_contains(archive_tail, charToRaw("PK\005\006")),
    vapply(
      required_patterns,
      function(pattern) nad_raw_contains(archive_tail, pattern),
      logical(1)
    )
  )
}

nad_raw_contains <- function(x, pattern) {
  limit <- length(x) - length(pattern) + 1L
  if (limit < 1L) {
    return(FALSE)
  }
  starts <- which(x[seq_len(limit)] == pattern[[1L]])
  any(vapply(
    starts,
    function(start) {
      identical(x[start + seq_along(pattern) - 1L], pattern)
    },
    logical(1)
  ))
}
