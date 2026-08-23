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
#' Data binaries are the persistent outputs of `nad_read()` for each
#' County/State and are created on first run with `nad()`.
#' The compressed national source is managed exclusively by `stow()` beneath
#' `stow::stow_path(package = "addr", subdir = "nad")`. Derived county RDS
#' files are separate processed data beneath
#' `file.path(tools::R_user_dir("addr", "data"), "v1", "nad", "23")`, organized
#' by state and county name. `nad_manifest()` inventories those county files
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
#' @param refresh_binary character, length one; choose how to refresh NAD
#' data binaries stored on disk if not already present; "yes" will
#' create data binary if not already present, "no" will
#' error if data binary is not already present, "force" will
#' create the data binary and overwrite any existing data binary
#'
#' @details
#' The revision 23 comma-delimited flat source archive is downloaded from the
#' transportation.gov data portal:
#' <https://data.transportation.gov/d/fc2s-wawr>.
#' `nad_download(version = 23L)` installs the pinned compressed archive as a
#' durable managed local copy using `stow::stow()`. County installation streams
#' the nationwide text member directly from that archive, retains the requested
#' county, and writes a separate processed RDS file.
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
#'   # county RDS from it
#'   nad_download()
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
  nad_sd <- nad_sd_path(
    county = county_info$county,
    state = county_info$state,
    version = version
  )
  if (!file.exists(nad_sd) || refresh_binary == "force") {
    if (refresh_binary == "no") {
      stop(
        nad_sd,
        " does not exist; set `refresh_binary = 'yes'`",
        " to install from source NAD"
      )
    }
    if (refresh_binary == "yes") {
      message(nad_sd, " does not exist; installing from source...")
    } else {
      message("forcing install from source...")
    }
    dir.create(dirname(nad_sd), recursive = TRUE, showWarnings = FALSE)
    d <- nad_read(
      county = county,
      state = state,
      version = version,
      refresh_source = refresh_source
    )
    nad_write_county_rds(d, nad_sd)
    nad_upsert_manifest(
      path = nad_sd,
      data = d,
      county_info = county_info,
      version = version
    )
    return(d)
  }
  d <- readRDS(nad_sd)
  nad_upsert_manifest(
    path = nad_sd,
    data = d,
    county_info = county_info,
    version = version
  )
  d
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

nad_sd_path <- function(county, state, version = 23L) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be a character vector" = is.character(state),
    "state must be length one" = length(state) == 1L,
    "state must not be missing" = !is.na(state)
  )
  nad_version_metadata(version)
  file.path(
    tools::R_user_dir("addr", "data"),
    "v1",
    "nad",
    as.character(version),
    state,
    sprintf("%s.rds", county)
  )
}

#' Inventory installed National Address Database counties
#'
#' `nad_manifest()` reads the local manifest written as county RDS files are
#' installed by `nad()`. The RDS path remains the primary existence check used
#' by `nad()`; the manifest provides a compact inventory and integrity metadata
#' for validation and portable NAD fuel bundles.
#'
#' @param version integer, length one; NAD revision to inventory. Only revision
#'   23 is supported.
#' @param validate logical, length one; validate every manifest row against its
#'   county RDS, including its readable row count, byte size, and SHA-256
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

nad_write_county_rds <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(path)), "-"),
    tmpdir = dirname(path),
    fileext = ".rds"
  )
  on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
  saveRDS(x, file = tmp_path)
  written <- readRDS(tmp_path)
  if (!is.data.frame(written) || nrow(written) != nrow(x)) {
    stop(
      "temporary NAD county RDS failed validation: ",
      tmp_path,
      call. = FALSE
    )
  }
  nad_atomic_replace(tmp_path, path)
  invisible(path)
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

  expected_paths <- file.path(
    data_root,
    x$state,
    paste0(x$county, ".rds")
  )
  actual_paths <- if (dir.exists(data_root)) {
    list.files(
      data_root,
      pattern = "[.]rds$",
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
      "NAD county manifest does not match the installed RDS inventory",
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(x))) {
    path <- expected_paths[[i]]
    info <- file.info(path)
    if (!isTRUE(info$isdir == FALSE)) {
      stop("NAD county RDS is missing: ", path, call. = FALSE)
    }
    if (!identical(as.numeric(info$size), as.numeric(x$size_bytes[[i]]))) {
      stop("NAD county RDS size does not match manifest: ", path, call. = FALSE)
    }
    value <- tryCatch(readRDS(path), error = identity)
    if (inherits(value, "error") || !is.data.frame(value)) {
      stop("NAD county RDS is not readable tabular data: ", path, call. = FALSE)
    }
    if (!identical(as.numeric(nrow(value)), as.numeric(x$row_count[[i]]))) {
      stop(
        "NAD county RDS row count does not match manifest: ",
        path,
        call. = FALSE
      )
    }
    if (!identical(nad_file_sha256(path), x$sha256[[i]])) {
      stop(
        "NAD county RDS SHA-256 does not match manifest: ",
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
  rnad$Longitude <- as.numeric(rnad$Longitude)
  rnad$Latitude <- as.numeric(rnad$Latitude)
  nad_transform(rnad, county_info$county_fips)
}

nad_transform <- function(rnad, county_fips) {
  na_to_empty <- \(x) ifelse(is.na(x), "", x)
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
  rnad_addr <-
    with(rnad, {
      addr(
        addr_number(
          prefix = na_to_empty(AddNum_Pre),
          digits = as.character(Add_Number),
          suffix = na_to_empty(AddNum_Suf)
        ),
        addr_street(
          predirectional = na_to_empty(St_PreDir),
          premodifier = na_to_empty(St_PreMod),
          pretype = na_to_empty(St_PreTyp),
          name = St_Name,
          posttype = na_to_empty(St_PosTyp),
          postdirectional = na_to_empty(St_PosDir)
        ),
        addr_place(
          name = na_to_empty(Post_City),
          state = na_to_empty(State),
          zip = Zip_Code
        )
      )
    })
  rnad_s2 <- s2::as_s2_cell(s2::s2_lnglat(rnad$Longitude, rnad$Latitude))
  tibble::tibble(
    nad_addr = rnad_addr,
    subaddress = rnad$SubAddress,
    uuid = rnad$UUID,
    date_update = as.Date(rnad$DateUpdate),
    s2 = rnad_s2,
    national_grid = rnad$NatGrid,
    placement = rnad$Placement,
    address_class = rnad$AddrClass,
    address_type = rnad$Addr_Type,
    parcel_id = rnad$Parcel_ID
  )
}

#' @param refresh_source character, length one; choose how to refresh NAD
#' source archive on disk if not already present; "yes" will download the
#' archive if needed, "no" will require an existing local source, and "force"
#' will download and overwrite an existing source
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
