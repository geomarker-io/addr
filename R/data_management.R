#' Remove processed National Address Database counties
#'
#' `nad_remove()` previews or removes installed county Parquet files and updates
#' their manifest. Downloaded source archives remain managed by stow. Use
#' [stow::stow_info()], [stow::stow_remove()], and [stow::stow_prune()] to manage
#' those source files separately.
#' Stop installation and matching jobs before removing their processed data.
#'
#' @inheritParams nad
#' @param county County name or five-digit FIPS identifier. `NULL` selects all
#'   counties in the requested NAD revision.
#' @param dry_run Logical; preview the affected files without changing anything?
#'   Defaults to `TRUE`. Set to `FALSE` to remove the selected processed files.
#' @returns A tibble with `path` and `size_bytes` for the existing files selected
#'   for removal. Missing files are dropped from the manifest on actual removal.
#' @seealso [nad_manifest()], [taf_remove()]
#' @export
#' @examples
#' nad_remove("39061")
nad_remove <- function(county = NULL, state = NULL, version = 23L, dry_run = TRUE) {
  nad_version_metadata(version)
  addr_check_dry_run(dry_run)
  county_fips <- if (is.null(county)) NULL else nad_county_info(county, state)$county_fips
  remove <- function() {
    manifest <- nad_read_manifest(version)
    selected <- if (is.null(county_fips)) {
      rep(TRUE, nrow(manifest))
    } else {
      manifest$county_fips %in% county_fips
    }
    rows <- manifest[selected, , drop = FALSE]
    root <- nad_data_path(version)
    paths <- file.path(root, nad_county_relative_path(rows$county_fips, rows$state))
    # file.path() with empty vectors can retain the root on older R versions.
    if (!nrow(rows)) paths <- character()
    inventory <- addr_processed_inventory(paths, root)
    if (!dry_run && any(selected)) {
      addr_unlink_processed(inventory$path)
      # Keep the original manifest on failure so an incomplete county is
      # still recognized as missing by the installation preflight.
      addr_check_removed(inventory$path)
      nad_write_manifest(manifest[!selected, , drop = FALSE], version)
    }
    inventory
  }
  if (dry_run) remove() else nad_with_manifest_lock(version, remove())
}

#' Remove processed TIGER Address Features
#'
#' `taf_remove()` previews or removes processed county-ZIP Parquet files for
#' one year and updates that year's manifest. Downloaded feature-name and
#' address-feature ZIP files remain managed by stow. Use [stow::stow_info()],
#' [stow::stow_remove()], and [stow::stow_prune()] to manage those sources.
#'
#' Remove years that are no longer needed explicitly; addr does not discard
#' an older year automatically because it may still be used for an analysis.
#' Stop geocoding and installation jobs before removing their processed data.
#'
#' @param county Character vector of five-digit county FIPS identifiers, or
#'   `NULL` to select all counties in the requested year.
#' @inheritParams taf
#' @param version Character; processed-data schema version. Only `"v2"` is supported.
#' @inheritParams nad_remove
#' @returns A tibble with `path` and `size_bytes` for the existing files selected
#'   for removal. Missing files are dropped from the manifest on actual removal.
#' @seealso [taf_manifest()], [nad_remove()]
#' @export
#' @examples
#' taf_remove("39061", year = "2024")
taf_remove <- function(
  county = NULL,
  year = as.character(2025:2011),
  version = "v2",
  dry_run = TRUE
) {
  year <- match.arg(year)
  addr_check_dry_run(dry_run)
  stopifnot(
    "version must be 'v2'" = identical(version, "v2"),
    "county must be NULL or five-digit FIPS identifiers" = is.null(county) ||
      (is.character(county) && !anyNA(county) && all(grepl("^[0-9]{5}$", county)))
  )
  remove <- function() {
    manifest <- taf_read_county_zip_manifest(year, version)
    selected <- if (is.null(county)) {
      rep(TRUE, nrow(manifest))
    } else {
      manifest$county_fips %in% county
    }
    paths <- taf_manifest_file_paths(
      manifest[selected, , drop = FALSE],
      data_root = taf_dataset_path(year, version)
    )
    if (!any(selected)) paths <- character()
    inventory <- addr_processed_inventory(paths, taf_dataset_path(year, version))
    if (!dry_run && any(selected)) {
      addr_unlink_processed(inventory$path)
      # Do not mark a partly removed county as a complete installation.
      addr_check_removed(inventory$path)
      taf_write_county_zip_manifest(
        manifest[!selected, , drop = FALSE], year, version
      )
    }
    inventory
  }
  if (dry_run) remove() else taf_with_install_lock(year, version, remove())
}

addr_check_dry_run <- function(dry_run) {
  stopifnot("dry_run must be TRUE or FALSE" = is.logical(dry_run) &&
    length(dry_run) == 1L && !is.na(dry_run))
}

addr_processed_inventory <- function(paths, root) {
  paths <- unique(paths[file.exists(paths)])
  if (length(paths)) {
    normalized <- normalizePath(paths, winslash = "/", mustWork = TRUE)
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    if (any(!startsWith(normalized, paste0(root, "/"))) || any(file.info(paths)$isdir)) {
      stop("processed data paths must be files inside the dataset directory", call. = FALSE)
    }
  }
  tibble::tibble(path = paths, size_bytes = unname(file.info(paths)$size))
}

addr_unlink_processed <- function(paths) {
  if (length(paths)) unlink(paths)
  invisible(NULL)
}

addr_check_removed <- function(paths) {
  if (any(file.exists(paths))) {
    stop("could not remove processed data file: ", paths[file.exists(paths)][[1L]], call. = FALSE)
  }
  invisible(NULL)
}
