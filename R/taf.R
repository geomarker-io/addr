#' Open installed TIGER Address Features as an Arrow dataset
#'
#' @description
#'
#' `taf_dataset()` uses the arrow package to open the Hive-partitioned Parquet
#' dataset of TIGER address features in the addr user data directory.
#' Arrow `FileSystemDataset` objects are database-like backends for
#' larger-than-memory datasets and support dplyr syntax for data manipulation;
#' see <https://arrow.apache.org/docs/r/articles/data_wrangling.html>.
#' Other TAF helpers such as `taf_catalog()`, `taf_install()`, and `taf()`
#' use nanoparquet directly for flat parquet file reads and writes. Arrow is
#' only required for the advanced dataset interface returned by
#' `taf_dataset()`.
#'
#' @param year character, length one; vintage of TIGER addrfeat (address feature)
#'   files
#' @param version character, length one; major version of the package
#' and taf dataset schema
#' @returns a Dataset R6 object (see `?arrow::open_dataset`); use `dplyr`
#' verbs to query the data and get results, see examples
#' @export
#' @examples
#' \dontrun{
#'   Sys.setenv("R_USER_DATA_DIR" = tempfile())
#'   taf_install("39061", "2025")
#'
#'   if (requireNamespace("arrow", quietly = TRUE) &&
#'     requireNamespace("dplyr", quietly = TRUE)) {
#'     taf_dataset()
#'
#'     # find top ten most frequent street name-posttype combinations
#'     taf_dataset() |>
#'       dplyr::group_by(street_name, street_posttype) |>
#'       dplyr::summarize(
#'         n_zips = dplyr::n_distinct(ZIP),
#'         n_ranges = dplyr::n(),
#'         .groups = "drop"
#'       ) |>
#'       dplyr::arrange(dplyr::desc(n_zips), dplyr::desc(n_ranges)) |>
#'       dplyr::collect() |>
#'       dplyr::slice(1:10)
#'   }
#' }
taf_dataset <- function(year = as.character(2025:2011), version = "v2") {
  check_installed("arrow", "to open the multi-file taf dataset")
  stopifnot(
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  year <- match.arg(year)
  taf_path <- taf_dataset_path(year = year, version = version)
  dir.create(taf_path, showWarnings = FALSE, recursive = TRUE)
  arrow::open_dataset(
    taf_path,
    format = "parquet",
    partitioning = arrow::hive_partition(
      zip3 = arrow::string(),
      zip2 = arrow::string()
    )
  )
}

#' Read TIGER address feature ZIP/county catalog
#'
#' `taf_catalog()` reads a TIGER-derived catalog of ZIP codes present in each
#' county's TIGER address feature file for a specific year and addr TAF schema
#' version. The catalog is installed with the package and is used to plan which
#' county TAF files may be needed for a set of ZIP codes. It is separate from
#' the local install manifest, which records only files installed on the current
#' machine.
#'
#' @inheritParams taf
#' @returns a tibble with `county_fips`, `ZIP`, `zip3`, `zip2`, and `n_ranges`
#'   columns
#' @export
#' @examples
#' taf_catalog("2025")
taf_catalog <- function(year = as.character(2025:2011), version = "v2") {
  stopifnot(
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  year <- match.arg(year)
  catalog_path <- taf_catalog_path(year = year, version = version)
  if (catalog_path == "" || !file.exists(catalog_path)) {
    stop(
      "taf catalog is not installed for year `",
      year,
      "` and version `",
      version,
      "`",
      call. = FALSE
    )
  }
  nanoparquet::read_parquet(catalog_path) |>
    tibble::as_tibble()
}

#' Inventory installed TIGER Address Feature files
#'
#' `taf_manifest()` reads the local inventory written as county TAF files are
#' installed. Each row represents one installed county-ZIP Parquet file. Set
#' `validate = TRUE` to verify the manifest schema and every inventoried file,
#' including its storage schema, row count, byte size, and SHA-256 digest.
#'
#' The local manifest is distinct from [taf_catalog()]. The catalog describes
#' every county-ZIP combination available from TIGER, while the manifest records
#' only files installed on the current machine.
#'
#' @inheritParams taf
#' @param validate logical, length one; validate every manifest row and reject
#'   missing, untracked, unreadable, or modified Parquet files?
#' @returns A tibble with one row per installed county-ZIP file and columns
#'   `county_fips`, `ZIP`, `zip3`, `zip2`, `n_ranges`, `size_bytes`, `sha256`,
#'   `taf_year`, `taf_version`, and `installed_at`.
#' @export
#' @examples
#' \dontrun{
#' taf_manifest()
#' taf_manifest(validate = TRUE)
#' }
taf_manifest <- function(
  year = as.character(2025:2011),
  version = "v2",
  validate = FALSE
) {
  stopifnot(
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version),
    "validate must be logical" = is.logical(validate),
    "validate must be length one" = length(validate) == 1L,
    "validate must not be missing" = !is.na(validate)
  )
  year <- match.arg(year)
  manifest <- taf_read_county_zip_manifest(year = year, version = version)
  if (validate) {
    taf_validate_manifest(
      manifest,
      data_root = taf_dataset_path(year = year, version = version),
      year = year,
      version = version,
      verify_files = TRUE
    )
  }
  manifest
}

#' Find and install TAF counties needed for ZIP codes
#'
#' `taf_needed_counties()` uses `taf_catalog()` to identify county TAF files
#' that may contain address ranges for ZIP codes in `x`. For `addr` input it
#' includes selected place, county-subdivision, and typographical ZIP candidates
#' when requested. Character input contains ZIP codes only, so place-derived
#' candidates cannot be inferred. `taf_ensure()` installs any needed counties
#' that are not already present in the local TAF manifest.
#'
#' @param x an addr vector (`?as_addr`) or character vector of ZIP codes
#' @inheritParams taf
#' @inheritParams match_zipcodes
#' @inheritParams taf_install
#' @param place_zip_variants logical; for `addr` input, consider ZCTAs associated
#'   with the normalized input place or county subdivision?
#' @param place_zip_variant nonempty character vector containing `"place"`
#'   and/or `"county-sub"`; requested order determines precedence
#' @returns `taf_needed_counties()` returns a tibble with catalog columns plus
#'   `source_zip` and `source_zip_variant`. `taf_ensure()` invisibly returns the
#'   subset of needed counties that were missing before installation.
#' @export
#' @examples
#' taf_needed_counties(as_addr("10 MAIN ST CINCINNATI OH 45220"))
taf_needed_counties <- function(
  x,
  year = as.character(2025:2011),
  version = "v2",
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  place_zip_variants = TRUE,
  place_zip_variant = c("place", "county-sub")
) {
  stopifnot(
    "x must be an addr vector or character vector" = inherits(x, "addr") ||
      is.character(x),
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants),
    "place_zip_variants must be TRUE or FALSE" = is.logical(
      place_zip_variants
    ) &&
      length(place_zip_variants) == 1L &&
      !is.na(place_zip_variants)
  )
  zip_variant <- validate_zip_variant(zip_variant)
  place_zip_variant <- validate_place_zip_variant(place_zip_variant)
  year <- match.arg(year)

  zipcodes <- if (inherits(x, "addr")) {
    geocode_zip_candidates(
      x,
      zip_variants = zip_variants,
      zip_variant = zip_variant,
      place_zip_variants = place_zip_variants,
      place_zip_variant = place_zip_variant
    )
  } else {
    taf_needed_zipcodes(
      x,
      zip_variants = zip_variants,
      zip_variant = zip_variant
    )
  }
  taf_needed_counties_from_zipcodes(
    zipcodes,
    year = year,
    version = version
  )
}

taf_needed_counties_from_zipcodes <- function(zipcodes, year, version) {
  if (nrow(zipcodes) == 0L) {
    return(taf_empty_needed_counties())
  }

  catalog <- taf_catalog(year = year, version = version)
  out <- merge(
    zipcodes,
    catalog,
    by.x = "ZIP",
    by.y = "ZIP",
    all = FALSE,
    sort = FALSE
  )
  if (nrow(out) == 0L) {
    return(taf_empty_needed_counties())
  }

  keep <- c(
    "county_fips",
    "ZIP",
    "zip3",
    "zip2",
    "n_ranges",
    "source_zip",
    "source_zip_variant",
    "source_zip_variant_rank"
  )
  if ("candidate_rank" %in% names(out)) {
    keep <- c(keep, "candidate_rank")
  }
  out <- out[keep]
  out <- unique(out)
  candidate_rank <- if ("candidate_rank" %in% names(out)) {
    out$candidate_rank
  } else {
    rep.int(0L, nrow(out))
  }
  out <- out[
    order(
      out$source_zip,
      out$source_zip_variant_rank,
      candidate_rank,
      out$county_fips
    ),
    ,
    drop = FALSE
  ]
  out$source_zip_variant_rank <- NULL
  if ("candidate_rank" %in% names(out)) {
    out$candidate_rank <- NULL
  }
  out <- unique(out)
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

#' @rdname taf_needed_counties
#' @export
taf_ensure <- function(
  x,
  year = as.character(2025:2011),
  version = "v2",
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  place_zip_variants = TRUE,
  place_zip_variant = c("place", "county-sub"),
  redownload = FALSE
) {
  stopifnot(
    "redownload must be logical" = is.logical(redownload),
    "redownload must be length one" = length(redownload) == 1L,
    "redownload must not be missing" = !is.na(redownload)
  )
  year <- match.arg(year)
  missing <- taf_missing_counties(
    x,
    year = year,
    version = version,
    zip_variants = zip_variants,
    zip_variant = zip_variant,
    place_zip_variants = place_zip_variants,
    place_zip_variant = place_zip_variant
  )
  if (nrow(missing) == 0L) {
    return(invisible(missing))
  }

  for (county in unique(missing$county_fips)) {
    taf_install(
      county = county,
      year = year,
      version = version,
      overwrite = FALSE,
      redownload = redownload
    )
  }
  invisible(missing)
}

taf_ensure_serial <- function(
  x,
  year,
  version,
  zip_variants,
  zip_variant,
  place_zip_variants,
  place_zip_variant,
  redownload
) {
  missing <- taf_missing_counties(
    x,
    year = year,
    version = version,
    zip_variants = zip_variants,
    zip_variant = zip_variant,
    place_zip_variants = place_zip_variants,
    place_zip_variant = place_zip_variant
  )
  if (nrow(missing) == 0L) {
    return(invisible(missing))
  }

  taf_with_install_lock(year, version, {
    taf_ensure(
      x,
      year = year,
      version = version,
      zip_variants = zip_variants,
      zip_variant = zip_variant,
      place_zip_variants = place_zip_variants,
      place_zip_variant = place_zip_variant,
      redownload = redownload
    )
  })
}

taf_with_install_lock <- function(year, version, expr) {
  lock_dir <- taf_install_lock_dir(year = year, version = version)
  timeout <- as.numeric(getOption("addr.taf_install_lock_timeout", 600))
  poll <- as.numeric(getOption("addr.taf_install_lock_poll", 0.25))
  stale_after <- as.numeric(
    getOption("addr.taf_install_lock_stale_after", 3600)
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
  if (file.exists(token_path)) {
    owner <- readLines(token_path, warn = FALSE, n = 1L)
    if (
      length(owner) == 1L &&
        startsWith(owner, paste0(Sys.getpid(), "-"))
    ) {
      return(eval(substitute(expr), parent.frame()))
    }
  }
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
        "timed out waiting for TAF install lock at `",
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

taf_install_lock_dir <- function(year, version) {
  file.path(
    tools::R_user_dir("addr", "data"),
    version,
    "tiger_addr_feat_locks",
    year,
    "install.lock"
  )
}

#' @name taf
#' @param county character, length 1; county FIPS code
#' @param overwrite logical, length 1; overwrite an existing county install?
#' @param redownload logical, length 1; replace existing durable managed local
#'   copies of TIGER ZIP files?
#' @export
#' @examples
#' \dontrun{
#'   Sys.setenv("R_USER_DATA_DIR" = tempfile())
#'   taf_install("39061", "2025")
#' }
taf_install <- function(
  county,
  year = as.character(2025:2011),
  version = "v2",
  overwrite = FALSE,
  redownload = FALSE
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must not be missing" = !is.na(year)
  )
  year <- match.arg(year)
  stopifnot(
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version),
    "overwrite must be logical" = is.logical(overwrite),
    "overwrite must be length one" = length(overwrite) == 1L,
    "overwrite must not be missing" = !is.na(overwrite),
    "redownload must be logical" = is.logical(redownload),
    "redownload must be length one" = length(redownload) == 1L,
    "redownload must not be missing" = !is.na(redownload)
  )
  manifest <- taf_read_county_zip_manifest(year = year, version = version)
  county_manifest <- manifest[manifest$county_fips == county, , drop = FALSE]
  county_installed <- county %in%
    taf_installed_counties(
      manifest,
      year = year,
      version = version
    )
  if (county_installed && !overwrite) {
    return(invisible(county))
  }

  d_names <- tiger_feat_names(
    county = county,
    year = year,
    redownload = redownload
  )
  d_geom <- tiger_addr_feat(
    county = county,
    year = year,
    redownload = redownload
  )

  idn <- match(d_geom$LINEARID, d_names$LINEARID)
  lid_no_name <- which(is.na(idn))
  if (length(lid_no_name) > 0) {
    # warning(
    #   "Parsed ",
    #   format(
    #     length(unique(d_geom[lid_no_name, "FULLNAME", drop = TRUE])),
    #     big.mark = ","
    #   ),
    #   " addr_street ",
    #   "without matching feature names (county ",
    #   county,
    #   ")",
    #   call. = FALSE
    # )
    lid_no_name_parse <-
      d_geom[lid_no_name, "FULLNAME", drop = TRUE] |>
      paste("3", street = _, "Anytown", "OHIO", "45000") |>
      as_addr() |>
      S7::prop("street")
  }
  addr_street_out <- tibble::as_tibble(d_names$addr_street[idn])
  if (length(lid_no_name) > 0) {
    addr_street_out[lid_no_name, ] <- tibble::as_tibble(lid_no_name_parse)
  }

  out <- tibble::tibble(d_geom, addr_street_out)
  out$county_fips <- county
  out$street_tag_parsed <- FALSE
  out[lid_no_name, "street_tag_parsed"] <- TRUE
  out$geometry_wkt <- s2::s2_as_text(out$s2_geography)
  out$s2_geography <- NULL
  out$zip3 <- substr(out$ZIP, 1, 3)
  out$zip2 <- substr(out$ZIP, 4, 5)

  zip_groups <- split(seq_len(nrow(out)), paste(out$zip3, out$zip2, sep = ":"))
  for (idx in zip_groups) {
    out_part <- out[idx, , drop = FALSE]
    taf_path <- file.path(
      taf_dataset_path(year = year, version = version),
      sprintf("zip3=%s", out_part$zip3[[1]]),
      sprintf("zip2=%s", out_part$zip2[[1]])
    )
    dir.create(taf_path, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(taf_path, sprintf("%s.parquet", county))
    out_part$zip2 <- NULL
    out_part$zip3 <- NULL
    taf_write_county_parquet(
      out_part,
      out_file,
      county = county,
      ZIP = out_part$ZIP[[1L]]
    )
  }
  manifest_add <- taf_county_zip_manifest_rows(
    out,
    county = county,
    year = year,
    version = version
  )
  taf_with_install_lock(year, version, {
    manifest <- taf_read_county_zip_manifest(year = year, version = version)
    county_manifest <- manifest[
      manifest$county_fips == county,
      ,
      drop = FALSE
    ]
    if (nrow(county_manifest) > 0L) {
      old_paths <- taf_manifest_file_paths(
        county_manifest,
        data_root = taf_dataset_path(year = year, version = version)
      )
      new_paths <- taf_manifest_file_paths(
        manifest_add,
        data_root = taf_dataset_path(year = year, version = version)
      )
      unlink(setdiff(old_paths, new_paths), force = TRUE)
    }
    manifest <- manifest[manifest$county_fips != county, , drop = FALSE]
    manifest <- vctrs::vec_rbind(manifest, manifest_add)
    taf_write_county_zip_manifest(manifest, year = year, version = version)
  })
  return(invisible(county))
}

#' Read TIGER Address Features into R
#'
#' `taf()` reads installed TIGER Address Features for one or more ZIP codes.
#' It reconstructs the `county_fips`, `s2_geography`, and `addr_street`
#' vectors in the returned data frame. `taf_install()` installs one county's
#' processed Parquet files from the stow-managed TIGER sources.
#' Use `taf_dataset()` to query all installed flat files lazily with Arrow.
#'
#' About 6% of ADDRFEAT rows do not have a county-local primary FEATNAMES
#' match by LINEARID. In these cases, street tags are parsed from the
#' ADDRFEAT full name, and the `street_tag_parsed` column is set to `TRUE`.
#' @param x character vector of five-digit ZIP codes
#' @param map logical, length 1; map street tags read from the TAF data
#' (type, directional, ordinal) when converting to `addr_street()` vector?
#' @param year character, length 1; vintage of TIGER addrfeat (address feature)
#'   files
#' @param version character, length 1; major version of the package
#' and taf dataset schema
#' @returns `taf()` returns a tibble with `LINEARID`, `FULLNAME`, `side`,
#'   `ZIP`, `FROMHN`, `TOHN`, `PARITY`, `OFFSET`, `s2_geography`,
#'   `addr_street`, `county_fips`, and `street_tag_parsed` columns.
#'   `taf_install()` invisibly returns the installed county FIPS identifier.
#' @name taf
#' @export
#' @examples
#' \dontrun{
#'   Sys.setenv("R_USER_DATA_DIR" = tempfile())
#'   taf_install("39061", "2025")
#'   taf(c("45249", "45230", "45220"))
#' }
taf <- function(
  x,
  map = TRUE,
  year = as.character(2025:2011),
  version = "v2"
) {
  stopifnot(is.character(x), length(x) > 0, !any(is.na(x)))
  stopifnot(
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  year <- match.arg(year)
  x <- unique(addr::addr_place(zipcode = x)@zipcode)
  manifest <- taf_read_county_zip_manifest(year = year, version = version)
  zip_manifest <- manifest[manifest$ZIP %in% x, , drop = FALSE]
  if (nrow(zip_manifest) == 0L) {
    d <- taf_empty_zip_tibble()
  } else {
    file_paths <- file.path(
      taf_dataset_path(year = year, version = version),
      sprintf("zip3=%s", zip_manifest$zip3),
      sprintf("zip2=%s", zip_manifest$zip2),
      sprintf("%s.parquet", zip_manifest$county_fips)
    )
    file_paths <- unique(file_paths[file.exists(file_paths)])
    if (length(file_paths) == 0L) {
      d <- taf_empty_zip_tibble()
    } else {
      d <- lapply(file_paths, function(path) {
        nanoparquet::read_parquet(path) |>
          tibble::as_tibble()
      }) |>
        do.call(what = vctrs::vec_rbind)
    }
  }
  d$s2_geography <- s2::as_s2_geography(d$geometry_wkt)
  d$geometry_wkt <- NULL
  d$addr_street <- addr_street(
    predirectional = d$street_predirectional,
    premodifier = d$street_premodifier,
    pretype = d$street_pretype,
    name = d$street_name,
    posttype = d$street_posttype,
    postdirectional = d$street_postdirectional,
    map_posttype = map,
    map_directional = map,
    map_pretype = map,
    map_ordinal = map
  )
  d$street_predirectional <- NULL
  d$street_premodifier <- NULL
  d$street_pretype <- NULL
  d$street_name <- NULL
  d$street_posttype <- NULL
  d$street_postdirectional <- NULL
  d$zip2 <- NULL
  d$zip3 <- NULL
  d
}

taf_dataset_path <- function(year, version) {
  file.path(
    tools::R_user_dir("addr", "data"),
    version,
    "tiger_addr_feat",
    year
  )
}

taf_county_zip_manifest_path <- function(year, version) {
  file.path(
    tools::R_user_dir("addr", "data"),
    version,
    "tiger_addr_feat_manifest",
    year,
    "county_zip.parquet"
  )
}

taf_catalog_path <- function(year, version) {
  override_dir <- getOption("addr.taf_catalog_dir")
  if (!is.null(override_dir)) {
    return(file.path(
      override_dir,
      version,
      "tiger_addr_feat_catalog",
      year,
      "county_zip.parquet"
    ))
  }

  package_path <- system.file(
    "extdata",
    version,
    "tiger_addr_feat_catalog",
    year,
    "county_zip.parquet",
    package = "addr"
  )
  if (nzchar(package_path)) {
    return(package_path)
  }

  source_path <- taf_catalog_source_path(year = year, version = version)
  if (file.exists(source_path)) {
    return(source_path)
  }

  ""
}

taf_catalog_source_path <- function(year, version, root = ".") {
  file.path(
    root,
    "inst",
    "extdata",
    version,
    "tiger_addr_feat_catalog",
    year,
    "county_zip.parquet"
  )
}

taf_read_county_zip_manifest <- function(year, version) {
  manifest_path <- taf_county_zip_manifest_path(year = year, version = version)
  if (!file.exists(manifest_path)) {
    return(taf_empty_manifest())
  }
  manifest <- nanoparquet::read_parquet(manifest_path) |>
    tibble::as_tibble()
  taf_assert_manifest_schema(manifest, year = year, version = version)
  manifest
}

taf_write_county_zip_manifest <- function(x, year, version) {
  taf_assert_manifest_schema(x, year = year, version = version)
  if (nrow(x) > 0L) {
    x <- x[order(x$ZIP, x$county_fips), , drop = FALSE]
    row.names(x) <- NULL
  }
  manifest_path <- taf_county_zip_manifest_path(year = year, version = version)
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(
    pattern = "county_zip_",
    tmpdir = dirname(manifest_path),
    fileext = ".parquet"
  )
  on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
  nanoparquet::write_parquet(x, tmp_path)
  written <- nanoparquet::read_parquet(tmp_path) |>
    tibble::as_tibble()
  taf_assert_manifest_schema(written, year = year, version = version)
  taf_atomic_replace(tmp_path, manifest_path)
  invisible(manifest_path)
}

taf_empty_manifest <- function() {
  tibble::tibble(
    county_fips = character(),
    ZIP = character(),
    zip3 = character(),
    zip2 = character(),
    n_ranges = integer(),
    size_bytes = numeric(),
    sha256 = character(),
    taf_year = character(),
    taf_version = character(),
    installed_at = character()
  )
}

taf_manifest_required_columns <- function() {
  names(taf_empty_manifest())
}

taf_assert_manifest_schema <- function(x, year, version) {
  required <- taf_manifest_required_columns()
  if (!is.data.frame(x) || !identical(names(x), required)) {
    stop(
      "TAF manifest must contain exactly: ",
      paste(required, collapse = ", "),
      call. = FALSE
    )
  }
  if (
    !is.character(x$county_fips) ||
      !is.character(x$ZIP) ||
      !is.character(x$zip3) ||
      !is.character(x$zip2) ||
      !is.numeric(x$n_ranges) ||
      !is.numeric(x$size_bytes) ||
      !is.character(x$sha256) ||
      !is.character(x$taf_year) ||
      !is.character(x$taf_version) ||
      !is.character(x$installed_at)
  ) {
    stop("TAF manifest has invalid column types", call. = FALSE)
  }
  if (nrow(x) == 0L) {
    return(invisible(TRUE))
  }
  if (anyNA(x)) {
    stop("TAF manifest must not contain missing values", call. = FALSE)
  }
  if (any(!grepl("^[0-9]{5}$", x$county_fips))) {
    stop("TAF manifest contains invalid county FIPS codes", call. = FALSE)
  }
  if (any(!grepl("^[0-9]{5}$", x$ZIP))) {
    stop("TAF manifest contains invalid ZIP codes", call. = FALSE)
  }
  if (
    any(x$zip3 != substr(x$ZIP, 1L, 3L)) ||
      any(x$zip2 != substr(x$ZIP, 4L, 5L))
  ) {
    stop("TAF manifest ZIP partitions do not match ZIP", call. = FALSE)
  }
  whole_positive <- function(value) {
    is.finite(value) & value > 0 & value == floor(value)
  }
  if (any(!whole_positive(x$n_ranges))) {
    stop("TAF manifest contains invalid range counts", call. = FALSE)
  }
  if (any(!whole_positive(x$size_bytes))) {
    stop("TAF manifest contains invalid file sizes", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{64}$", x$sha256))) {
    stop("TAF manifest contains invalid SHA-256 digests", call. = FALSE)
  }
  if (any(x$taf_year != year)) {
    stop("TAF manifest year does not match requested year", call. = FALSE)
  }
  if (any(x$taf_version != version)) {
    stop("TAF manifest version does not match requested version", call. = FALSE)
  }
  if (any(!nzchar(x$installed_at))) {
    stop("TAF manifest contains invalid installation timestamps", call. = FALSE)
  }
  keys <- paste(x$county_fips, x$ZIP, sep = "\r")
  if (anyDuplicated(keys) != 0L) {
    stop("TAF manifest contains duplicate county-ZIP rows", call. = FALSE)
  }
  invisible(TRUE)
}

taf_manifest_file_paths <- function(x, data_root) {
  file.path(
    data_root,
    sprintf("zip3=%s", x$zip3),
    sprintf("zip2=%s", x$zip2),
    sprintf("%s.parquet", x$county_fips)
  )
}

taf_validate_manifest <- function(
  x,
  data_root,
  year,
  version,
  verify_files = TRUE
) {
  taf_assert_manifest_schema(x, year = year, version = version)
  if (!verify_files) {
    return(invisible(TRUE))
  }

  expected_paths <- taf_manifest_file_paths(x, data_root = data_root)
  actual_paths <- if (dir.exists(data_root)) {
    list.files(
      data_root,
      pattern = "[.]parquet$",
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = FALSE
    )
  } else {
    character()
  }
  expected_normalized <- normalizePath(expected_paths, mustWork = FALSE)
  actual_normalized <- normalizePath(actual_paths, mustWork = FALSE)
  missing_paths <- expected_paths[!expected_normalized %in% actual_normalized]
  if (length(missing_paths) > 0L) {
    stop(
      "TAF manifest file is missing: ",
      missing_paths[[1L]],
      call. = FALSE
    )
  }
  untracked_paths <- actual_paths[!actual_normalized %in% expected_normalized]
  if (length(untracked_paths) > 0L) {
    stop(
      "TAF data file is not inventoried in the manifest: ",
      untracked_paths[[1L]],
      call. = FALSE
    )
  }

  for (i in seq_len(nrow(x))) {
    path <- expected_paths[[i]]
    size_bytes <- unname(file.info(path)$size)
    if (!identical(as.numeric(size_bytes), as.numeric(x$size_bytes[[i]]))) {
      stop("TAF manifest file size mismatch: ", path, call. = FALSE)
    }
    if (!identical(taf_file_sha256(path), x$sha256[[i]])) {
      stop("TAF manifest SHA-256 mismatch: ", path, call. = FALSE)
    }
    value <- tryCatch(
      nanoparquet::read_parquet(path) |> tibble::as_tibble(),
      error = identity
    )
    if (inherits(value, "error")) {
      stop("TAF manifest file is unreadable: ", path, call. = FALSE)
    }
    taf_assert_storage_schema(
      value,
      county = x$county_fips[[i]],
      ZIP = x$ZIP[[i]],
      path = path
    )
    if (nrow(value) != x$n_ranges[[i]]) {
      stop("TAF manifest row count mismatch: ", path, call. = FALSE)
    }
  }
  invisible(TRUE)
}

taf_refresh_manifest_file_metadata <- function(x, data_root, year, version) {
  taf_assert_manifest_schema(x, year = year, version = version)
  paths <- taf_manifest_file_paths(x, data_root = data_root)
  if (any(!file.exists(paths))) {
    stop(
      "cannot refresh metadata for missing TAF file: ",
      paths[!file.exists(paths)][[1L]],
      call. = FALSE
    )
  }
  x$size_bytes <- unname(file.info(paths)$size)
  x$sha256 <- vapply(paths, taf_file_sha256, character(1))
  taf_assert_manifest_schema(x, year = year, version = version)
  x
}

taf_installed_counties <- function(x, year, version) {
  if (nrow(x) == 0L) {
    return(character())
  }
  paths <- taf_manifest_file_paths(
    x,
    data_root = taf_dataset_path(year = year, version = version)
  )
  present <- file.exists(paths)
  by_county <- split(present, x$county_fips)
  names(by_county)[vapply(by_county, all, logical(1))]
}

taf_file_sha256 <- function(path) {
  digest::digest(
    algo = "sha256",
    serialize = FALSE,
    file = path
  )
}

taf_write_catalog <- function(x, year, version, root = ".") {
  catalog_path <- taf_catalog_source_path(
    year = year,
    version = version,
    root = root
  )
  dir.create(dirname(catalog_path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(
    pattern = "county_zip_catalog_",
    tmpdir = dirname(catalog_path),
    fileext = ".parquet"
  )
  on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
  nanoparquet::write_parquet(taf_catalog_rows(x), tmp_path)
  if (file.exists(catalog_path)) {
    unlink(catalog_path, force = TRUE)
  }
  if (!file.rename(tmp_path, catalog_path)) {
    file.copy(tmp_path, catalog_path, overwrite = TRUE)
  }
  invisible(catalog_path)
}

taf_catalog_rows <- function(x) {
  out <- x[
    c("county_fips", "ZIP", "zip3", "zip2", "n_ranges")
  ]
  out <- unique(out)
  out <- out[order(out$ZIP, out$county_fips), , drop = FALSE]
  row.names(out) <- NULL
  tibble::as_tibble(out)
}

taf_needed_zipcodes <- function(
  x,
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap")
) {
  if (inherits(x, "addr")) {
    x <- x@place@zipcode
  }
  zipcodes <- addr_place(zipcode = x)@zipcode
  zipcodes <- unique(zipcodes[!is.na(zipcodes) & zipcodes != ""])
  if (length(zipcodes) == 0L) {
    return(tibble::tibble(
      source_zip = character(),
      source_zip_variant = character(),
      source_zip_variant_rank = integer(),
      ZIP = character()
    ))
  }

  exact <- tibble::tibble(
    source_zip = zipcodes,
    source_zip_variant = "exact",
    source_zip_variant_rank = 0L,
    ZIP = zipcodes
  )
  if (!zip_variants) {
    return(exact)
  }

  variants <- lapply(zipcodes, function(zip) {
    do.call(
      vctrs::vec_rbind,
      lapply(seq_along(zip_variant), function(i) {
        variant <- zip_variant[[i]]
        ZIP <- zipcode_variant(zip, variant = variant)
        if (length(ZIP) == 0L) {
          return(tibble::tibble(
            source_zip = character(),
            source_zip_variant = character(),
            source_zip_variant_rank = integer(),
            ZIP = character()
          ))
        }
        tibble::tibble(
          source_zip = zip,
          source_zip_variant = variant,
          source_zip_variant_rank = i,
          ZIP = ZIP
        )
      })
    )
  }) |>
    do.call(what = vctrs::vec_rbind)

  unique(vctrs::vec_rbind(exact, variants))
}

taf_empty_needed_counties <- function() {
  tibble::tibble(
    county_fips = character(),
    ZIP = character(),
    zip3 = character(),
    zip2 = character(),
    n_ranges = integer(),
    source_zip = character(),
    source_zip_variant = character()
  )
}

taf_missing_counties <- function(
  x,
  year,
  version,
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  place_zip_variants = TRUE,
  place_zip_variant = c("place", "county-sub")
) {
  needed <- taf_needed_counties(
    x,
    year = year,
    version = version,
    zip_variants = zip_variants,
    zip_variant = zip_variant,
    place_zip_variants = place_zip_variants,
    place_zip_variant = place_zip_variant
  )
  if (nrow(needed) == 0L) {
    return(needed)
  }

  manifest <- taf_read_county_zip_manifest(year = year, version = version)
  installed_counties <- taf_installed_counties(
    manifest,
    year = year,
    version = version
  )
  missing_counties <- setdiff(
    unique(needed$county_fips),
    installed_counties
  )
  needed[needed$county_fips %in% missing_counties, , drop = FALSE]
}

taf_warn_missing_counties <- function(missing) {
  if (nrow(missing) == 0L) {
    return(invisible(missing))
  }

  counties <- unique(missing$county_fips)
  zip_variants <- unique(paste0(
    missing$ZIP,
    " (",
    missing$source_zip_variant,
    " from ",
    missing$source_zip,
    ")"
  ))
  warning(
    "TAF files are missing for ",
    length(counties),
    " county/counties needed for geocoding; proceeding with installed files ",
    "only because taf_install = FALSE. Missing counties: ",
    taf_collapse_for_message(counties),
    ". Affected ZIPs: ",
    taf_collapse_for_message(zip_variants),
    ".",
    call. = FALSE
  )
  invisible(missing)
}

taf_collapse_for_message <- function(x, n = 8L) {
  if (length(x) <= n) {
    return(paste(x, collapse = ", "))
  }
  paste0(paste(x[seq_len(n)], collapse = ", "), ", ...")
}

taf_county_zip_manifest_rows <- function(x, county, year, version) {
  n_ranges <- stats::aggregate(
    rep.int(1L, nrow(x)),
    by = list(
      ZIP = x$ZIP,
      zip3 = x$zip3,
      zip2 = x$zip2
    ),
    FUN = sum
  )
  names(n_ranges)[names(n_ranges) == "x"] <- "n_ranges"
  n_ranges$county_fips <- county
  paths <- taf_manifest_file_paths(
    n_ranges,
    data_root = taf_dataset_path(year = year, version = version)
  )
  if (any(!file.exists(paths))) {
    stop(
      "cannot inventory missing TAF county file: ",
      paths[!file.exists(paths)][[1L]],
      call. = FALSE
    )
  }
  n_ranges$size_bytes <- unname(file.info(paths)$size)
  n_ranges$sha256 <- vapply(paths, taf_file_sha256, character(1))
  n_ranges$taf_year <- year
  n_ranges$taf_version <- version
  n_ranges$installed_at <- format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  )
  n_ranges <- n_ranges[
    taf_manifest_required_columns()
  ]
  tibble::as_tibble(n_ranges)
}

taf_write_county_parquet <- function(x, path, county, ZIP) {
  taf_assert_storage_schema(x, county = county, ZIP = ZIP, path = path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(
    pattern = paste0(".", tools::file_path_sans_ext(basename(path)), "-"),
    tmpdir = dirname(path),
    fileext = ".parquet"
  )
  on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
  nanoparquet::write_parquet(x, tmp_path, compression = "snappy")
  written <- tryCatch(
    nanoparquet::read_parquet(tmp_path) |> tibble::as_tibble(),
    error = identity
  )
  if (inherits(written, "error")) {
    stop("temporary TAF county Parquet file is unreadable", call. = FALSE)
  }
  taf_assert_storage_schema(
    written,
    county = county,
    ZIP = ZIP,
    path = tmp_path
  )
  if (nrow(written) != nrow(x)) {
    stop("temporary TAF county Parquet row count mismatch", call. = FALSE)
  }
  taf_atomic_replace(tmp_path, path)
  invisible(path)
}

taf_storage_required_columns <- function() {
  names(taf_empty_zip_tibble())
}

taf_assert_storage_schema <- function(x, county, ZIP, path = "TAF data") {
  required <- taf_storage_required_columns()
  if (
    !is.data.frame(x) ||
      length(names(x)) != length(required) ||
      !setequal(names(x), required)
  ) {
    stop(
      "TAF county Parquet data must contain exactly: ",
      paste(required, collapse = ", "),
      "; file: ",
      path,
      call. = FALSE
    )
  }
  if (nrow(x) == 0L) {
    stop("TAF county Parquet data must not be empty: ", path, call. = FALSE)
  }
  if (any(is.na(x$county_fips)) || any(x$county_fips != county)) {
    stop("TAF county Parquet contains the wrong county: ", path, call. = FALSE)
  }
  if (any(is.na(x$ZIP)) || any(x$ZIP != ZIP)) {
    stop("TAF county Parquet contains the wrong ZIP: ", path, call. = FALSE)
  }
  invisible(TRUE)
}

taf_atomic_replace <- function(tmp_path, path) {
  if (isTRUE(suppressWarnings(file.rename(tmp_path, path)))) {
    return(invisible(path))
  }
  if (!file.exists(path)) {
    stop("could not atomically install TAF file: ", path, call. = FALSE)
  }

  backup_path <- tempfile(
    pattern = paste0(".", basename(path), "-backup-"),
    tmpdir = dirname(path)
  )
  if (!isTRUE(suppressWarnings(file.rename(path, backup_path)))) {
    stop("could not prepare TAF file for replacement: ", path, call. = FALSE)
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
    stop("could not atomically replace TAF file: ", path, call. = FALSE)
  }
  replaced <- TRUE
  unlink(backup_path, force = TRUE)
  invisible(path)
}

taf_delete_county_zip_files <- function(x, year, version) {
  if (nrow(x) == 0L) {
    return(invisible(x))
  }
  file_paths <- file.path(
    taf_dataset_path(year = year, version = version),
    sprintf("zip3=%s", x$zip3),
    sprintf("zip2=%s", x$zip2),
    sprintf("%s.parquet", x$county_fips)
  )
  unlink(unique(file_paths), force = TRUE)
  invisible(file_paths)
}

taf_empty_zip_tibble <- function() {
  tibble::tibble(
    LINEARID = character(),
    FULLNAME = character(),
    side = character(),
    ZIP = character(),
    FROMHN = integer(),
    TOHN = integer(),
    PARITY = character(),
    OFFSET = numeric(),
    geometry_wkt = character(),
    street_predirectional = character(),
    street_premodifier = character(),
    street_pretype = character(),
    street_name = character(),
    street_posttype = character(),
    street_postdirectional = character(),
    street_tag_parsed = logical(),
    county_fips = character()
  )
}
