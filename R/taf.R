#' TIGER Address Features dataset
#'
#' @description
#'
#' `taf()` uses the arrow package to open the hive-partitioned parquet dataset
#' of TIGER address features in the addr user data directory.
#' Arrow `FileSystemDataset` objects are database-like backends for
#' larger-than-memory datasets and support dplyr syntax for data manipulation;
#' see <https://arrow.apache.org/docs/r/articles/data_wrangling.html>.
#'
#' `taf_install()` downloads and links TIGER address features and
#' feature names for a specific year and county, installing the resulting
#' file in the addr user data directory.
#' If an address feature does not have a corresponding LINEARID with a
#' feature name, then the street tags are parsed from the full name, in
#' which case the column, `street_tag_parsed` will be TRUE.
#'
#' @param year integer, length one; vintage of TIGER addrfeat files
#' @param version character, length one; major version of the package
#' and taf dataset schema
#' @returns a Dataset R6 object (see `?arrow::open_dataset`); use `dplyr`
#' verbs to query the data and get results, see examples
#' @export
#' @examples
#' Sys.setenv("R_USER_DATA_DIR" = tempfile())
#' taf_install("39061", "2025")
#'
#' taf()
#'
#' # use dplyr verbs to query
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # find top ten most frequent street name-posttype combinations
#' taf() |>
#'   group_by(street_name, street_posttype) |>
#'   summarize(
#'     n_zips = n_distinct(ZIP),
#'     n_ranges = n(),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(n_zips), desc(n_ranges)) |>
#'   collect() |>
#'   slice(1:10)
taf <- function(year = as.character(2025:2011), version = "v1") {
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
taf_catalog <- function(year = as.character(2025:2011), version = "v1") {
  check_installed("arrow", "to read the taf ZIP/county catalog")
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
  arrow::read_parquet(catalog_path) |>
    tibble::as_tibble()
}

#' @rdname taf
#' @param county character, length 1; county FIPS code
#' @param overwrite logical, length 1; overwrite an existing county install?
#' @param redownload logical, length 1; re-download cached TIGER ZIP files?
#' @export
#' @examples
#' Sys.setenv("R_USER_DATA_DIR" = tempfile())
#' taf_install("39061", "2025")
taf_install <- function(
  county,
  year = as.character(2025:2011),
  version = "v1",
  overwrite = FALSE,
  redownload = FALSE
) {
  check_installed("arrow", "to write to the multi-file taf dataset")
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
  if (nrow(county_manifest) > 0L && !overwrite) {
    return(invisible(county))
  }
  if (nrow(county_manifest) > 0L && overwrite) {
    taf_delete_county_zip_files(county_manifest, year = year, version = version)
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
    warning(
      "structured street name info not found for ",
      length(lid_no_name),
      " ranges"
    )
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
    if (file.exists(out_file)) {
      file.remove(out_file)
    }
    arrow::write_parquet(out_part, out_file)
  }
  manifest <- taf_read_county_zip_manifest(year = year, version = version)
  manifest <- manifest[manifest$county_fips != county, , drop = FALSE]
  manifest_add <- taf_county_zip_manifest_rows(out, county = county)
  manifest <- vctrs::vec_rbind(manifest, manifest_add)
  taf_write_county_zip_manifest(manifest, year = year, version = version)
  return(invisible(county))
}

#' Read taf() data for ZIP codes across all installed counties
#'
#' `taf_zip()` reads and transforms `taf()` data for a subset of ZIP codes.
#' It reconstructs the `county_fips`, `s2_geography`, and `addr_street`
#' vectors in the returned data frame.
#' @param x character vector of five-digit ZIP codes
#' @param map logical, length 1; map street tags read from taf() data
#' (type, directional, ordinal) when converting to `addr_street()` vector?
#' @param year character, length 1; vintage of TIGER addrfeat files
#' @param version character, length 1; major version of the package
#' and taf dataset schema
#' @returns a tibble with `LINEARID`, `FULLNAME`, `side`, `ZIP`,
#' `FROMHN`, `TOHN`, `PARITY`, `OFFSET`, `s2_geography`, `addr_street`,
#' `county_fips`, and `street_tag_parsed` columns
#' @export
#' @examples
#' Sys.setenv("R_USER_DATA_DIR" = tempfile())
#' taf_install("39061", "2025")
#' taf_zip(c("45249", "45230", "45220"))
taf_zip <- function(
  x,
  map = TRUE,
  year = as.character(2025:2011),
  version = "v1"
) {
  check_installed("arrow", "to read from the multi-file taf dataset")
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
        arrow::read_parquet(path) |>
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
    return(tibble::tibble(
      county_fips = character(),
      ZIP = character(),
      zip3 = character(),
      zip2 = character(),
      n_ranges = integer(),
      installed_at = character()
    ))
  }
  arrow::read_parquet(manifest_path) |>
    tibble::as_tibble()
}

taf_write_county_zip_manifest <- function(x, year, version) {
  manifest_path <- taf_county_zip_manifest_path(year = year, version = version)
  dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- tempfile(
    pattern = "county_zip_",
    tmpdir = dirname(manifest_path),
    fileext = ".parquet"
  )
  on.exit(unlink(tmp_path, force = TRUE), add = TRUE)
  arrow::write_parquet(x, tmp_path)
  if (file.exists(manifest_path)) {
    unlink(manifest_path, force = TRUE)
  }
  if (!file.rename(tmp_path, manifest_path)) {
    file.copy(tmp_path, manifest_path, overwrite = TRUE)
  }
  invisible(manifest_path)
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
  arrow::write_parquet(taf_catalog_rows(x), tmp_path)
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

taf_county_zip_manifest_rows <- function(x, county) {
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
  n_ranges$installed_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  n_ranges <- n_ranges[
    c("county_fips", "ZIP", "zip3", "zip2", "n_ranges", "installed_at")
  ]
  tibble::as_tibble(n_ranges)
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
