tiger_download <- function(x) {
  stopifnot(
    "x must be a character vector" = is.character(x),
    "x must be length one" = length(x) == 1L,
    "x must not be missing" = !is.na(x)
  )
  tiger_url <- paste0("ftp://ftp2.census.gov/geo/tiger/", x)
  dest <- file.path(tools::R_user_dir("addr", "data"), x)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(dest)) {
    tf <- tempfile()
    utils::download.file(tiger_url, tf)
    file.copy(tf, dest)
  }
  return(dest)
}

#' Get names for tiger street ranges
#'
#' @description
#'
#' TIGER primary feature names are read from compressed featnames databases for
#' each county and census vintage.
#' If not already present, compressed addrfeat shapefiles are downloaded
#' from the census.gov FTP site to the R user's data directory for
#' the addr package.
#'
#' When reading into R, the data is filtered to addressable MTFCCs
#' (S1100, S1200, S1400, S1640) that have a name.
#' @inheritParams tiger_addr_feat
#' @returns a tibble with unique `LINEARID` and `addr` columns
#' @export
#' @examples
#' tiger_feat_names("39061", "2025")
tiger_feat_names <- function(county, year) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year)
  )
  check_installed("sf", "to read TIGER feature names")
  tp <- tiger_download(sprintf(
    "TIGER%s/FEATNAMES/tl_%s_%s_featnames.zip",
    year,
    year,
    county
  )) |>
    paste0("/vsizip/", file_path = _)
  rd <- sf::st_read(
    tp,
    quiet = TRUE,
    stringsAsFactors = FALSE,
    as_tibble = TRUE
  )
  rd <- rd[rd$MTFCC %in% c("S1100", "S1200", "S1400", "S1630", "S1640"), ]
  rd <- rd[!is.na(rd$FULLNAME), ]
  rd <- rd[rd$PAFLAG == "P", ]
  na_to_empty <- \(x) ifelse(is.na(x), "", x)
  d <- tibble::tibble(
    LINEARID = rd$LINEARID,
    premodifier = na_to_empty(rd$PREQUALABR),
    predirectional = na_to_empty(rd$PREDIRABRV),
    pretype = na_to_empty(rd$PRETYPABRV),
    name = na_to_empty(rd$NAME),
    posttype = na_to_empty(rd$SUFTYPABRV),
    postdirectional = na_to_empty(rd$SUFDIRABRV),
  ) |>
    unique()
  d$addr_street <- addr::addr_street(
    premodifier = d$premodifier,
    predirectional = d$predirectional,
    pretype = d$pretype,
    name = d$name,
    posttype = d$posttype,
    postdirectional = d$postdirectional,
    map_posttype = FALSE,
    map_directional = FALSE,
    map_pretype = FALSE,
    map_ordinal = FALSE
  )
  d$premodifier <- NULL
  d$predirectional <- NULL
  d$pretype <- NULL
  d$name <- NULL
  d$posttype <- NULL
  d$postdirectional <- NULL
  d
}

#' Get s2_geography for tiger street ranges
#'
#' @description
#'
#' TIGER address features (i.e. street address ranges) are read from compressed
#' addrfeat shapefiles for each county and Census vintage.
#' If not already present, compressed addrfeat shapefiles are downloaded
#' from the census.gov FTP site to the R user's data directory for
#' the addr package.
#'
#' When reading into R, the data is converted to a "long" format by street
#' side L/R to be used within `taf_install()` to create `taf()`.
#' @param county character string of county identifier
#' @param year character year of tigris product
#' @returns a tibble with `LINEARID`, `FULLNAME`, `side`, `ZIP`,
#' `FROMHN`, `TOHN`, `PARITY`, `OFFSET`, and `s2_geography` columns
#' @export
#' @examples
#' tiger_addr_feat("39061", "2025")
tiger_addr_feat <- function(county, year) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year)
  )
  check_installed("sf", "to read address range shapefiles")

  tp <- tiger_download(sprintf(
    "TIGER%s/ADDRFEAT/tl_%s_%s_addrfeat.zip",
    year,
    year,
    county
  )) |>
    paste0("/vsizip/", file_path = _)

  rd <-
    sf::st_read(
      tp,
      query = sprintf(
        "SELECT LINEARID, FULLNAME, ZIPL, ZIPR, LFROMHN, LTOHN, RFROMHN, RTOHN, PARITYL, PARITYR, OFFSETL, OFFSETR FROM tl_%s_%s_addrfeat",
        year,
        county
      ),
      quiet = TRUE,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

  out <- pivot_addrfeat_sides(rd) |>
    stats::na.omit()

  out$FROMHN <- to_int(out$FROMHN)
  out$TOHN <- to_int(out$TOHN)

  out$s2_geography <- s2::as_s2_geography(out$geometry)
  sf::st_drop_geometry(out)
}

pivot_addrfeat_sides <- function(x) {
  side_cols <- c(
    "ZIPL",
    "ZIPR",
    "LFROMHN",
    "LTOHN",
    "RFROMHN",
    "RTOHN",
    "PARITYL",
    "PARITYR",
    "OFFSETL",
    "OFFSETR"
  )
  base_cols <- setdiff(names(x), side_cols)
  left <- x[base_cols]
  left$side <- "L"
  left$ZIP <- x$ZIPL
  left$FROMHN <- x$LFROMHN
  left$TOHN <- x$LTOHN
  left$PARITY <- x$PARITYL
  left$OFFSET <- x$OFFSETL
  right <- x[base_cols]
  right$side <- "R"
  right$ZIP <- x$ZIPR
  right$FROMHN <- x$RFROMHN
  right$TOHN <- x$RTOHN
  right$PARITY <- x$PARITYR
  right$OFFSET <- x$OFFSETR
  rbind(left, right)
}


#' TIGER Address Features dataset
#'
#' taf() uses the arrow package to open the
#' hive partitioned, parquet dataset of TIGER address features in the R user
#' directory for the addr package.
#' The FileSystemDataset in Arrow is database-like backend for
#' larger-than-memory datasets and provides functionality
#' to use dplyr syntax for data manipulation;
#' see <https://arrow.apache.org/docs/r/articles/data_wrangling.html> for
#' more details on arrow and dplyr
#' @param year integer, length one; vintage of TIGER addrfeat files
#' @param version character, length one; major version of the package
#' and taf dataset schema
#' @returns a Dataset R6 object (see `?arrow::open_dataset`); use `dplyr`
#' verbs to query the data and get results, see examples
#' @export
#' @examples
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
  stopifnot(
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  year <- match.arg(year)
  taf_path <- file.path(
    tools::R_user_dir("addr", "data"),
    version,
    "tiger_addr_feat",
    year
  )
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

#' @rdname taf
#' @param county character, length 1; county fips code
#' @export
#' @examples
#' taf_install("39061", "2025")
taf_install <- function(
  county,
  year = as.character(2025:2011),
  version = "v1"
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year)
  )
  year <- match.arg(year)
  stopifnot(
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  d_names <- tiger_feat_names(county = county, year = year)
  d_geom <- tiger_addr_feat(county = county, year = year)

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
      tools::R_user_dir("addr", "data"),
      version,
      "tiger_addr_feat",
      year,
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
  return(invisible(county))
}

#' read taf() for a ZIP code across all installed counties
#'
#' taf_zip() is a helper that transforms data read in from taf()
#' for a subset of zip codes to use with the addr package.
#' It reconstructs the `county_fips`, `s2_geography`, and `addr_street`,
#' vectors in the returned data frame to use with the addr package.
#' @param map logical, length 1; map street tags read from taf() data
#' (type, directional, ordinal) when converting to `addr_street()` vector?
#' @returns a tibble with `LINEARID`, `FULLNAME`, `side`, `ZIP`,
#' `FROMHN`, `TOHN`, `PARITY`, `OFFSET`, `s2_geography`, `addr_street`,
#' and `county_fips` columns
#' @examples
#' taf_zip(c("45249", "45230", "45220"))
taf_zip <- function(x, map = TRUE) {
  stopifnot(is.character(x), length(x) > 0, !any(is.na(x)))
  x <- unique(addr::addr_place(zipcode = x)@zipcode)
  filter_expr <- lapply(x, function(z) {
    (arrow::Expression$field_ref("zip3") == substr(z, 1, 3)) &
      (arrow::Expression$field_ref("zip2") == substr(z, 4, 5))
  }) |>
    Reduce(`|`, x = _)
  d <- taf()$NewScan()$Filter(filter_expr)$Finish()$ToTable() |>
    tibble::as_tibble()
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
