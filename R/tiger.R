tiger_download <- function(x, overwrite = FALSE) {
  stopifnot(
    "x must be a character vector" = is.character(x),
    "x must be length one" = length(x) == 1L,
    "x must not be missing" = !is.na(x),
    "overwrite must be logical" = is.logical(overwrite),
    "overwrite must be length one" = length(overwrite) == 1L,
    "overwrite must not be missing" = !is.na(overwrite)
  )
  tiger_url <- paste0("ftp://ftp2.census.gov/geo/tiger/", x)
  dest <- file.path(tools::R_user_dir("addr", "data"), x)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  if (overwrite || !file.exists(dest)) {
    tf <- tempfile()
    utils::download.file(tiger_url, tf)
    file.copy(tf, dest, overwrite = TRUE)
  }
  return(dest)
}

#' Get names for tiger street ranges
#'
#' @description
#'
#' TIGER primary feature names are read from compressed feature-name databases
#' for each county and Census vintage.
#' If not already present, compressed addrfeat (address feature) shapefiles are
#' downloaded from the Census FTP site to the addr user data directory.
#'
#' When reading into R, the data is filtered to addressable MTFCCs
#' (S1100, S1200, S1400, S1640) that have a name.
#' @inheritParams tiger_addr_feat
#' @param redownload logical, length 1; re-download the cached TIGER ZIP file?
#' @returns a tibble with unique `LINEARID` and `addr` columns
#' @export
#' @examples
#' tiger_feat_names("39061", "2025")
tiger_feat_names <- function(county, year, redownload = FALSE) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year),
    "redownload must be logical" = is.logical(redownload),
    "redownload must be length one" = length(redownload) == 1L,
    "redownload must not be missing" = !is.na(redownload)
  )
  check_installed("sf", "to read TIGER feature names")
  tp <- tiger_download(
    sprintf(
      "TIGER%s/FEATNAMES/tl_%s_%s_featnames.zip",
      year,
      year,
      county
    ),
    overwrite = redownload
  ) |>
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
#' TIGER address features (street address ranges) are read from compressed
#' addrfeat (address feature) shapefiles for each county and Census vintage.
#' If not already present, compressed addrfeat shapefiles are downloaded from
#' the Census FTP site to the addr user data directory.
#'
#' When reading into R, the data is converted to one row per street side
#' (`L`/`R`) for use by `taf_install()`.
#' @param county character string of county FIPS identifier
#' @param year character year of the Census TIGER/Line product
#' @param redownload logical, length 1; re-download the cached TIGER ZIP file?
#' @returns a tibble with `LINEARID`, `FULLNAME`, `side`, `ZIP`,
#' `FROMHN`, `TOHN`, `PARITY`, `OFFSET`, and `s2_geography` columns
#' @export
#' @examples
#' tiger_addr_feat("39061", "2025")
tiger_addr_feat <- function(county, year, redownload = FALSE) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year),
    "redownload must be logical" = is.logical(redownload),
    "redownload must be length one" = length(redownload) == 1L,
    "redownload must not be missing" = !is.na(redownload)
  )
  check_installed("sf", "to read address range shapefiles")

  tp <- tiger_download(
    sprintf(
      "TIGER%s/ADDRFEAT/tl_%s_%s_addrfeat.zip",
      year,
      year,
      county
    ),
    overwrite = redownload
  ) |>
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
