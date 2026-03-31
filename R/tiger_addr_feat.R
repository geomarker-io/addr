tiger_download <- function(x) {
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
#' side L/R to be used with `tiger_geocode()`.
#' @param county character string of county identifier
#' @param year character year of tigris product
#' @returns a tibble with `LINEARID`, `FULLNAME`, `side`, `ZIP`,
#' `FROMHN`, `TOHN`, `PARITY`, `OFFSET`, and `s2_geography` columns
#' @export
#' @examples
#' tiger_addr_feat("39061", "2024")
tiger_addr_feat <- function(county, year) {
  # TODO validate county and year args
  check_installed("sf", "to read address range shapefiles")
  rd <- tiger_download(sprintf(
    "TIGER%s/ADDRFEAT/tl_%s_%s_addrfeat.zip",
    year,
    year,
    county
  )) |>
    paste0("/vsizip/", file_path = _) |>
    sf::st_read(
      dsn = _,
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

  out$ZIP <- to_int(out$ZIP)
  out$FROMHN <- to_int(out$FROMHN)
  out$TOHN <- to_int(out$TOHN)

  out$s2_geography <- s2::as_s2_geography(out$geometry)
  sf::st_drop_geometry(out)
}
