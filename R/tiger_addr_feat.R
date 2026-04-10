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
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year)
  )
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

  out$FROMHN <- to_int(out$FROMHN)
  out$TOHN <- to_int(out$TOHN)

  out$s2_geography <- s2::as_s2_geography(out$geometry)
  sf::st_drop_geometry(out)
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
      state = arrow::string(),
      county = arrow::string()
    )
  )
}

#' @param county_fips character, length 1; county fips code
#' @rdname taf
#' @export
#' @examples
#' taf_install("39061", "2025")
taf_install <- function(county_fips, year, version = "v1") {
  taf_path <- file.path(
    tools::R_user_dir("addr", "data"),
    version,
    "tiger_addr_feat",
    year,
    sprintf("state=%s", substr(county_fips, 1, 2)),
    sprintf("county=%s", substr(county_fips, 3, 5))
  )
  dir.create(taf_path, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(taf_path, "part-0.parquet")
  if (!file.exists(out_file)) {
    d <- tiger_addr_feat(county_fips, year)
    d_addr <-
      as_addr(
        paste(
          "3",
          d$FULLNAME,
          "Springfield",
          "OH",
          d$ZIP
        ),
        clean = FALSE,
        map_posttype = FALSE,
        map_directional = FALSE,
        map_pretype = FALSE,
        map_ordinal = FALSE,
        map_state = FALSE
      )
    d$addr_street <- d_addr@street
    out <- tibble::tibble(d, tibble::as_tibble(d$addr_street))
    out$addr_street <- NULL
    out$geometry_wkt <- s2::s2_as_text(out$s2_geography)
    out$s2_geography <- NULL
    arrow::write_parquet(out, out_file)
  }
  return(invisible(county_fips))
}

# read taf() for a ZIP code across all installed counties
# transform data read in from taf()
# - reconstruct county_fips
# - reconstruct s2_geography
# - reconstruct addr_street, mapping all tags
#   (ensures consistency with loaded version of package)
#' taf_zip(c("45249", "45230", "45220"))
taf_zip <- function(x) {
  stopifnot(is.character(x), length(x) > 0, !any(is.na(x)))
  x <- addr::addr_place(zipcode = x)@zipcode
  filter_expr <- lapply(x, function(z) {
    arrow::Expression$field_ref("ZIP") == z
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
    map_posttype = TRUE,
    map_directional = TRUE,
    map_pretype = TRUE,
    map_ordinal = TRUE
  )
  d$street_predirectional <- NULL
  d$street_premodifier <- NULL
  d$street_pretype <- NULL
  d$street_name <- NULL
  d$street_posttype <- NULL
  d$street_postdirectional <- NULL
  d$county_fips <- paste0(d$state, d$county)
  d$state <- NULL
  d$county <- NULL
  d
}
