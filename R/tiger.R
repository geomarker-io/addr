#' download tiger files
#' @param x filename relative to ftp://ftp2.census.gov/geo/tiger/
#' @keywords internal
tiger_download <- function(x) {
  tiger_url <- paste0("ftp://ftp2.census.gov/geo/tiger/", x)
  dest <- file.path(tools::R_user_dir("addr", "cache"), x)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)

  if (!file.exists(dest)) {
    tf <- tempfile()
    utils::download.file(tiger_url, tf)
    file.copy(tf, dest)
  }

  return(dest)
}

#' get s2_geography for census block groups
#' @param state census FIPS state identifier
#' @param year vintage of TIGER/Line block group geography files
#' @returns a tibble with `GEOID` and `s2_geography` columns
#' @keywords internal
tiger_block_groups <- function(state, year) {
  dest <- tiger_download(glue::glue("TIGER{year}/BG/tl_{year}_{state}_bg.zip"))
  out <-
    sf::st_read(
      paste0("/vsizip/", dest),
      as_tibble = TRUE,
      quiet = TRUE,
      query = glue::glue("SELECT GEOID FROM tl_{year}_{state}_bg")
    )
  out$s2_geography <- s2::as_s2_geography(out$geometry)
  out <- sf::st_drop_geometry(out)
  return(out)
}

#' Get s2_geography for census states
#' @param year vintage of TIGER/Line block group geography files
#' @returns a tibble with `GEOID` and `s2_geography` columns
#' @keywords internal
tiger_states <- function(year) {
  dest <- tiger_download(glue::glue("TIGER{year}/STATE/tl_{year}_us_state.zip"))
  out <-
    sf::st_read(
      paste0("/vsizip/", dest),
      as_tibble = TRUE,
      quiet = TRUE,
      query = glue::glue("SELECT GEOID FROM tl_{year}_us_state")
    )
  out$s2_geography <- s2::as_s2_geography(out$geometry)
  out <- sf::st_drop_geometry(out)
  return(out)
}

#' Get s2_geography for tiger street ranges
#'
#' Street ranges with missing minimum or maximum address numbers are excluded.
#' @param county character string of county identifier
#' @param year year of tigris product
#' @returns a list of tibbles, one for each street name, with `TLID`, `s2_geography`, `from`, and `to` columns
#' @keywords internal
tiger_street_ranges <- function(county, year = "2022") {
  stopifnot(year == "2022")
  dest_path <- tiger_download(glue::glue("TIGER2022/ADDRFEAT/tl_2022_{county}_addrfeat.zip"))
  sf::st_read(
    dsn = paste0("/vsizip/", dest_path),
    query = "SELECT TLID, FULLNAME, LFROMHN, LTOHN, RFROMHN, RTOHN FROM tl_2022_39061_addrfeat",
    quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE
  ) |>
    dplyr::mutate(
      dplyr::across(dplyr::ends_with("HN"), as.numeric),
      TLID = as.character(TLID),
      s2_geography = s2::as_s2_geography(geometry)
    ) |>
    sf::st_drop_geometry() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      from = min(LFROMHN, LTOHN, RFROMHN, RTOHN, na.rm = TRUE),
      to = max(LFROMHN, LTOHN, RFROMHN, RTOHN, na.rm = TRUE),
      .keep = "unused"
    ) |>
    dplyr::filter(from < Inf & to > -Inf) |>
    suppressWarnings() |>
    dplyr::ungroup() |>
    dplyr::nest_by(FULLNAME, .key = "data") |>
    dplyr::ungroup() |>
    tibble::deframe()
}
