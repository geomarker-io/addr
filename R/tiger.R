#' download tiger files to the R user's data directory for the addr package
#' @param x filename relative to ftp://ftp2.census.gov/geo/tiger/
#' @keywords internal
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
#' TIGER address features (i.e. street address ranges) are downloaded
#' @param county character string of county identifier
#' @param year year of tigris product
#' @returns a list of tibbles, one for each street name, with `TLID`, `s2_geography`, `from`, and `to` columns
#' @keywords internal
tiger_addr_feat <- function(county, year) {
  tiger_download(sprintf(
    "TIGER%s/ADDRFEAT/tl_%s_%s_addrfeat.zip",
    year,
    year,
    county
  )) |>
    paste0("/vsizip/", file_path = _) |>
    sf::st_read(
      dsn = _,
      query = "SELECT LINEARID, FULLNAME, ZIPL, ZIPR, LFROMHN, LTOHN, RFROMHN, RTOHN, PARITYL, PARITYR, OFFSETL, OFFSETR FROM tl_2022_39061_addrfeat",
      quiet = TRUE,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    ) |>
    dplyr::rename(
      FROMHNL = LFROMHN,
      TOHNL = LTOHN,
      FROMHNR = RFROMHN,
      TOHNR = RTOHN
    ) |>
    tidyr::pivot_longer(
      cols = c(
        ZIPL,
        ZIPR,
        FROMHNL,
        TOHNL,
        FROMHNR,
        TOHNR,
        PARITYL,
        PARITYR,
        OFFSETL,
        OFFSETR
      ),
      names_to = c(".value", "side"),
      names_pattern = "(ZIP|FROMHN|TOHN|PARITY|OFFSET)(L|R)"
    ) |>
    na.omit() |>
    dplyr::mutate(dplyr::across(c(ZIP, FROMHN, TOHN), to_int))
}
