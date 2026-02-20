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

# TODO refactor tests so that these functions return preinstalled data so no actual downloads are triggered

#' Get s2_geography for tiger street ranges
#'
#' TIGER address features (i.e. street address ranges) are downloaded from the census ftp site
#' @param county character string of county identifier
#' @param year year of tigris product
#' @returns a list of tibbles, one for each street name, with `TLID`, `s2_geography`, `from`, and `to` columns
#' @export
#' @examples
#' \dontrun{
#'   tiger_addr_feat("39061", "2024")
#' }
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
      query = sprintf(
        "SELECT LINEARID, FULLNAME, ZIPL, ZIPR, LFROMHN, LTOHN, RFROMHN, RTOHN, PARITYL, PARITYR, OFFSETL, OFFSETR FROM tl_%s_%s_addrfeat",
        year,
        county
      ),
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
    stats::na.omit() |>
    dplyr::mutate(dplyr::across(c(ZIP, FROMHN, TOHN), to_int))
}
