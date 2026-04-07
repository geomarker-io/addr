#' Translate ZIP codes and county FIPS identifiers
#'
#' @description
#' `zip_fips_lookup()` uses a package-internal reference derived from the
#' 2025 Q4 HUD USPS ZIP Code Crosswalk Files to translate between
#' a ZIP code and county FIPS identifiers.
#' Some ZIP codes are linked to multiple counties; also specify
#' the county FIPS identifier to get the USPS preferred city and
#' state names (in an addr_place() vector).
#' @param zip character, length one; a 5-digit ZIP code
#' @param fips character, length one; a 5-digit FIPS county identifier
#' @returns if fips is NULL, a character vector of matched 5-digit FIPS
#' county identifiers, ordered by percentages of residential addresses;
#' if fips is not NULL, an addr_place vector of length one
#'
#' Potentially real ZIP codes that are not in the HUD crosswalk return an error.
#' A ZIP code can be a real, active USPS ZIP and still be absent from
#' the HUD crosswalk for a specific quarter because HUD builds the file from
#' quarterly ZIP+4 records rather than a complete USPS list, and excludes
#' records that cannot cannot be reliably geocoded to Census geography;
#' PO Box-only and some institutional or unique ZIP codes may therefore
#' be absent.
#' If a zip code is absent from the HUD crosswalk, an error is raised.
#' If a zip code is present, but not in the specified county, then
#' `@name` and `@state` (but not @zipcode) are set to missing in the returned
#' addr_place vector
#' @export
#' @examples
#'
#' # one zip to one county
#' zip_fips_lookup(zip = "45220")
#'
#' # one zip to > one county
#' zip_fips_lookup(zip = "45249")
#'
#' # use fips code to specify the place
#' zip_fips_lookup(zip = "45249", fips = "39061")
#'
#' # zip codes in wrong county will have NA place and state
#' zip_fips_lookup(zip = "45249", fips = "39017")
zip_fips_lookup <- function(zip, fips = NULL) {
  stopifnot(
    "zip must be a character vector" = is.character(zip),
    "zip must be length one" = length(zip) == 1L,
    "zip must not be missing" = !is.na(zip)
  )
  x <- addr_place(zipcode = zip, name = "", state = "")@zipcode
  m <- zip_code_reference[zip_code_reference$ZIP == x, ]
  if (nrow(m) == 0) {
    stop(
      sprintf("zipcode `%s` was not found in HUD crosswalk", x),
      call. = FALSE
    )
  }
  m <- m[order(m$RES_RATIO, decreasing = TRUE), ]
  if (is.null(fips)) {
    return(m$COUNTY)
  }
  stopifnot(
    "fips must be a character vector" = is.character(fips),
    "fips must be length one" = length(fips) == 1L,
    "fips must not be missing" = !is.na(fips),
    "fips must be 5 digits" = nchar(fips) == 5
  )
  m <- m[m$COUNTY == fips, ]
  if (nrow(m) == 0) {
    return(addr_place(
      zipcode = zip,
      name = NA_character_,
      state = NA_character_,
      map_state = FALSE
    ))
  }
  addr_place(
    zipcode = m$ZIP[1],
    name = m$USPS_ZIP_PREF_CITY[1],
    state = m$USPS_ZIP_PREF_STATE[1],
    map_state = TRUE
  )
}
