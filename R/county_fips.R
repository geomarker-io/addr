normalize_county_name <- function(x) {
  x <- as_norm_char(x)
  x <- gsub("[.']", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

strip_county_equivalent_suffix <- function(x) {
  sub(
    "( County| Parish| City and Borough| Census Area| Planning Region| Borough| Municipality| Municipio| city)$",
    "",
    x
  )
}

#' Translate county names and county FIPS identifiers
#'
#' @description
#' `county_fips_lookup()` uses a package-internal reference derived from the
#' 2025 U.S. Census county adjacency file to translate between county names,
#' state abbreviations, and 5-digit county FIPS identifiers.
#'
#' Name lookups accept either the full county-equivalent label
#' (for example, `"Orleans Parish"`) or a shortened form with common suffixes
#' removed (for example, `"Orleans"`). If a shortened form is ambiguous within
#' a state, the function errors and asks for the full county-equivalent name
#' or the 5-digit FIPS identifier.
#'
#' @param county character, length one; either a county name or a 5-digit
#'   county FIPS identifier
#' @param state character, length one; state abbreviation or full state name;
#' required when `county` is a name, ignored when `county` is already a
#' 5-digit FIPS identifier
#' @returns A tibble with one row and columns `county`, `county_full`, `state`,
#'   and `county_fips`.
#' @export
#' @examples
#' county_fips_lookup("Hamilton", "OH")
#' county_fips_lookup("Hamilton", "Ohio")
#' county_fips_lookup("39061")
county_fips_lookup <- function(county, state = NULL) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county)
  )

  if (grepl("^[0-9]{5}$", county)) {
    hit <- county_fips_reference$county_fips == county
    if (!any(hit)) {
      stop(sprintf("county FIPS `%s` was not found", county), call. = FALSE)
    }
    return(
      tibble::as_tibble(
        county_fips_reference[
          hit,
          c("county", "county_full", "state", "county_fips")
        ]
      )
    )
  }

  stopifnot(
    "state must be a character vector" = is.character(state),
    "state must be length one" = length(state) == 1L,
    "state must not be missing" = !is.na(state)
  )

  state_abbrev <- map_state_to_abbrev(state)
  if (is.na(state_abbrev) || state_abbrev == "") {
    stop(sprintf("state `%s` was not recognized", state), call. = FALSE)
  }

  county_norm <- normalize_county_name(county)
  ref <- county_fips_reference[county_fips_reference$state == state_abbrev, ]

  hit_full <- ref$county_full_norm == county_norm
  if (sum(hit_full) == 1L) {
    return(
      tibble::as_tibble(
        ref[hit_full, c("county", "county_full", "state", "county_fips")]
      )
    )
  }

  hit_short <- ref$county_norm == county_norm
  if (sum(hit_short) == 1L) {
    return(
      tibble::as_tibble(
        ref[hit_short, c("county", "county_full", "state", "county_fips")]
      )
    )
  }

  if (sum(hit_short) > 1L) {
    stop(
      sprintf(
        "county `%s` is ambiguous in `%s`; use one of: %s, or a 5-digit FIPS",
        county,
        state_abbrev,
        paste(ref$county_full[hit_short], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  stop(
    sprintf(
      "county `%s` was not found in `%s`",
      county,
      state_abbrev
    ),
    call. = FALSE
  )
}
