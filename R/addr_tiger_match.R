#' Match an addr vector to TIGER street ranges
#' @param x an addr vector to match
#' @param county character string of county identifier
#' @param year year of tigris product
#' @param max_dist_street_name maximum OSA distance to consider a match for the addr street_name
#' @param max_dist_street_type maximum OSA distance to consider a match for the addr street_type
#' @param street_only_match for addresses that match a TIGER street name, but have street numbers that don't
#' intersect with ranges of potential street numbers, return `"none"`, `"all"`, or the `"closest"` range geographies
#' @param summarize optionally summarize matched street ranges as their union or centroid
#' @return a list of matched tigris street range tibbles;
#' a NULL value indicates that no street name was matched; if `street_only_match` is FALSE,
#' a street range tibble with zero rows indicates that although a street was matched,
#' there was no range containing the street number
#' @export
#' @details
#' To best parse street names and types, this function appends dummy address components just
#' for the purposes of matching tiger street range names (e.g., `1234 {tiger_street_name} Anytown AB 00000`).
#'
#' TIGER street range files are saved to the R user cache directory for the addr package. This allows
#' R sessions to reuse previously downloaded files. See `?tools::R_user_dir()` to change where TIGER street range
#' files are saved.
#' @examples
#' my_addr <- as_addr(c("224 Woolper Ave", "3333 Burnet Ave", "33333 Burnet Ave", "609 Walnut St"))
#'
#' addr_match_tiger_street_ranges(my_addr, county = "39061", street_only_match = "all")
#'
#' addr_match_tiger_street_ranges(my_addr, county = "39061", summarize = "centroid")
#'
#' addr_match_tiger_street_ranges(my_addr,
#'   county = "39061",
#'   street_only_match = "closest", summarize = "centroid"
#' ) |>
#'   dplyr::bind_rows() |>
#'   dplyr::mutate(census_bg_id = s2_join_tiger_bg(s2::as_s2_cell(s2_geography)))
addr_match_tiger_street_ranges <- function(x,
                                           county = "39061",
                                           year = "2022",
                                           max_dist_street_name = 1,
                                           max_dist_street_type = 0,
                                           street_only_match = c("none", "all", "closest"),
                                           summarize = c("none", "union", "centroid")) {
  stopifnot(inherits(x, "addr"))
  street_only_match <- rlang::arg_match(street_only_match)
  summarize <- rlang::arg_match(summarize)
  ia <- unique(x)
  d_tiger <- tiger_street_ranges(county = county, year = year)
  tiger_addr <- as_addr(glue::glue("1234 {names(d_tiger)} Anytown AB 00000"))
  names(d_tiger) <- as.character(tiger_addr)

  street_matches <-
    addr_match_line_one(
      ia,
      tiger_addr,
      max_dist_street_number = NULL,
      max_dist_street_name = max_dist_street_name,
      max_dist_street_type = max_dist_street_type
    ) |>
    purrr::map(as.character)
  
  no_match_street <- which(purrr::map_int(street_matches, length) == 0)
  ia[no_match_street] <- NA
  ia <- stats::na.omit(ia)
  street_matches[no_match_street] <- NULL
  stopifnot(length(ia) == length(street_matches))

  street_matches <- purrr::map(street_matches, ~ d_tiger[[.x]])

  output <-
    purrr::map2(
      vctrs::field(ia, "street_number"), street_matches,
      \(.sn, .sm) {
        out <- dplyr::filter(.sm, from <= .sn, to >= .sn)
        if (nrow(out) == 0) {
          if (street_only_match == "none") {
            return(out)
          }
          if (street_only_match == "all") {
            return(.sm)
          }
          if (street_only_match == "closest") {
            return(.sm[unique(which.min(abs(.sm$from - .sn)), which.min(abs(.sm$to - .sn))), ])
          }
        }
        return(out)
      }
    ) |>
    stats::setNames(as.character(ia))

  out <- stats::setNames(output[as.character(x)], as.character(x))

  if (summarize == "none") {
    return(out)
  }
  if (summarize %in% c("union", "centroid")) {
    out <- purrr::map(out, \(.) summarize_street_range_tibble(., method = summarize), .progress = "summarizing street ranges")
  }
  return(out)
}

summarize_street_range_tibble <- function(x, method = c("union", "centroid")) {
  method <- rlang::arg_match(method)
  if (length(x) == 0) {
    return(x)
  }
  if (nrow(x) == 0) {
    return(x)
  }
  out <- tibble::tibble(
    TLID = paste(x$TLID, collapse = "-"),
    s2_geography = s2::s2_union_agg(x$s2_geography),
    from = min(x$from, na.rm = TRUE),
    to = max(x$to, na.rm = TRUE)
  )
  if (method == "centroid") {
    out$s2_geography <- s2::s2_centroid_agg(out$s2_geography)
  }
  return(out)
}

utils::globalVariables(c("from", "to", "geometry", "FULLNAME", "LFROMHN", "LTOHN", "RFROMHN", "RTOHN", "TLID"))
