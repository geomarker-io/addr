#' Geocode addr vectors
#'
#' Addresses are attempted to be matched to reference geographies using different methods
#' associated with decreasing levels of precision in the order listed below.
#' Each method generates matched s2 cell identifiers differently
#' and is recorded in the `match_method` column of the returned tibble:  
#' 1. `ref_addr`: reference s2 cell from direct match to reference address
#' 2. `tiger_range`: centroid of street-matched TIGER address ranges containing street number
#' 3. `tiger_street`: centroid of street-matched TIGER address ranges closest to the street number
#' 4. `none`: unmatched using all previous approaches; return missing s2 cell identifier
#'
#' @param x an addr vector (or character vector of address strings) to geocode
#' @param ref_addr an addr vector to search for matches in
#' @param ref_s2 a s2_cell vector of locations for each ref_addr
#' @param county character county identifer for TIGER street range files to search for matches in
#' @param year character year for TIGER street range files to search for matches in
#' @returns a tibble with columns: `addr` contains `x` converted to an `addr` vector,
#' `s2` contains the resulting geocoded s2 cells as an `s2cell` vector,
#' `match_method` is a factor with levels described above
#' @export
#' @details
#'
#' Performance was compared to the degauss geocoder (see `/inst/compare_geocoding_to_degauss.R`) using
#' real-world addresses in `voter_addresses()`.
#' Match success rates were similar, but DeGAUSS matched about 5% more of the addresses. These differences are
#' sensitive to the match criteria considered for DeGAUSS (here precision of 'range' & score > 0.7 *or*
#' precision of 'street' & score > 0.55):
#'
#' |addr_matched |degauss_matched |      n|perc  |
#' |:------------|:---------------|------:|:-----|
#' |TRUE         |TRUE            | 224714|92.8% |
#' |FALSE        |TRUE            |  13407|5.5%  |
#' |FALSE        |FALSE           |   2993|1.2%  |
#' |TRUE         |FALSE           |   1019|0.4%  |
#'
#' Among those that were geocoded by both, 97.7% were geocoded to the same census tract, and
#' 96.6% to the same block group:
#'
#' |ct_agree |bg_agree |      n|s2_dist_ptiles (5th, 25th, 50th, 75th, 95th) |perc  |
#' |:--------|:--------|------:|:--------------------------------------------|:-----|
#' |TRUE     |TRUE     | 217179|14.7, 24.3, 39, 68.9, 153.6                  |96.6% |
#' |FALSE    |FALSE    |   4805|21.6, 39.2, 158.9, 5577.9, 16998.8           |2.1%  |
#' |TRUE     |FALSE    |   2730|19.6, 28.6, 41.2, 94.8, 571.8                |1.2%  |
#' @examples
#' set.seed(1)
#' cagis_s2 <-
#'   cagis_addr()$cagis_addr_data |>
#'   purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
#'   purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())
#' addr_match_geocode(x = sample(voter_addresses(), 100), ref_s2 = cagis_s2) |>
#'   print(n = 100)
addr_match_geocode <- function(x,
                               ref_addr = cagis_addr()$cagis_addr,
                               ref_s2,
                               county = "39061",
                               year = "2022") {
  x_addr <- as_addr(x)
  x_addr_ref_match <-
    addr_match(x_addr,
      ref_addr,
      max_dist_street_number = 0,
      max_dist_street_name = 1,
      max_dist_street_type = 0,
      simplify = TRUE
    )

  x_addr_ref_match_which <- match(x_addr_ref_match, ref_addr)

  x_s2 <-
    ref_s2[x_addr_ref_match_which] |>
    stats::setNames(x_addr)

  x_addr_ref_no_match_which <- which(is.na(x_s2))

  t_matches <-
    addr_match_tiger_street_ranges(
      x_addr[x_addr_ref_no_match_which],
      county = county,
      year = year,
      street_only_match = "none",
      summarize = "centroid"
    ) |>
    purrr::discard(\(.) length(.) < 1) |> # removes NULL
    purrr::discard(\(.) nrow(.) < 1) |> # removes empty data.frame
    purrr::map_vec(\(.) s2::as_s2_cell(.$s2_geography), .ptype = s2::s2_cell())

  x_which_addr_tiger_match <- match(names(t_matches), names(x_s2))
  x_s2[x_which_addr_tiger_match] <- t_matches

  x_addr_ref_no_no_match_which <- is.na(x_s2)

  t_street_matches <-
    addr_match_tiger_street_ranges(
      x_addr[x_addr_ref_no_no_match_which],
      county = county,
      year = year,
      street_only_match = "closest",
      summarize = "centroid"
    ) |>
    purrr::discard(\(.) length(.) < 1) |> # removes NULL
    purrr::discard(\(.) nrow(.) < 1) |> # removes empty data.frame
    purrr::map_vec(\(.) s2::as_s2_cell(.$s2_geography), .ptype = s2::s2_cell())

  x_which_addr_tiger_street_match <- match(names(t_street_matches), names(x_s2))
  x_s2[x_which_addr_tiger_street_match] <- t_street_matches

  x_mm <- rep(NA, length = length(x_s2))
  x_mm[!is.na(x_addr_ref_match_which)] <- "ref_addr"
  x_mm[x_which_addr_tiger_match] <- "tiger_range"
  x_mm[x_which_addr_tiger_street_match] <- "tiger_street"
  x_mm[is.na(x_mm)] <- "none"

  out <-
    tibble::tibble(
      addr = x_addr,
      s2 = x_s2,
      match_method = factor(x_mm, levels = c("ref_addr", "tiger_range", "tiger_street", "none"))
    )

  return(out)
}
