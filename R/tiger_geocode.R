#' Geocode using TIGER address features
#'
#' @description
#' geocode_tiger() geocodes addr vectors using TIGER address
#' range geometries downloaded from census.gov for a specific county.
#'
#' The geocoding precision is the level of matching achieved for each address:
#' - `range`: is the most precise; the address matched on street name and
#' ZIP code, and the number was within one of the ranges; point locations are
#' interpolated from the geometry of the best match using the street number
#' - `street`: is less precise; the address matched on street name and ZIP code,
#' but the number was *not* within one of the ranges; point locations
#' are interpolated as the centroid of all matching range geometries
#' - `unmatched`: no precision; the address did not match any street name
#' and ZIP code combinations; point locations are `<null feature>` (which is
#' equivalent to `NA`)
#' @param x an addr vector
#' @inheritParams tiger_addr_feat
#' @param offset number of meters to offset geocode from street line
#' @returns a s2_geography vector of geocoded point locations;
#' an attribute ("geocode_precision") is added as a factor with
#' levels `range`, `street`, or `unmatched`.
#' @details
#' If a street number-name has multiple in range and parity matches, then
#' the best match is chosen based on the smallest width of ranges and
#' then on the range with the closest midpoint.
#'
#' @export
#' @examples
#' voter_geocoded_points <-
#'   geocode_tiger(as_addr(voter_addresses()[1:100]),
#'                 county = "39061", year = "2024", offset = 20)
#'
#' voter_geocoded_points
#'
#' table(attr(voter_geocoded_points, "geocode_precision"))
geocode_tiger <- function(x, county, year, offset = 0) {
  stopifnot("x must be an addr vector" = inherits(x, "addr"))

  taf <- tiger_addr_feat(county = county, year = year)

  taf_addr <-
    paste("123", taf$FULLNAME, "Anytown OH", taf$ZIP) |>
    as_addr(
      map_pretype = FALSE,
      map_posttype = FALSE,
      map_state = FALSE,
      map_directional = TRUE
    )
  # not mapping prevents missing, which is a deal-breaker for matching in y

  message(
    "matching ",
    length(x),
    " addresses to ",
    nrow(taf),
    " address ranges in ",
    "county ",
    county,
    " (TIGER vintage ",
    year,
    ")",
    " ..."
  )

  x_which_matched_ranges <-
    addr_fuzzy_match(
      x,
      taf_addr,
      addr_fields = c(
        number_prefix = Inf,
        number_digits = Inf,
        number_suffix = Inf,
        place_name = Inf,
        place_state = Inf,
        place_zipcode = 1
      )
    )

  no_match_street_zip <- which(lengths(x_which_matched_ranges) == 0)
  if (length(no_match_street_zip) > 0) {
    warning(
      length(no_match_street_zip),
      " addr in x could not be matched to any street-zipcodes"
    )
  }

  message("finding best matched ranges using street number...")

  x_best_ranges <-
    lapply(seq_along(x), \(.) {
      find_best_range(
        x[.]@number@digits,
        taf[x_which_matched_ranges[[.]], ]
      )
    })

  no_in_range <-
    which(do.call(c, lapply(x_best_ranges, nrow)) == 0) |>
    setdiff(no_match_street_zip)

  if (length(no_in_range) > 0) {
    warning(
      length(no_in_range),
      " addr in x were matched to a street, but could not be matched to a range"
    )
  }

  message("imputing point along matched range...")

  out <-
    lapply(seq_along(x), \(.) {
      impute_range_point(
        to_int(x[.]@number@digits),
        x_best_ranges[[.]]
      )
    }) |>
    do.call(c, args = _)

  if (length(no_in_range) > 0) {
    street_match_centroids <-
      x_which_matched_ranges[no_in_range] |>
      lapply(\(.) taf[., ]) |>
      lapply(`[[`, "geometry") |>
      lapply(s2::s2_union_agg) |>
      lapply(s2::s2_centroid) |>
      do.call(c, args = _)

    out[no_in_range] <- street_match_centroids
  }

  if (length(no_match_street_zip) > 0) {
    out[no_match_street_zip] <- NA_character_
  }

  out_precision <- rep("range", length(x))
  out_precision[no_match_street_zip] <- "unmatched"
  out_precision[no_in_range] <- "street"

  attr(out, "geocode_precision") <- factor(
    out_precision,
    levels = c("range", "unmatched", "street")
  )
  out
}

find_best_range <- function(street_number, candidate_ranges) {
  sn <- to_int(street_number)
  if (nrow(candidate_ranges) == 0) {
    return(candidate_ranges[NULL, ])
  }
  sn_par <- if (sn %% 2 == 0) "E" else "O"
  cand0 <- candidate_ranges

  cand0$in_range <- sn >= pmin(cand0$FROMHN, cand0$TOHN) &
    sn <= pmax(cand0$FROMHN, cand0$TOHN)

  cand0$par_ok <- cand0$in_range &
    (is.na(cand0$PARITY) | cand0$PARITY %in% c("B", sn_par))

  cand0$width <- ifelse(
    cand0$in_range,
    abs(cand0$FROMHN - cand0$TOHN),
    NA_integer_
  )
  cand0$mid <- ifelse(cand0$in_range, (cand0$FROMHN + cand0$TOHN) / 2, NA_real_)
  cand0$mid_dist <- abs(sn - cand0$mid)

  cand <- cand0[cand0$par_ok, ]
  ord <- order(cand$width, cand$mid_dist, na.last = TRUE)
  cand <- cand[ord, ]

  if (nrow(cand) == 0) {
    return(candidate_ranges[NULL, ])
  } else {
    if (nrow(cand) > 1) {
      message("one of multiple street_number ranges matched")
    }
    best_range <- cand[1, ]
  }

  best_range[, c(
    "FULLNAME",
    "ZIP",
    "side",
    "FROMHN",
    "TOHN",
    "OFFSET",
    "geometry"
  )] |>
    tibble::as_tibble()
}

impute_range_point <- function(street_number, best_range_match, offset = 0L) {
  if (nrow(best_range_match) == 0) {
    return(s2::s2_geog_from_text("POINT EMPTY"))
  }
  rng_frac <- max(
    0,
    min(
      1,
      (street_number - best_range_match$FROMHN) /
        (best_range_match$TOHN - best_range_match$FROMHN)
    )
  )
  rng_point <- s2::s2_interpolate_normalized(
    best_range_match$geometry,
    rng_frac
  )
  if (best_range_match$OFFSET == "Y" && offset > 0) {
    stop("offsetting not yet implemented", call. = FALSE)
  }
  return(rng_point)
}
