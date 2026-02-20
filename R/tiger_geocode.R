#' Geocode using TIGER address features
#'
#' @param x an addr vector
#' @inheritParams tiger_addr_feat
#' @param offset number of meters to offset geocode from street line
#' @returns a s2_geography vector of geocoded point locations
#' @details
#' If a street number-name has multiple in range and parity matches, then the best match
#' is chosen based on the smallest width of ranges and then on the range with the closest midpoint
#' @export
geocode_tiger <- function(x, county, year, offset = 0) {
  stopifnot("x must be an addr vector" = inherits(x, "addr"))
  x_street_number <- to_int(vctrs::field(x, "street_number"))
  x_street_name <- paste(
    vctrs::field(x, "street_name"),
    vctrs::field(x, "street_type")
  )
  narrow_county_ranges(x_street_name, tiger_addr_feat(county, year)) |>
    find_best_range(x_street_number, candidate_ranges = _) |>
    impute_range_point(x_street_number, best_range_match = _)
}

# TODO better fuzzy matching on street names and types
narrow_county_ranges <- function(street_name, tiger_addr_feat) {
  tiger <- tiger_addr_feat("39061", "2022")
  street_name <- "Woolper"
  street_name_post_type <- "Ave"
}

find_best_range <- function(street_number, candidate_ranges) {
  sn <- to_int(street_number)
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
    message("street number did not match any street_number ranges")
    return(NA)
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
