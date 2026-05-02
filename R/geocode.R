#' Geocode addr vectors with Census TIGER address features
#'
#' @description
#' `geocode()` geocodes addr vectors using Census TIGER address
#' features (see `?taf`) by:
#' 1. searching for a matching street (see `?match_addr_street`),
#' within the same ZIP code, also searching similar ZIP codes for a matching
#' street if necessary
#' 2. using the address number to select the best address feature range and
#' side of the street (even/odd), breaking ties on smallest width and spread
#' 3. linearly interpolating a geographic point along the best range line based
#' on the actual and potential range of address numbers
#' 4. offsetting the interpolated point from the range line perpendicularly
#'
#' Only matched input addresses return non-missing matched ZIP code and street
#' values. Missing or unmatched ZIP codes return missing matched ZIP code,
#' street, geography, and s2 cell values. If all ranges on the matched ZIP code
#' and street exclude the address number, only the geography and s2 cell values
#' return `NA`.
#' @param x an addr vector (`?as_addr`)
#' @param offset number of meters to offset geocode from street line
#' @param progress logical; show a ZIP-code progress bar while geocoding?
#' @inheritParams match_addr_street
#' @inheritParams match_zipcodes
#' @returns A tibble with columns `addr` (the input addr vector),
#'   `matched_zipcode` (character vector), `matched_street` (`addr_street`
#'   vector), `matched_geography` (`s2_geography` point vector), and `s2_cell`
#'   (`s2_cell` vector).
#' @details
#' `geocode_zip()` is the workhorse function and operates on addr vectors
#' with the same ZIP code; use `geocode()` to geocode an addr vector
#' with multiple ZIP codes by grouping them by ZIP code and processing
#' serially.
#' At a lower level, grouping addr vectors by ZIP code and applying
#' `geocode_zip()` facilitates more control (e.g., parallel processing)
#'
#' @export
#' @examples
#' Sys.setenv("R_USER_DATA_DIR" = tempfile())
#' taf_install("39061", "2025")
#'
#' x <- as_addr(voter_addresses()[1:100])
#' gcd <- geocode(x)
#'
#' leaflet::leaflet(wk::wk_coords(gcd$matched_geography)) |>
#'   leaflet::addTiles() |>
#'   leaflet::addCircleMarkers(lng = ~x, lat = ~y, label = ~feature_id)
geocode <- function(
  x,
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_predirectional = TRUE,
  match_street_posttype = TRUE,
  match_street_pretype = TRUE,
  match_street_postdirectional = FALSE,
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  offset = 10L,
  progress = interactive()
) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants),
    "progress must be TRUE or FALSE" = is.logical(progress) &&
      length(progress) == 1L &&
      !is.na(progress)
  )
  zip_variant <- validate_zip_variant(zip_variant)
  validate_geocode_offset(offset)
  validate_match_addr_street_args(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_predirectional = match_street_predirectional,
    match_street_posttype = match_street_posttype,
    match_street_pretype = match_street_pretype,
    match_street_postdirectional = match_street_postdirectional
  )
  xu <- unique(x)
  missing_zip <- is.na(xu@place@zipcode) | xu@place@zipcode == ""
  z_list <- split(xu[!missing_zip], xu[!missing_zip]@place@zipcode)

  # gcd <- mirai::mirai_map(z_list, geocode_zip)[.progress, .stop]
  gcd <- if (length(z_list) == 0L) {
    list()
  } else if (progress) {
    lapply_pb(
      z_list,
      geocode_zip,
      offset = offset,
      name_phonetic_dist = name_phonetic_dist,
      name_fuzzy_dist = name_fuzzy_dist,
      match_street_predirectional = match_street_predirectional,
      match_street_posttype = match_street_posttype,
      match_street_pretype = match_street_pretype,
      match_street_postdirectional = match_street_postdirectional,
      zip_variants = zip_variants,
      zip_variant = zip_variant
    )
  } else {
    lapply(
      z_list,
      geocode_zip,
      offset = offset,
      name_phonetic_dist = name_phonetic_dist,
      name_fuzzy_dist = name_fuzzy_dist,
      match_street_predirectional = match_street_predirectional,
      match_street_posttype = match_street_posttype,
      match_street_pretype = match_street_pretype,
      match_street_postdirectional = match_street_postdirectional,
      zip_variants = zip_variants,
      zip_variant = zip_variant
    )
  }
  if (any(missing_zip)) {
    gcd <- c(gcd, list(missing_zip = geocode_no_match(xu[missing_zip])))
  }
  if (length(gcd) == 0L) {
    gcd <- list(geocode_no_match(xu))
  }
  gcd <- do.call(rbind, gcd)

  out <- gcd[match(format(x), format(gcd$addr)), ]
  out$s2_cell <- s2::as_s2_cell(out$matched_geography)
  return(out)
}

lapply_pb <- function(x, FUN, ...) {
  total <- sum(lengths(x))
  processed <- 0L
  addr_progress_update(
    processed,
    total,
    "geocoding addr vectors",
    first = TRUE
  )
  on.exit(cat("\n"), add = TRUE)

  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    zip <- names(x)[[i]]
    x_n <- length(x[[i]])
    text <- geocode_progress_text(zip, x_n, NA_integer_)
    progress_callback <- function(y_n) {
      text <<- geocode_progress_text(zip, x_n, y_n)
      addr_progress_update(
        processed,
        total,
        text,
        first = FALSE
      )
    }
    args <- c(list(x[[i]]), list(...))
    if ("progress_callback" %in% names(formals(FUN))) {
      args$progress_callback <- progress_callback
    }
    out[[i]] <- do.call(FUN, args)
    processed <- processed + x_n
    addr_progress_update(
      processed,
      total,
      text,
      first = FALSE
    )
  }
  out
}

geocode_progress_text <- function(zip, x_n, y_n) {
  sprintf(
    "geocoding %s (%s addr to %s addr_street)",
    zip,
    prettyNum(x_n, big.mark = ",", preserve.width = "none"),
    prettyNum(y_n, big.mark = ",", preserve.width = "none")
  )
}

geocode_no_match <- function(x) {
  tibble::tibble(
    addr = x,
    matched_zipcode = rep(NA_character_, length(x)),
    matched_street = addr_street(rep(NA_character_, length(x))),
    matched_geography = s2::as_s2_geography(rep(NA_character_, length(x)))
  )
}


# # convert s2 cell and point geographies to text
# gcd_tbl$s2_cell <- as.character(gcd_tbl$s2_cell)
# gcd_tbl$wkt <- s2::s2_as_text(x_s2_geo)

#' @param progress_callback optional callback used internally by `geocode()`
#'   to update progress after ZIP-code reference data is loaded
#' @rdname geocode
geocode_zip <- function(
  x,
  offset = 10L,
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_predirectional = TRUE,
  match_street_posttype = TRUE,
  match_street_pretype = TRUE,
  match_street_postdirectional = FALSE,
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  progress_callback = NULL
) {
  stopifnot("x must be an addr vector" = inherits(x, "addr"))
  stopifnot(
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants)
  )
  zip_variant <- validate_zip_variant(zip_variant)
  validate_geocode_offset(offset)
  validate_match_addr_street_args(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_predirectional = match_street_predirectional,
    match_street_posttype = match_street_posttype,
    match_street_pretype = match_street_pretype,
    match_street_postdirectional = match_street_postdirectional
  )
  street_match_args <- list(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_predirectional = match_street_predirectional,
    match_street_posttype = match_street_posttype,
    match_street_pretype = match_street_pretype,
    match_street_postdirectional = match_street_postdirectional
  )
  zpcd <- unique(x@place@zipcode)
  if (length(zpcd) != 1) {
    stop(
      "addr vectors passed to geocode_zip()",
      " must only have one unique zipcode;",
      " found ",
      length(zpcd),
      " unique zipcodes;",
      " use `geocode()` with more than one unique zipcode;",
      call. = FALSE
    )
  }

  out <- geocode_no_match(x)

  if (is.na(zpcd) || zpcd == "") {
    return(out)
  }

  ref_exact <- taf_zip(zpcd, map = TRUE)
  if (is.function(progress_callback)) {
    progress_callback(length(ref_exact$addr_street))
  }
  out$matched_street <- do.call(
    match_addr_street,
    c(list(x = x@street, y = ref_exact$addr_street), street_match_args)
  )

  no <- which(is.na(out$matched_street))
  out$matched_zipcode <- zpcd

  if (length(no) != 0 && zip_variants) {
    out$matched_zipcode[no] <- NA_character_
    ref_variant <- taf_zip(
      zipcode_variant(zpcd, variant = zip_variant),
      map = TRUE
    )
    out$matched_street[no] <- do.call(
      match_addr_street,
      c(list(x = x@street[no], y = ref_variant$addr_street), street_match_args)
    )
    out$matched_zipcode[no] <-
      ref_variant[
        match(
          as.character(out$matched_street[no]),
          as.character(ref_variant$addr_street)
        ),
        "ZIP",
        drop = TRUE
      ]
  } else if (length(no) != 0) {
    out$matched_zipcode[no] <- NA_character_
    ref_variant <- ref_exact[0, ]
  } else {
    ref_variant <- ref_exact[0, ]
  }

  ref_rng <-
    lapply(seq_along(x), \(.i) {
      if (is.na(out$matched_zipcode[.i]) || is.na(out$matched_street[.i])) {
        return(ref_exact[0, ])
      }
      if (out$matched_zipcode[.i] == zpcd) {
        return(
          ref_exact[
            format(ref_exact$addr_street) == format(out$matched_street[.i]),
          ]
        )
      } else {
        return(
          ref_variant[
            format(ref_variant$addr_street) == format(out$matched_street[.i]) &
              ref_variant$ZIP == out$matched_zipcode[.i],
          ]
        )
      }
    })

  out$matched_geography <-
    lapply(seq_along(x), \(.i) {
      sn <- to_int(x@number@digits[.i])
      if (is.na(sn) || nrow(ref_rng[[.i]]) == 0L) {
        return(s2::as_s2_geography(NA_character_))
      }
      sn_par <- ifelse(sn %% 2 == 0, "E", "O")
      cand0 <- ref_rng[[.i]]
      cand0$in_range <- sn >= pmin(cand0$FROMHN, cand0$TOHN) &
        sn <= pmax(cand0$FROMHN, cand0$TOHN)
      cand0$par_ok <- cand0$in_range &
        (is.na(cand0$PARITY) | cand0$PARITY %in% c("B", sn_par))
      cand0$width <- ifelse(
        cand0$in_range,
        abs(cand0$FROMHN - cand0$TOHN),
        NA_integer_
      )
      cand0$mid <- ifelse(
        cand0$in_range,
        (cand0$FROMHN + cand0$TOHN) / 2,
        NA_real_
      )
      cand0$mid_dist <- abs(sn - cand0$mid)
      cand0 <- cand0[cand0$par_ok, ]
      cand0 <- cand0[order(cand0$width, cand0$mid_dist, na.last = TRUE), ]
      if (nrow(cand0) == 0L) {
        return(s2::as_s2_geography(NA_character_))
      }
      brm <- cand0[1, ]
      fraction <- geocode_range_fraction(sn, brm$FROMHN, brm$TOHN)
      point <- s2::s2_interpolate_normalized(
        brm$s2_geography,
        fraction
      )
      # TIGER side is relative to the digitized street line direction.
      geocode_offset_point(point, brm$s2_geography, fraction, brm$side, offset)
    }) |>
    do.call(c, args = _)

  out
}

validate_geocode_offset <- function(offset) {
  stopifnot(
    "offset must be numeric" = is.numeric(offset),
    "offset must be length one" = length(offset) == 1L,
    "offset must not be missing" = !is.na(offset),
    "offset must be finite" = is.finite(offset),
    "offset must be non-negative" = offset >= 0
  )
}

geocode_range_fraction <- function(number, from, to) {
  if (from == to) {
    return(0.5)
  }
  max(0, min(1, (number - from) / (to - from)))
}

geocode_offset_point <- function(point, line, fraction, side, offset) {
  if (offset == 0 || is.na(side) || !side %in% c("L", "R")) {
    return(point)
  }

  bearing <- geocode_line_bearing(line, fraction)
  if (is.na(bearing)) {
    return(point)
  }

  offset_bearing <- bearing + if (side == "L") -90 else 90
  geocode_direct_point(point, offset_bearing, offset)
}

geocode_line_bearing <- function(line, fraction) {
  delta <- 1e-6
  from <- s2::s2_interpolate_normalized(line, max(0, fraction - delta))
  to <- s2::s2_interpolate_normalized(line, min(1, fraction + delta))

  if (s2::s2_distance(from, to) == 0) {
    return(NA_real_)
  }

  geocode_bearing(from, to)
}

geocode_bearing <- function(from, to) {
  from_lon <- geocode_degrees_to_radians(s2::s2_x(from))
  from_lat <- geocode_degrees_to_radians(s2::s2_y(from))
  to_lon <- geocode_degrees_to_radians(s2::s2_x(to))
  to_lat <- geocode_degrees_to_radians(s2::s2_y(to))

  d_lon <- to_lon - from_lon
  x <- sin(d_lon) * cos(to_lat)
  y <- cos(from_lat) * sin(to_lat) - sin(from_lat) * cos(to_lat) * cos(d_lon)

  geocode_radians_to_degrees(atan2(x, y))
}

geocode_direct_point <- function(point, bearing, distance) {
  radius <- s2::s2_earth_radius_meters()
  angular_distance <- distance / radius
  lon <- geocode_degrees_to_radians(s2::s2_x(point))
  lat <- geocode_degrees_to_radians(s2::s2_y(point))
  bearing <- geocode_degrees_to_radians(bearing)

  out_lat <- asin(
    sin(lat) *
      cos(angular_distance) +
      cos(lat) * sin(angular_distance) * cos(bearing)
  )
  out_lon <- lon +
    atan2(
      sin(bearing) * sin(angular_distance) * cos(lat),
      cos(angular_distance) - sin(lat) * sin(out_lat)
    )

  s2::s2_geog_point(
    geocode_normalize_longitude(geocode_radians_to_degrees(out_lon)),
    geocode_radians_to_degrees(out_lat)
  )
}

geocode_degrees_to_radians <- function(x) {
  x * pi / 180
}

geocode_radians_to_degrees <- function(x) {
  x * 180 / pi
}

geocode_normalize_longitude <- function(x) {
  ((x + 540) %% 360) - 180
}
