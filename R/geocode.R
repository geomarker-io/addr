#' Geocode addr vectors with census.gov TIGER address features
#'
#' @description
#' `geocode_tiger()` geocodes addr vectors using census.gov TIGER address
#' features (see `?taf`) by:
#' 1. searching for a matching street (see `?match_addr_street`),
#' within the same zip code, also searching similar zip codes (see
#' `?zip_variant`) for a matching street if necessary
#' 2. using the address number to select the best address feature range and
#' side of the street (even/odd), tiebreaking on smallest width and spread)
#' 3. linearly interpolate a geographic point along the best range line based
#' on the actual and potential range of address numbers
#'
#' Only matched input addr will return non-missing matched zipcode/street
#' values. Missing or unmatched zip codes return missing matched zipcode/street,
#' geography, and s2cell values, like any other non-match. If all ranges on the
#' matched zipcode/street exclude the address number, then *only* the
#' geography/s2cell values will return NA.
#' @param x an addr vector (`?as_addr`)
#' @param offset number of meters to offset geocode from street line
#' @param progress logical; show a ZIP-code progress bar while geocoding?
#' @returns a tbl with columns: addr (`x`, addr vector),
#' matched zipcode (character vector, matched street (addr_street vector),
#' s2_geography (s2_geography point vector), and s2_cell (s2_cell vector)
#' @details
#' `geocode_zip()` is the workhorse function and operates on addr vectors
#' with the same ZIP code; use `geocode()` to geocode an addr vector
#' with different ZIP codes by grouping them by ZIP code and processing
#' serially.
#' At a lower level, grouping addr vectors by ZIP code and applying
#' `geocode_zip()` facilitates more control (e.g., parallel processing)
#'
#' @export
#' @examples
#'
#' x <- as_addr(voter_addresses()[1:100])
#' gcd <- geocode(x)
#'
#' leaflet::leaflet(wk::wk_coords(gcd$matched_geography)) |>
#'   leaflet::addTiles() |>
#'   leaflet::addCircleMarkers(lng = ~x, lat = ~y, label = ~feature_id)
geocode <- function(x, offset = 0L, progress = interactive()) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "offset must be an integer" = typeof(offset) == "integer",
    "offset must be length one" = length(offset) == 1,
    "offset must not be missing" = !is.na(offset),
    "progress must be TRUE or FALSE" = is.logical(progress) &&
      length(progress) == 1L &&
      !is.na(progress)
  )
  xu <- unique(x)
  missing_zip <- is.na(xu@place@zipcode) | xu@place@zipcode == ""
  z_list <- split(xu[!missing_zip], xu[!missing_zip]@place@zipcode)

  # gcd <- mirai::mirai_map(z_list, geocode_zip)[.progress, .stop]
  gcd <- if (length(z_list) == 0L) {
    list()
  } else if (progress) {
    lapply_pb(z_list, geocode_zip, offset = offset)
  } else {
    lapply(z_list, geocode_zip, offset = offset)
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
geocode_zip <- function(x, offset = 0L, progress_callback = NULL) {
  stopifnot("x must be an addr vector" = inherits(x, "addr"))
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
  out$matched_street <- match_addr_street(x@street, ref_exact$addr_street)

  no <- which(is.na(out$matched_street))
  out$matched_zipcode <- zpcd

  if (length(no) != 0) {
    out$matched_zipcode[no] <- NA_character_
    ref_variant <- taf_zip(zipcode_variant(zpcd), map = TRUE)
    out$matched_street[no] <- match_addr_street(
      x@street[no],
      ref_variant$addr_street
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
      s2::s2_interpolate_normalized(
        brm$s2_geography,
        max(0, min(1, (sn - brm$FROMHN) / (brm$TOHN - brm$FROMHN)))
      )
      # TODO: offset...
    }) |>
    do.call(c, args = _)

  out
}
