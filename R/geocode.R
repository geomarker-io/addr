#' geocode using installed TIGER address features
#'
# @description
# geocode_tiger() geocodes addr vectors using census.gov TIGER address
# features (see `?taf`) by:
#' 1. searching for a matching street (see `?match_addr_street`),
#' within the same zip code, also searching similar zip codes (see
#' `?zip_variant`) for a matching street if necessary
#' 2. using the address number to select the best address feature range and
#' side of the street (even/odd), tiebreaking on smallest width and spread)
#' 3. linearly interpolate a geographic point along the best range line based
#' on the actual and potential range of address numbers
#'
#' Under the hood, addresses are grouped by ZIP code for processing;
#' parallel and asyncronous geocoding are supported through `mirai::daemons`
#' @param x an addr vector (`?as_addr`)
#' @param offset number of meters to offset geocode from street line
#' @returns a tbl with columns: addr (`x`, addr vector),
#' matched zipcode (character vector, matched street (addr_street vector),
#' s2_geography (s2_geography point vector), and s2_cell (s2_cell vector);
#' see details
#' @details
#' Only matched input addr will return non-missing matched zipcode/street (and
#' geography/s2cell) values, but if all ranges on the matched zipcode/street
#' exclude the address number, then *only* the geography/s2cell values
#' will return NA.
#'
#' `geocode_zip()` is the workhorse function and operates on addr vectors
#' with the same ZIP code; use `geocode()` to geocode an addr vector
#' with different ZIP codes by grouping them by ZIP code and processing
#' serially.
#' At a lower level, grouping addr vectors by ZIP code and applying
#' `geocode_zip()` facilitates more control (e.g., parallel processing)
#' @export
#' @examples
#'
#' x <- as_addr(voter_addresses()[1:100])
#' gcd <- geocode(x)
#'
#' leaflet::leaflet(wk::wk_coords(gcd$matched_geography)) |>
#'   leaflet::addTiles() |>
#'   leaflet::addCircleMarkers(lng = ~x, lat = ~y, label = ~feature_id)
geocode <- function(x, offset = 0L) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "offset must be an integer" = typeof(offset) == "integer",
    "offset must be length one" = length(offset) == 1,
    "offset must not be missing" = !is.na(offset)
  )
  xu <- unique(x)
  z_list <- split(xu, xu@place@zipcode)

  # gcd <- mirai::mirai_map(z_list, geocode_zip)[.progress, .stop]
  gcd <- lapply_pb(z_list, geocode_zip)
  gcd <- do.call(rbind, gcd)

  out <- gcd[match(format(x), format(gcd$addr)), ]
  out$s2_cell <- s2::as_s2_cell(out$matched_geography)
  return(out)
}

lapply_pb <- function(x, FUN, ...) {
  pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
  on.exit(close(pb), add = TRUE)
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- FUN(x[[i]], ...)
    utils::setTxtProgressBar(pb, i)
  }
  out
}


# # convert s2 cell and point geographies to text
# gcd_tbl$s2_cell <- as.character(gcd_tbl$s2_cell)
# gcd_tbl$wkt <- s2::s2_as_text(x_s2_geo)

#' @rdname geocode
geocode_zip <- function(x, offset = 0L) {
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

  out <- tibble::tibble(
    addr = x,
    matched_zipcode = rep(NA_character_, length(x)),
    matched_street = addr_street(rep(NA_character_, length(x))),
    matched_geography = s2::as_s2_geography(rep(NA_character_, length(x)))
  )

  if (is.na(zpcd) || zpcd == "") {
    return(out)
  }

  ref_exact <- addr::taf_zip(zpcd, map = TRUE)
  out$matched_street <- match_addr_street(x@street, ref_exact$addr_street)

  no <- which(is.na(out$matched_street))
  out$matched_zipcode <- zpcd

  if (length(no) != 0) {
    out$matched_zipcode[no] <- NA_character_
    ref_variant <- addr::taf_zip(zipcode_variant(zpcd), map = TRUE)
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
    out$matched_zipcode[is.na(out$matched_zipcode)] <- ""
  }

  ref_rng <-
    lapply(seq_along(x), \(.i) {
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

