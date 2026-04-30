#' geocode_zip
geocode_zip <- function(x) {
  stopifnot("x must be an addr vector" = inherits(x, "addr"))
  zpcd <- unique(x@place@zipcode)
  if (length(zpcd) != 1) {
    stop(
      "addr vectors passed to geocode_zip()",
      " must only have one unique zipcode;",
      " found ",
      length(zpcd),
      " unique zipcodes",
      call. = FALSE
    )
  }

  out <- list(
    matched_zipcode = rep(NA_character_, length(x)),
    matched_street = addr_street(rep(NA_character_, length(x))),
    matched_geography = s2::as_s2_geography(rep(NA_character_, length(x)))
  )

  if (is.na(zpcd) || zpcd == "") {
    return(out)
  }

  ref_exact <- taf_zip(zpcd, map = TRUE)
  out$matched_street <- match_addr_street(x@street, ref_exact$addr_street)
  no <- which(is.na(out$matched_street))
  out$matched_zipcode[-no] <- zpcd

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

  out$matched_zipcode[is.na(out$matched_zipcode)] <- ""

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

