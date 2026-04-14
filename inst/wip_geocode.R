devtools::load_all()

## arguments
# (map street name tags/ordinals in input (if char, not already addr)?)
# map street name tags/ordinals in tiger addr features?
# zip_variants logical;
# match_addr_street criteria;

#' geocode_tiger(as_addr(voter_addresses()[1323]))
geocode_tiger <- function(x) {
  out <- tibble::tibble(
    matched_zipcode = NA_character_,
    matched_addr_street = addr_street(),
    s2_geography = s2::s2_geog_from_text("POINT EMPTY")
  )
  if (is.na(x@place) || x@place@zipcode == "") {
    warning("missing or empty parsed zipcode in x")
    return(out)
  }
  ref <- taf_zip(x@place@zipcode, map = TRUE)
  if (nrow(ref) == 0) {
    warning("parsed zipcode not in TIGER address features: ", x@place@zipcode)
    ref_fuzzy <- taf_zip(zipcode_variant(x@place@zipcode), map = TRUE)
    x_ref_street <- match_addr_street(x@street, ref_fuzzy$addr_street)
  } else {
    out$matched_zipcode <- x@place@zipcode
    x_ref_street <- match_addr_street(x@street, ref$addr_street)
  }
  if (is.na(x_ref_street)) {
    if (!is.na(out$matched_zipcode)) {
      ref_fuzzy <- taf_zip(zipcode_variant(x@place@zipcode), map = TRUE)
      x_ref_street <- match_addr_street(x@street, ref_fuzzy$addr_street)
    }
  }
  if (is.na(x_ref_street)) {
    out$matched_zipcode <- character(0)
    return(out)
  }
  ref_rng <- ref[as.character(ref$addr_street) == as.character(x_ref_street), ]
  sn <- to_int(x@number@digits)
  sn_par <- ifelse(sn %% 2 == 0, "E", "O")
  cand0 <- ref_rng
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
  cand0 <- cand0[cand0$par_ok, ]
  cand0 <- cand0[order(cand0$width, cand0$mid_dist, na.last = TRUE), ]
  if (nrow(cand0) == 0) {
    # TODO: no matching ranges in matched street
  }
  brm <- cand0[1, ]
  out$matched_zipcode <- brm[1, "ZIP", drop = TRUE]
  out$matched_addr_street <- brm[1, "addr_street", drop = TRUE]
  out$s2_geography <-
    s2::s2_interpolate_normalized(
      brm$s2_geography,
      max(0, min(1, (sn - brm$FROMHN) / (brm$TOHN - brm$FROMHN)))
    )
  # TODO: offset...
  return(out)
}
