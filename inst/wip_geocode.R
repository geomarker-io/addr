devtools::load_all()

## arguments
# (map street name tags/ordinals in input (if char, not already addr)?)
# map street name tags/ordinals in tiger addr features?
# zip_variants logical;
# match_addr_street criteria;

x <- voter_addresses()[1:500]

x_addr <- as_addr(x)


x_addr[x_addr@place@zipcode == "45220"] |>
  geocode_zip()


gcd <- purrr::map(as.list(x_addr), geocode_tiger, .progress = "geocoding")

# stop and return here for R-based output of list
# continue to use (degraded) table-based output

gcd_tbl <-
  tibble::tibble(
    address = x,
    matched_zip = vapply(gcd, \(.) .$matched_zipcode, character(1)),
    matched_street = vapply(
      gcd,
      \(.) format(.$matched_addr_street),
      character(1)
    )
  )

x_s2_geo <- do.call(c, lapply(gcd, \(.) .$s2_geography))

# prevent s2_cell for empty points
x_s2_geo[s2::s2_is_empty(x_s2_geo)] <- NA_character_
# prevent s2_cell for line string matches
gcd_lines <- which(vapply(x_s2_geo, s2::s2_dimension, integer(1)) == 1)
gcd_tbl[-gcd_lines, "s2_cell"] <- s2::as_s2_cell(x_s2_geo[-gcd_lines])


# add tiger block group
gcd_tbl$tiger_bg_2024 <-
  geomarker::s2_join_tiger_bg(
    gcd_tbl$s2_cell,
    year = "2024"
  )

# convert s2 cell and point geographies to text
gcd_tbl$s2_cell <- as.character(gcd_tbl$s2_cell)
gcd_tbl$wkt <- s2::s2_as_text(x_s2_geo)

gcd_tbl # can be output as simple table here

# also do version that outputs json version of list above? to allow non-R
# outputs that don't degrade...

# move to tests
geocode_one(addr()) # NA addr
geocode_one(as_addr(NA_character_)) # NA address string
geocode_one(as_addr("")) # empty address string
geocode_one(as_addr("3333 Burnet Ave Cincinnati, OH 45229")) # good
geocode_one(as_addr("3333 burnet Av. Cincy, Ohio 45229")) # aliases, messy
geocode_one(as_addr("3333 Bunret Ave Cincinnati, OH 45229")) # typo swap
geocode_one(as_addr("3333 Burmnett Ave Cincinnati, OH 45229")) # phonetic
geocode_one(as_addr("3333 Burnet Ave Cincinnati, OH")) # no zip
geocode_one(as_addr("333 Burnet Ave Cincinnati, OH 45229")) # out of range
geocode_one(as_addr("3333 Burnet Ave Cincinnati, OH 45239")) # zip typo good
geocode_one(as_addr("3333 Burnet Ave Cincinnati, OH 45219")) # zip typo bad


#' geocode_one(as_addr(voter_addresses()[1323]))
geocode_one <- function(x) {
  stopifnot("x must be an addr vector" = inherits(x, "addr"))
  out <- list(
    matched_zipcode = NA_character_,
    matched_addr_street = addr_street(),
    s2_geography = s2::as_s2_geography(NA_character_)
  )
  if (is.na(x)) {
    return(out)
  }
  if (is.na(x@place) || x@place@zipcode == "") {
    warning("missing or empty parsed zipcode in x")
    out$s2_geography <- s2::as_s2_geography("POINT EMPTY")
    return(out)
  }

  ref <- taf_zip(x@place@zipcode, map = TRUE)
  if (nrow(ref) != 0) {
    out$matched_addr_street <- match_addr_street(x@street, ref$addr_street)
    if (!is.na(out$matched_addr_street)) {
      out$matched_zipcode <- x@place@zipcode
    }
  }

  if (nrow(ref) == 0 || is.na(out$matched_addr_street)) {
    ref <- taf_zip(zipcode_variant(x@place@zipcode), map = TRUE)
    out$matched_addr_street <- match_addr_street(x@street, ref$addr_street)
  }

  if (is.na(out$matched_addr_street)) {
    out$matched_zipcode <- ""
    return(out)
  }

  ref_rng <- ref[format(ref$addr_street) == format(out$matched_addr_street), ]
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
    out$matched_zipcode <- ref_rng[1, "ZIP", drop = TRUE]
    out$s2_geography <- s2::s2_union_agg(ref_rng$s2_geography)
    # out$s2_geography <- s2::as_s2_geography("POINT EMPTY")
    return(out)
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
