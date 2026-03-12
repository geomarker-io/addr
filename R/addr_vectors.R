#' @include addr_part.R addr.R
#' @importFrom vctrs vec_proxy vec_restore vec_ptype_full vec_ptype_abbr vec_size
NULL

#' @method vec_proxy addr_part
#' @export
vec_proxy.addr_part <- function(x, ...) {
  as.data.frame(x, stringsAsFactors = FALSE)
}

#' @method vec_restore addr_number
#' @export
vec_restore.addr_number <- function(x, to, ...) {
  addr_number(
    prefix = x[["number_prefix"]],
    digits = x[["number_digits"]],
    suffix = x[["number_suffix"]]
  )
}

#' @method vec_restore addr_street
#' @export
vec_restore.addr_street <- function(x, to, ...) {
  addr_street(
    predirectional = x[["street_predirectional"]],
    premodifier = x[["street_premodifier"]],
    pretype = x[["street_pretype"]],
    name = x[["street_name"]],
    posttype = x[["street_posttype"]],
    postdirectional = x[["street_postdirectional"]],
    map_posttype = FALSE,
    map_directional = FALSE,
    map_pretype = FALSE,
    map_ordinal = FALSE
  )
}

#' @method vec_restore addr_place
#' @export
vec_restore.addr_place <- function(x, to, ...) {
  addr_place(
    name = x[["place_name"]],
    state = x[["place_state"]],
    zipcode = x[["place_zipcode"]],
    map_state = FALSE
  )
}

#' @method vec_size addr_part
#' @export
vec_size.addr_part <- function(x, ...) {
  length(x)
}

#' @method vec_proxy addr
#' @export
vec_proxy.addr <- function(x, ...) {
  as.data.frame(x, stringsAsFactors = FALSE)
}

#' @method vec_restore addr
#' @export
vec_restore.addr <- function(x, to, ...) {
  if (nrow(x) == 0L) {
    return(addr())
  }
  addr(
    number = addr_number(
      prefix = x[["number_prefix"]],
      digits = x[["number_digits"]],
      suffix = x[["number_suffix"]]
    ),
    street = addr_street(
      predirectional = x[["street_predirectional"]],
      premodifier = x[["street_premodifier"]],
      pretype = x[["street_pretype"]],
      name = x[["street_name"]],
      posttype = x[["street_posttype"]],
      postdirectional = x[["street_postdirectional"]],
      map_posttype = FALSE,
      map_directional = FALSE,
      map_pretype = FALSE
    ),
    place = addr_place(
      name = x[["place_name"]],
      state = x[["place_state"]],
      zipcode = x[["place_zipcode"]],
      map_state = FALSE
    )
  )
}

#' @method vec_ptype_full addr
#' @export
vec_ptype_full.addr <- function(x, ...) {
  "addr"
}

#' @method vec_ptype_abbr addr
#' @export
vec_ptype_abbr.addr <- function(x, ...) {
  "addr"
}

#' @method vec_size addr
#' @export
vec_size.addr <- function(x, ...) {
  length(x)
}
