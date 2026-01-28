#' Fuzzy match
#'
#' Fuzzy match strings in x to y using optimized string alignment
#' (ignoring capitalization).
#'
#' If multiple strings in `y` are tied for the minimum osa distances with a
#' string in `x`, then all of their indices are included in the return value.
#' @param x character vector to match
#' @param y character vector to match to
#' @param osa_max_dist maximum OSA distance to consider a match
#' @return a list of integer vectors representing the position of the best
#' matching string(s) in `y` for each string in `x`
fuzzy_match <- function(
  x,
  y,
  osa_max_dist = 1
) {
  if (!rlang::is_character(x)) {
    rlang::abort("x must be a character")
  }
  if (!rlang::is_character(y)) {
    rlang::abort("y must be a character")
  }
  if (!rlang::is_bare_numeric(osa_max_dist)) {
    rlang::abort("osa_max_dist must be a numeric")
  }

  the_dist <- stringdist::stringdistmatrix(
    tolower(x),
    tolower(y),
    method = "osa"
  )
  min_dist_matches <- apply(
    the_dist,
    MARGIN = 1,
    FUN = \(.) {
      which(min(., na.rm = TRUE) <= osa_max_dist & . == min(., na.rm = TRUE))
    },
    simplify = FALSE
  ) |>
    suppressWarnings()
  out <- purrr::modify_if(min_dist_matches, \(.) length(.) == 0, \(.) NA)

  return(out)
}

#' @param x_addr addr vector to match
#' @param y_addr addr vector to match to
#' @param addr_field character name of `addr()` field to match on
#' @rdname fuzzy_match
#' @export
#' @examples
#' fuzzy_match_addr_field(as_addr(c("123 Main St.", "3333 Burnet Ave", "3333 Foofy Ave")),
#'                        as_addr(c("0000 Main Street", "0000 Burnet Avenue", "222 Burnet Ave")),
#'                        addr_field = "street_name")
fuzzy_match_addr_field <- function(
  x,
  y,
  addr_field = c(
    "number_prefix",
    "number_digits",
    "number_suffix",
    "street_predirectional",
    "street_premodifier",
    "street_pretype",
    "street_name",
    "street_posttype",
    "street_postdirectional",
    "place_name",
    "place_state",
    "place_zipcode"
  ),
  osa_max_dist = 0
) {
  stopifnot(
    "x must be an addr object" = inherits(x, "addr"),
    "y must be an addr object" = inherits(y, "addr")
  )
  addr_field <- strsplit(match.arg(addr_field), "_", fixed = TRUE)[[1]]
  x_field <- S7::prop(S7::prop(x, addr_field[1]), addr_field[2])
  y_field <- S7::prop(S7::prop(y, addr_field[1]), addr_field[2])
  fuzzy_match(
    x_field,
    y_field,
    osa_max_dist = osa_max_dist
  )
}
