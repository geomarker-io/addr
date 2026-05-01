#' Fuzzy match
#'
#' Fuzzy match strings in `x` to strings in `y` using optimized string
#' alignment (OSA) distance and ignoring capitalization.
#'
#' If multiple strings in `y` are tied for the minimum OSA distance from a
#' string in `x`, all of their indices are included in the return value.
#' @param x character vector to match
#' @param y character vector to match to
#' @param osa_max_dist maximum OSA distance to consider a match;
#' `Inf` is a special case that avoids computing string distance by
#' returning all of `y` instead of just the best match or matches in `y`.
#' @param prefilter method used to prefilter `y` before computing
#' OSA distances; `"none"` does nothing, and `"psk"` removes values in `y`
#' that do not share a `phonetic_street_key()` with any value in `x`.
#' @return a list of integer vectors representing the position of the best
#' matching string(s) in `y` for each string in `x`
#' @export
#' @examples
#' my_names <-
#'   c("Pinye", "Pine", "Oalck", "Sunset", "Riverbend", "Greenfild")
#' the_names <-
#'   c("Piney", "Pine", "Oak", "Cheshire", "Greenfield", "Maple", "Elm")
#' matches <- fuzzy_match(my_names, the_names, osa_max_dist = 1)
#' matches
#'
#' lapply(matches, \(i) the_names[i])
#'
#' x <- as_addr(voter_addresses()[1:100])@street@name
#' y <- unique(nad_example_data()$nad_addr@street@name)
#' system.time(fuzzy_match(x, y))
#' # larger vectors see a speedup when using
#' # phonetic_street_key as a prefilter
#' # but may miss potential matches that are within
#' # osa_max_dist of each other, but did not have
#' # identical phonetic codes (e.g., "woolper" and "woopler")
#' system.time(fuzzy_match(x, y, prefilter = "psk"))
fuzzy_match <- function(x, y, osa_max_dist = 1, prefilter = c("none", "psk")) {
  prefilter <- match.arg(prefilter)
  stopifnot(
    "x must be character" = is.character(x),
    "y must be character" = is.character(y),
    "osa_max_dist must be numeric" = is.numeric(osa_max_dist),
    "osa_max_dist must be length one" = length(osa_max_dist) == 1,
    "osa_max_dist must not be missing" = !is.na(osa_max_dist),
    "y must not contain NA" = !any(is.na(y))
  )
  if (is.infinite(osa_max_dist)) {
    return(replicate(length(x), seq_along(y), simplify = FALSE))
  }
  if (any(is.na(x))) {
    out <- rep(list(NA), length(x))
    keep <- which(!is.na(x))
    if (length(keep) == 0) {
      return(out)
    }
    x <- x[keep]
  } else {
    out <- NULL
    keep <- NULL
  }

  if (prefilter == "psk") {
    psks <- lapply(list(x = x, y = y), phonetic_street_key)
    chuck <- which(!psks$y %in% unique(psks$x))
    y[chuck] <- NA_character_
  }

  the_dist <- stringdist::stringdistmatrix(
    tolower(x),
    tolower(y),
    method = "osa"
  )
  matches <- apply(
    the_dist,
    MARGIN = 1,
    FUN = matrix_row_min_thresh,
    thresh = osa_max_dist,
    simplify = FALSE
  )
  if (!is.null(out)) {
    out[keep] <- matches
  } else {
    out <- matches
  }
  return(out)
}

#' Fuzzy match addr vectors using field-specific string distances
#'
#' @description
#' `addr_fuzzy_match()` matches two addr vectors using more than one address
#' field.
#'
#' `fuzzy_match_addr_field()` matches two addr vectors using a single address
#' field.
#'
#' Distances between address tags are defined using optimized string alignment;
#' see `fuzzy_match()` and `stringdist::stringdist()` for more details.
#' @param x addr vector to match
#' @param y addr vector to match to
#' @param addr_fields a named vector of OSA maximum distances. Defaults are
#'   used for fields that are not supplied; see Details.
#' @details
#' Defaults for `addr_fields`:
#' \itemize{
#'   \item number_prefix: 0
#'   \item number_digits: 0
#'   \item number_suffix: 0
#'   \item street_predirectional: 0
#'   \item street_premodifier: 0
#'   \item street_pretype: 0
#'   \item street_name: 1
#'   \item street_posttype: 0
#'   \item street_postdirectional: 0
#'   \item place_name: 0
#'   \item place_state: 0
#'   \item place_zipcode: 0
#' }
#'
#' When fuzzy matching `street_name`, the "phonetic_street_key"
#' prefilter is automatically used (see `?fuzzy_match`).
#' @return a list of integer vectors representing the position of the best
#' matching address(es) in `y` for each address in `x`
#' @export
#' @examples
#' x_addr <- as_addr(c("123 Main St.", "333 Burnet Ave", "3333 Foofy Ave"))
#' y_addr <- as_addr(c("0000 Main Street", "3333 Burnet Avenue", "222 Burnet Ave"))
#'
#' # no matches with defaults
#' addr_fuzzy_match(x_addr, y_addr)
#'
#' # match on osa_max_dist of 2 for the address number
#' addr_fuzzy_match(x_addr, y_addr, addr_fields = c("number_digits" = 2))
#'
#' # ignore address number when matching
#' addr_fuzzy_match(x_addr, y_addr, addr_fields = c("number_digits" = Inf))
#'
addr_fuzzy_match <- function(
  x,
  y,
  addr_fields = NULL
) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "y must be an addr vector" = inherits(y, "addr"),
    "y must have length > 0" = length(y) > 0
  )
  if (length(x) == 0) {
    return(list(integer(0)))
  }
  addr_fields <- addr_fuzzy_match_resolve_max_dist(addr_fields)
  matches <- mapply(
    fuzzy_match_addr_field,
    addr_field = names(addr_fields),
    osa_max_dist = addr_fields,
    MoreArgs = list(x = x, y = y),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  out <- do.call(
    Map,
    c(list(function(...) Reduce(intersect, list(...))), matches)
  )
  return(out)
}


#' Fuzzy match addr vectors on one field
#'
#' @param addr_field character name of single addr field to match on
#' @param osa_max_dist maximum optimized string alignment distance
#' used as threshold for matching on single addr field
#' @rdname addr_fuzzy_match
#' @export
#' @examples
#'
#' fuzzy_match_addr_field(
#'   as_addr(c("123 Main St.", "3333 Burnet Ave", "3333 Foofy Ave")),
#'   as_addr(c("0000 Main Street", "0000 Burnet Avenue", "222 Burnet Ave")),
#'   addr_field = "street_name", osa_max_dist = 1
#' )
#'
#' # empty address fields have an OSA distance of zero and always match
#' fuzzy_match_addr_field(
#'   as_addr(c("123 Main St.", "3333 Burnet Ave", "3333 Foofy Ave")),
#'   as_addr(c("0000 Main Street", "0000 Burnet Avenue", "222 Burnet Ave")),
#'   addr_field = "number_prefix"
#' )
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
  addr_field12 <- strsplit(match.arg(addr_field), "_", fixed = TRUE)[[1]]
  x_field <- S7::prop(S7::prop(x, addr_field12[1]), addr_field12[2])
  y_field <- S7::prop(S7::prop(y, addr_field12[1]), addr_field12[2])
  if (any(is.na(y_field))) {
    stop(
      sprintf(
        "addr_fuzzy_match: y field '%s' contains NA; see ?as_addr",
        paste(addr_field12, collapse = "_")
      ),
      call. = FALSE
    )
  }
  fuzzy_match(
    x_field,
    y_field,
    osa_max_dist = osa_max_dist,
    prefilter = "none"
    # prefilter = ifelse(addr_field == "street_name", "psk", "none")
  )
}

addr_fuzzy_match_default_max_dist <- function() {
  c(
    "number_prefix" = 0,
    "number_digits" = 0,
    "number_suffix" = 0,
    "street_predirectional" = 0,
    "street_premodifier" = 0,
    "street_pretype" = 0,
    "street_name" = 1,
    "street_posttype" = 0,
    "street_postdirectional" = 0,
    "place_name" = 0,
    "place_state" = 0,
    "place_zipcode" = 0
  )
}

addr_fuzzy_match_validate_max_dist <- function(max_dist) {
  if (!is.numeric(max_dist) || any(is.na(max_dist))) {
    stop(
      "addr_fuzzy_match max distances must be numeric and not missing.",
      call. = FALSE
    )
  }
  if (is.null(names(max_dist)) || any(names(max_dist) == "")) {
    allowed <- names(addr_fuzzy_match_default_max_dist())
    stop(
      "addr_fuzzy_match max distances must be a named vector. ",
      "Allowed fields are: ",
      paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }
  if (any(max_dist < 0)) {
    stop("addr_fuzzy_match max distances cannot be negative.", call. = FALSE)
  }
  allowed <- names(addr_fuzzy_match_default_max_dist())
  unknown <- setdiff(names(max_dist), allowed)
  if (length(unknown) > 0) {
    stop(
      "addr_fuzzy_match max distances include unknown fields: ",
      paste(unknown, collapse = ", "),
      ". Allowed fields are: ",
      paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

addr_fuzzy_match_resolve_max_dist <- function(addr_fields = NULL) {
  out <- addr_fuzzy_match_default_max_dist()
  if (!is.null(addr_fields)) {
    addr_fuzzy_match_validate_max_dist(addr_fields)
    out[names(addr_fields)] <- addr_fields
  }
  out
}
