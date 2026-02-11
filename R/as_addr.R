#' @include addr_part.R addr.R
NULL

#' Coerce to addr
#'
#' `as_addr()` converts other objects into `addr()` vectors.
#' See `?addr` for more details on its structure.
#' @section Methods implemented for:
#' - `character`: will be cleaned (if `clean = TRUE`) with `clean_address_text()`
#' and then tagged using `usaddress_tag()`; tags are normalized to abbreviations
#' by passing all `map_*` arguments to `addr_street()` or `addr_place()`
#' - `data.frame`: must have columns named according to fields in
#' `addr_number()`, `addr_street()`, or `addr_place()`; also passes
#' the `map_*` arguments to `addr_street()` and `addr_place()`
#' - `addr`: returned as-is
#' @param x object to coerce to an addr vector
#' @param ... additional arguments passed to methods
#' @export
#' @examples
#' as_addr(voter_addresses()[1:1000])
#'
#' data.frame(
#'   number_digits = c("290", "200"),
#'   street_name = c("Burnet", "Main"),
#'   street_posttype = c("Ave", "St"),
#'   place_name = c("Cincinnati", "Cincinnati"),
#'   place_state = c("OH", "OH"),
#'   place_zipcode = c("45229", "45220"),
#'   stringsAsFactors = FALSE
#' )|>
#'   as_addr()
as_addr <- S7::new_generic("as_addr", dispatch_args = "x")

#' @export
S7::method(as_addr, addr) <- function(x, ...) x

#' @export
S7::method(as_addr, S7::class_character) <- function(
  x,
  ...,
  clean = TRUE,
  map_state = TRUE,
  map_posttype = TRUE,
  map_directional = TRUE,
  map_pretype = TRUE
) {
  tags <- tag_usaddress(x, clean = clean)

  extract_tag <- function(tag_vec, label, collapse = " ") {
    if (length(tag_vec) == 1L && is.na(tag_vec)) {
      return(NA_character_)
    }
    if (length(tag_vec) == 0L) {
      return("")
    }
    vals <- unname(tag_vec[names(tag_vec) == label])
    if (length(vals) == 0L) {
      return("")
    }
    paste(vals, collapse = collapse)
  }

  prefix <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "AddressNumberPrefix",
    collapse = ""
  )
  digits <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "AddressNumber",
    collapse = ""
  )
  suffix <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "AddressNumberSuffix",
    collapse = ""
  )
  predirectional <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "StreetNamePreDirectional"
  )
  premodifier <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "StreetNamePreModifier"
  )
  pretype <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "StreetNamePreType"
  )
  name <- vapply(tags, extract_tag, character(1), label = "StreetName")
  posttype <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "StreetNamePostType"
  )
  postdirectional <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "StreetNamePostDirectional"
  )
  place_name <- vapply(tags, extract_tag, character(1), label = "PlaceName")
  place_state <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "StateName",
    collapse = ""
  )
  place_zip <- vapply(
    tags,
    extract_tag,
    character(1),
    label = "ZipCode",
    collapse = ""
  )

  addr(
    addr_number(prefix = prefix, digits = digits, suffix = suffix),
    addr_street(
      predirectional = predirectional,
      premodifier = premodifier,
      pretype = pretype,
      name = name,
      posttype = posttype,
      postdirectional = postdirectional,
      map_posttype = map_posttype,
      map_directional = map_directional,
      map_pretype = map_pretype
    ),
    addr_place(
      name = place_name,
      state = place_state,
      zipcode = place_zip,
      map_state = map_state
    )
  )
}

.data.frame <- S7::new_S3_class("data.frame")

#' @export
S7::method(as_addr, .data.frame) <-
  function(
    x,
    ...,
    map_state = TRUE,
    map_posttype = TRUE,
    map_directional = TRUE,
    map_pretype = TRUE
  ) {
    n <- nrow(x)
    col_or_na <- function(name) {
      if (name %in% names(x)) {
        as.character(x[[name]])
      } else {
        rep("", n)
      }
    }
    addr(
      addr_number(
        prefix = col_or_na("number_prefix"),
        digits = col_or_na("number_digits"),
        suffix = col_or_na("number_suffix")
      ),
      addr_street(
        predirectional = col_or_na("street_predirectional"),
        premodifier = col_or_na("street_premodifier"),
        pretype = col_or_na("street_pretype"),
        name = col_or_na("street_name"),
        posttype = col_or_na("street_posttype"),
        postdirectional = col_or_na("street_postdirectional"),
        map_posttype = map_posttype,
        map_directional = map_directional,
        map_pretype = map_pretype
      ),
      addr_place(
        name = col_or_na("place_name"),
        state = col_or_na("place_state"),
        zipcode = col_or_na("place_zipcode"),
        map_state = map_state
      )
    )
  }
