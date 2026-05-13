#' @include addr_part.R addr.R
NULL

#' Coerce to addr
#'
#' `as_addr()` converts other objects into `addr()` vectors.
#' See `?addr` for more details on its structure.
#' @section Methods implemented for:
#' - `character`: will be cleaned (if `clean = TRUE`) with `clean_address_text()`
#' and then tagged using `usaddress_tag()`; tags are normalized to abbreviations
#' by passing all `map_*` arguments to `addr_street()` or `addr_place()`;
#' ZIP codes parsed with more than five characters are truncated
#' with a warning; non-numeric characters in parsed
#' address number digits will be removed with a warning; parsed address number
#' digits greater than 999999 are truncated to the first six digits with a
#' warning
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
  stopifnot(
    "clean must be TRUE or FALSE" = is.logical(clean) &&
      length(clean) == 1L &&
      !is.na(clean),
    "map_state must be TRUE or FALSE" = is.logical(map_state) &&
      length(map_state) == 1L &&
      !is.na(map_state),
    "map_posttype must be TRUE or FALSE" = is.logical(map_posttype) &&
      length(map_posttype) == 1L &&
      !is.na(map_posttype),
    "map_directional must be TRUE or FALSE" = is.logical(map_directional) &&
      length(map_directional) == 1L &&
      !is.na(map_directional),
    "map_pretype must be TRUE or FALSE" = is.logical(map_pretype) &&
      length(map_pretype) == 1L &&
      !is.na(map_pretype)
  )
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
    paste(unique(vals), collapse = collapse)
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

  bad_digits <- which(!grepl("^[0-9]+$", digits) & !digits == "")
  if (length(bad_digits) > 0) {
    warning(
      "Removing non-numeric characters from parsed address number digits in ",
      length(bad_digits),
      " addresses.",
      call. = FALSE
    )
    digits[bad_digits] <- gsub("\\D", "", digits[bad_digits], perl = TRUE)
  }

  digits_num <- suppressWarnings(as.numeric(digits))
  long_digits <- which(!is.na(digits_num) & digits_num > 999999)

  if (length(long_digits) > 0) {
    warning(
      "Truncating ",
      length(long_digits),
      " parsed address number digits greater than 999999 to the first ",
      "six digits.",
      call. = FALSE
    )
    digits[long_digits] <- substr(digits[long_digits], 1, 6)
  }

  bad_zips <- which(nchar(place_zip) > 5)

  if (length(bad_zips) > 0) {
    warning(
      "Truncating ",
      length(bad_zips),
      " parsed ZIP codes to the first five characters.",
      call. = FALSE
    )
    place_zip[bad_zips] <- substr(place_zip[bad_zips], 1, 5)
  }

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
    stopifnot(
      "map_state must be TRUE or FALSE" = is.logical(map_state) &&
        length(map_state) == 1L &&
        !is.na(map_state),
      "map_posttype must be TRUE or FALSE" = is.logical(map_posttype) &&
        length(map_posttype) == 1L &&
        !is.na(map_posttype),
      "map_directional must be TRUE or FALSE" = is.logical(map_directional) &&
        length(map_directional) == 1L &&
        !is.na(map_directional),
      "map_pretype must be TRUE or FALSE" = is.logical(map_pretype) &&
        length(map_pretype) == 1L &&
        !is.na(map_pretype)
    )
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
