#' @include addr_part.R
NULL

canonicalize_addr_number <- function(x) {
  if (addr_part_is_uppercase(x)) {
    return(x)
  }
  addr_number(
    prefix = x@prefix,
    digits = x@digits,
    suffix = x@suffix
  )
}

canonicalize_addr_street <- function(x) {
  if (addr_part_is_uppercase(x)) {
    return(x)
  }
  addr_street(
    predirectional = x@predirectional,
    premodifier = x@premodifier,
    pretype = x@pretype,
    name = x@name,
    posttype = x@posttype,
    postdirectional = x@postdirectional,
    map_posttype = FALSE,
    map_directional = FALSE,
    map_pretype = FALSE,
    map_ordinal = FALSE
  )
}

canonicalize_addr_place <- function(x) {
  if (addr_part_is_uppercase(x)) {
    return(x)
  }
  addr_place(
    name = x@name,
    state = x@state,
    zipcode = x@zipcode,
    map_state = FALSE
  )
}

addr_is_uppercase <- function(x) {
  all(vapply(S7::props(x), addr_part_is_uppercase, logical(1)))
}

#' addr classes
#'
#' @description
#' The structures for `addr()` and the `addr_` classes are
#' derived as a subset of the United States Thoroughfare, Landmark, and Postal
#' Address Data Standard that is relevant for residential, numbered thoroughfare
#' addresses:
#'
#' ```
#'   Address
#'    +-- AddressNumber
#'    |   +-- AddressNumberPrefix
#'    |   +-- AddressNumber
#'    |   +-- AddressNumberSuffix
#'    +-- StreetName
#'    |   +-- StreetNamePreModifier
#'    |   +-- StreetNamePreDirectional
#'    |   +-- StreetNamePreType
#'    |   +-- StreetName
#'    |   +-- StreetNamePostType
#'    |   +-- StreetNamePostDirectional
#'    +-- Place
#'        +-- PlaceName
#'        +-- StateName
#'        +-- ZipCode
#' ```
#'`addr()` combines `addr_number()`, `addr_street()`, and `addr_place()` into a
#' single addr vector:
#'
#' ```
#' <addr>
#'  @ number: <addr_number>
#'  .. @ prefix
#'  .. @ digits
#'  .. @ suffix
#'  @ street: <addr_street>
#'  .. @ predirectional
#'  .. @ premodifier
#'  .. @ pretype
#'  .. @ name
#'  .. @ posttype
#'  .. @ postdirectional
#'  @ place : <addr_place>
#'  .. @ name
#'  .. @ state
#'  .. @ zipcode
#' ```
#'
#' @details
#' All field values must be character vectors of at least length one
#' (including missing values). Length-one fields are recycled to match the
#' length of other fields.
#'
#' All letters in stored `addr` and `addr_` fields are converted to uppercase.
#' The `map_*` arguments control abbreviation mapping, not this uppercase
#' canonicalization.
#'
#' @param prefix address number prefix, often a fractional or grid component
#' @param digits primary street number for the address; must be between 0 and
#' 999999
#' @param suffix address number suffix, often a letter or unit-like component
#' @param predirectional direction before the street name
#' @param premodifier descriptive modifier before the street name
#' @param pretype street type or classification before the street name
#' @param name street name, or city/town/municipality name for `addr_place()`
#' @param posttype street type or classification after the street name
#' @param postdirectional direction after the street name
#' @param state state or territory abbreviation
#' @param zipcode ZIP code (must be five digits not starting with "000")
#' @param map_posttype logical; map posttype to abbreviations?
#' @param map_directional logical; map pre- and post-directional
#' to abbreviations?
#' @param map_pretype logical; map pretype to abbreviations?
#' @param map_state logical; map state to abbreviations?
#' @param map_ordinal logical; map ordinal street names to abbreviations?
#' @param number an addr_number vector
#' @param street an addr_street vector
#' @param place an addr_place vector
#' @returns An addr, addr_number, addr_street, or addr_place vector
#' @export
#' @examples
#' # define a new addr_number vector
#' addr_number(digits = "290")
#' addr_number(prefix = "N", digits = "290", suffix = "A")
#'
#' # define a new addr_street vector
#' addr_street(name = "Burnet", posttype = "Ave")
#'
#' # street names are automatically mapped to abbreviations
#' addr_street(predirectional = "North", name = "Fifth", posttype = "Street")
#'
#' # define a new addr_place vector
#' addr_place(name = "Cincinnati", state = "OH", zipcode = "45220")
#'
#' # define a new addr vector
#' addr(
#'   addr_number(digits = "290"),
#'   addr_street(name = "Burnet", posttype = "Ave"),
#'   addr_place(name = "Cincinnati", state = "OH", zipcode = "45229")
#' )
#'
#' # define a more complicated addr vector
#' # and explicitly specify empty components to avoid NA
#' addr(
#'   addr_number(prefix = "", digits = "200", suffix = ""),
#'   addr_street(
#'     predirectional = "west",
#'     premodifier = "Old",
#'     pretype = "US",
#'     name = "50",
#'     posttype = "avenue",
#'     postdirectional = "east",
#'     map_directional = TRUE,
#'     map_pretype = TRUE,
#'     map_posttype = TRUE
#'   ),
#'   addr_place(name = "Cincinnati", state = "ohio", zipcode = "45220")
#' )
#'
#' # addr_* vectors are recycled and omitted fields are missing
#' addr(
#'   addr_number(digits = c("290", "200", "3333", "111")),
#'   addr_street(
#'     name = c("Burnet", "Main", "Ludlow", "State Route 32"),
#'     posttype = c("Ave", "St", "Ave", NA_character_)
#'   ),
#'   addr_place(name = "Cincinnati", state = "OH")
#' )
addr <- S7::new_class(
  "addr",
  package = NULL,
  properties = list(
    number = addr_number,
    street = addr_street,
    place = addr_place
  ),
  constructor = function(
    number = addr_number(),
    street = addr_street(),
    place = addr_place()
  ) {
    number <- canonicalize_addr_number(number)
    street <- canonicalize_addr_street(street)
    place <- canonicalize_addr_place(place)
    lens <- c(
      number = length(number@digits),
      street = length(street@name),
      place = length(place@zipcode)
    )
    target <- max(lens, 0L)
    if (target == 0L) {
      return(S7::new_object(
        S7::S7_object(),
        number = number,
        street = street,
        place = place
      ))
    }
    if (any(lens == 0L)) {
      stop(
        sprintf(
          "addr components must all be length 0 or length 1/%d",
          target
        ),
        call. = FALSE
      )
    }
    if (!all(lens %in% c(1L, target))) {
      stop(
        sprintf(
          "addr components must have length 1 or %d for recycling",
          target
        ),
        call. = FALSE
      )
    }
    if (lens[["number"]] == 1L && target > 1L) {
      number <-
        S7::props(number) |>
        lapply(rep, target) |>
        do.call(addr_number, args = _)
    }
    if (lens[["street"]] == 1L && target > 1L) {
      street_args <- S7::props(street) |>
        lapply(rep, target)
      street <- do.call(
        addr_street,
        c(
          street_args,
          list(
            map_posttype = FALSE,
            map_directional = FALSE,
            map_pretype = FALSE,
            map_ordinal = FALSE
          )
        )
      )
    }
    if (lens[["place"]] == 1L && target > 1L) {
      place_args <- S7::props(place) |>
        lapply(rep, target)
      place <- do.call(
        addr_place,
        c(place_args, list(map_state = FALSE))
      )
    }
    S7::new_object(
      S7::S7_object(),
      number = number,
      street = street,
      place = place
    )
  },
  validator = function(self) {
    lens <- c(
      number = length(self@number),
      street = length(self@street),
      place = length(self@place)
    )
    if (length(unique(lens)) > 1L) {
      return(
        sprintf(
          paste0(
            "addr components must all have equal lengths; ",
            "got number=%d, street=%d, place=%d"
          ),
          lens[["number"]],
          lens[["street"]],
          lens[["place"]]
        )
      )
    }
    if (!addr_is_uppercase(self)) {
      return("addr components must contain only uppercase letters")
    }
  }
)

#' @method as.data.frame addr
#' @export
S7::method(as.data.frame, addr) <- function(x, ...) {
  number_df <- as.data.frame(x@number)
  street_df <- as.data.frame(x@street)
  place_df <- as.data.frame(x@place)
  data.frame(
    number_df,
    street_df,
    place_df,
    stringsAsFactors = FALSE
  )
}

S7::method(format, addr) <- function(x, ...) {
  parts <- vapply(S7::props(x), format, character(length(x)))
  if (is.null(nrow(parts))) {
    parts <- t(as.matrix(parts))
  }
  out <- apply(parts, 1, paste, collapse = " ", simplify = TRUE)
  gsub(" +", " ", trimws(out))
}

S7::method(as.character, addr) <- function(x, ...) {
  format(x)
}

S7::method(as.list, addr) <- function(x, ...) {
  lapply(seq_len(length(x)), function(i) x[i])
}

S7::method(is.na, addr) <- function(x, ...) {
  !complete.cases(as.data.frame(x))
}

S7::method(length, addr) <- function(x, ...) {
  length(x@street)
}

S7::method(`[`, addr) <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }
  do.call(addr, lapply(S7::props(x), `[`, i, ...))
}

#' @method [<- addr
#' @export
`[<-.addr` <- function(x, i, ..., value) {
  if (!inherits(value, "addr")) {
    stop("`value` must be an addr vector", call. = FALSE)
  }
  if (length(list(...)) > 0L) {
    stop(
      "addr vectors only support one-dimensional subassignment",
      call. = FALSE
    )
  }

  x_parts <- S7::props(x)
  value_parts <- S7::props(value)
  has_i <- !missing(i)
  x_parts <- Map(
    function(old, new) {
      if (has_i) {
        old[i] <- new
      } else {
        old[] <- new
      }
      old
    },
    x_parts,
    value_parts
  )

  do.call(addr, x_parts)
}

S7::method(unique, addr) <- function(x, ...) {
  x[!duplicated(as.character(x))]
}
