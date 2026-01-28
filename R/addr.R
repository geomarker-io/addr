#' addr classes
#'
#' `addr()` combines `addr_number()`, `addr_street()`, and `addr_place()` into a
#' single address object. The structures for `addr()` and the `addr_` classes are
#' derived as a subset of the United States Thoroughfare, Landmark, and Postal
#' Address Data Standard that is relevant for residential thoroughfare
#' addresses:
#' - `addr_number()` objects contain fields for "AddressNumberPrefix",
#' "AddressNumber", and "AddressNumberSuffix".
#' - `addr_street()` objects contain fields for "StreetNamePreDirectional",
#' "StreetNamePreModifier", "StreetNamePreType", "StreetName",
#' "StreetNamePostType", and "StreetNamePostDirectional".
#' - `addr_place()` objects contain fields for "PlaceName", "StateName",
#' and "ZipCode".
#'
#' All field values must must be a character vector of at least length one
#' (including missing values); length one fields will be
#' recycled to match the length of other fields.
#'
#' @include addr_helpers.R addr_number.R addr_street.R addr_place.R
#' @param prefix (often fractional) appears before digits
#' @param digits primary street number for the address
#' @param suffix (often letter/part) attached after digits
#' @param predirectional direction before name
#' @param premodifier descriptive modifier before name
#' @param pretype type/classification before name
#' @param name core street name (excluding type/directionals)
#' @param posttype type/classification after name
#' @param postdirectional direction after name
#' @param name city, town, or municipality name
#' @param state state or territory abbreviation
#' @param map_posttype logical; map posttype to abbreviations?
#' @param map_directional logical; map pre- and post-directional
#' to abbreviations?
#' @param map_pretype logical; map pretype to abbreviations?
#' @param map_state logical; map state to abbreviations?
#' @param number an addr_number object
#' @param street an addr_street_object
#' @param place an addr_street_object
#' @returns An addr, addr_number, addr_street, or addr_place object
#' @export
#' @examples
#' # define a new address number object
#' addr_number(digits = "290")
#' addr_number(prefix = "N", digits = "290", suffix = "A")
#'
#' # define a new address street object
#' addr_street(name = "Burnet", posttype = "Ave")
#'
#' # define a new address place object
#' addr_place(name = "Cincinnati", state = "OH", zipcode = "45220")
#'
#' # define a new addr object
#' addr(
#'   addr_number(digits = "290"),
#'   addr_street(name = "Burnet", posttype = "Ave"),
#'   addr_place(name = "Cincinnati", state = "OH", zipcode = "45229")
#' )
#'
#' # define a more complicated addr object
#' addr(
#'   addr_number(digits = "200"),
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
#' # addr_* objects are recycled and omitted fields are missing
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
    len_number <- addr_component_length(number, c("prefix", "digits", "suffix"))
    len_street <- addr_component_length(
      street,
      c(
        "predirectional",
        "premodifier",
        "pretype",
        "name",
        "posttype",
        "postdirectional"
      )
    )
    len_place <- addr_component_length(
      place,
      c("name", "state", "zipcode")
    )
    lens <- c(number = len_number, street = len_street, place = len_place)
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
    if (len_number == 1L && target > 1L) {
      number <- recycle_addr_component(
        number,
        c("prefix", "digits", "suffix"),
        target,
        addr_number
      )
    }
    if (len_street == 1L && target > 1L) {
      street <- recycle_addr_component(
        street,
        c(
          "predirectional",
          "premodifier",
          "pretype",
          "name",
          "posttype",
          "postdirectional"
        ),
        target,
        addr_street
      )
    }
    if (len_place == 1L && target > 1L) {
      place <- recycle_addr_component(
        place,
        c("name", "state", "zipcode"),
        target,
        addr_place
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
    len_number <- addr_component_length(
      self@number,
      c("prefix", "digits", "suffix")
    )
    len_street <- addr_component_length(
      self@street,
      c(
        "predirectional",
        "premodifier",
        "pretype",
        "name",
        "posttype",
        "postdirectional"
      )
    )
    len_place <- addr_component_length(
      self@place,
      c("name", "state", "zipcode")
    )
    lens <- c(number = len_number, street = len_street, place = len_place)
    target <- max(lens, 0L)
    if (target == 0L) {
      return(NULL)
    }
    if (any(lens == 0L)) {
      return(
        sprintf(
          "addr components must all be length 0 or length 1/%d",
          target
        )
      )
    }
    if (!all(lens %in% c(1L, target))) {
      return(
        sprintf(
          "addr components must have length 1 or %d for recycling",
          target
        )
      )
    }
  }
)

#' @method as.data.frame addr
#' @export
as.data.frame.addr <- function(x, ...) {
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
  format_addr_tokens(
    format(x@number),
    format(x@street),
    format(x@place)
  )
}

S7::method(print, addr) <- function(x, ...) {
  print_addr_vector(format(x, ...))
}
