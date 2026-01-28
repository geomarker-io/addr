#' Address class
#'
#' @include addr_helpers.R addr_number.R addr_street.R addr_place.R
#' @export
addr <- S7::new_class(
  "addr",
  package = NULL,
  properties = list(
    number = addr_number,
    street = addr_street,
    place = addr_place
  ),
  constructor = function(number, street, place) {
    if (missing(number) || missing(street) || missing(place)) {
      stop("addr requires number, street, and place", call. = FALSE)
    }
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
