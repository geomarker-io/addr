#' @rdname addr
#' @export
addr_place <- S7::new_class(
  "addr_place",
  package = NULL,
  properties = list(
    name = S7::class_character,
    state = S7::class_character,
    zipcode = S7::class_character
  ),
  constructor = function(
    name = NA_character_,
    state = NA_character_,
    zipcode = NA_character_,
    map_state = TRUE
  ) {
    if (isTRUE(map_state)) {
      state <- map_state_to_abbrev(state)
    }
    fields <- recycle_fields(
      list(name = name, state = state, zipcode = zipcode),
      "addr_place"
    )
    S7::new_object(
      S7::S7_object(),
      name = fields$name,
      state = fields$state,
      zipcode = fields$zipcode
    )
  },
  validator = function(self) {
    len_msg <- check_recyclable_lengths(
      self,
      c("name", "state", "zipcode"),
      "addr_place"
    )
    if (!is.null(len_msg)) {
      return(len_msg)
    }
    bad <- !is.na(self@zipcode) &
      self@zipcode != "" &
      !grepl("^[0-9]{5}$", self@zipcode)
    if (any(bad)) {
      "@zipcode must be exactly five numeric digits"
    }
  }
)

#' @method as.data.frame addr_place
#' @export
as.data.frame.addr_place <- function(x, ...) {
  data.frame(
    place_name = S7::prop(x, "name"),
    place_state = S7::prop(x, "state"),
    place_zipcode = S7::prop(x, "zipcode"),
    stringsAsFactors = FALSE
  )
}

S7::method(format, addr_place) <- function(x, ...) {
  format_addr_tokens(
    S7::prop(x, "name"),
    S7::prop(x, "state"),
    S7::prop(x, "zipcode")
  )
}

S7::method(print, addr_place) <- function(x, ...) {
  print_addr_vector(format(x, ...))
}
