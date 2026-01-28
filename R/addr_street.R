#' Address street class
#'
#' @export
addr_street <- S7::new_class(
  "addr_street",
  package = NULL,
  properties = list(
    predirectional = S7::class_character,
    premodifier = S7::class_character,
    pretype = S7::class_character,
    name = S7::class_character,
    posttype = S7::class_character,
    postdirectional = S7::class_character
  ),
  constructor = function(
    predirectional = character(0),
    premodifier = character(0),
    pretype = character(0),
    name = character(0),
    posttype = character(0),
    postdirectional = character(0),
    map_posttype = TRUE,
    map_directional = TRUE,
    map_pretype = TRUE
  ) {
    if (isTRUE(map_posttype)) {
      posttype <- map_street_name_post_type(posttype)
    }
    if (isTRUE(map_pretype)) {
      pretype <- map_street_name_pre_type(pretype)
    }
    if (isTRUE(map_directional)) {
      predirectional <- map_direction(predirectional)
      postdirectional <- map_direction(postdirectional)
    }
    fields <- recycle_fields(
      list(
        predirectional = predirectional,
        premodifier = premodifier,
        pretype = pretype,
        name = name,
        posttype = posttype,
        postdirectional = postdirectional
      ),
      "addr_street"
    )
    S7::new_object(
      S7::S7_object(),
      predirectional = fields$predirectional,
      premodifier = fields$premodifier,
      pretype = fields$pretype,
      name = fields$name,
      posttype = fields$posttype,
      postdirectional = fields$postdirectional
    )
  },
  validator = function(self) {
    check_recyclable_lengths(
      self,
      c(
        "predirectional",
        "premodifier",
        "pretype",
        "name",
        "posttype",
        "postdirectional"
      ),
      "addr_street"
    )
  }
)

#' @method as.data.frame addr_street
#' @export
as.data.frame.addr_street <- function(x, ...) {
  data.frame(
    street_predirectional = S7::prop(x, "predirectional"),
    street_premodifier = S7::prop(x, "premodifier"),
    street_pretype = S7::prop(x, "pretype"),
    street_name = S7::prop(x, "name"),
    street_posttype = S7::prop(x, "posttype"),
    street_postdirectional = S7::prop(x, "postdirectional"),
    stringsAsFactors = FALSE
  )
}

S7::method(format, addr_street) <- function(x, ...) {
  format_addr_tokens(
    S7::prop(x, "predirectional"),
    S7::prop(x, "premodifier"),
    S7::prop(x, "pretype"),
    S7::prop(x, "name"),
    S7::prop(x, "posttype"),
    S7::prop(x, "postdirectional")
  )
}

S7::method(print, addr_street) <- function(x, ...) {
  print_addr_vector(format(x, ...))
}
