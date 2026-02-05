addr_part <- S7::new_class("addr_part", abstract = TRUE)

#' @rdname addr
#' @export
addr_number <- S7::new_class(
  "addr_number",
  package = NULL,
  properties = list(
    prefix = S7::class_character,
    digits = S7::class_character,
    suffix = S7::class_character
  ),
  constructor = function(
    prefix = NA_character_,
    digits = NA_character_,
    suffix = NA_character_
  ) {
    fields <- recycle_fields(
      list(prefix = prefix, digits = digits, suffix = suffix),
      "addr_number"
    )
    S7::new_object(
      addr_part,
      prefix = fields$prefix,
      digits = fields$digits,
      suffix = fields$suffix
    )
  },
  validator = function(self) {
    len_msg <- check_recyclable_lengths(
      self,
      c("prefix", "digits", "suffix"),
      "addr_number"
    )
    if (!is.null(len_msg)) {
      return(len_msg)
    }
    bad <- !is.na(self@digits) &
      self@digits != "" &
      !grepl("^[0-9]+$", self@digits)
    if (any(bad)) {
      "@digits must contain only numeric characters"
    }
  }
)

#' @rdname addr
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
    predirectional = NA_character_,
    premodifier = NA_character_,
    pretype = NA_character_,
    name = NA_character_,
    posttype = NA_character_,
    postdirectional = NA_character_,
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
      addr_part,
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
      addr_part,
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
