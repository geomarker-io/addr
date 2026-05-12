#' @include addr_helpers.R
NULL

addr_part <- S7::new_class("addr_part", package = NULL)

S7::method(as.data.frame, addr_part) <- function(x, ...) {
  out <- as.data.frame(S7::props(x))
  names(out) <- sprintf("%s_%s", which_addr_part(x), names(out))
  out
}

S7::method(as.character, addr_part) <- function(x, ...) {
  format(x)
}

S7::method(is.na, addr_part) <- function(x, ...) {
  !complete.cases(as.data.frame(x))
}

S7::method(`[`, addr_part) <- function(x, i, ...) {
  if (missing(i)) {
    return(x)
  }
  cnstr <- get(paste0("addr_", which_addr_part(x)))
  do.call(cnstr, lapply(S7::props(x), \(.) .[i]))
}

#' @method [<- addr_part
#' @export
`[<-.addr_part` <- function(x, i, ..., value) {
  if (!inherits(value, "addr_part") ||
    which_addr_part(value) != which_addr_part(x)) {
    stop(
      "`value` must be an addr_",
      which_addr_part(x),
      " vector",
      call. = FALSE
    )
  }
  if (length(list(...)) > 0L) {
    stop("addr_part vectors only support one-dimensional subassignment",
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

  cnstr <- get(paste0("addr_", which_addr_part(x)))
  do.call(cnstr, x_parts)
}

S7::method(format, addr_part) <- function(x, ...) {
  parts <- S7::props(x)
  lens <- vapply(parts, length, integer(1))
  target <- max(lens, 0L)
  if (target == 0L) {
    return(character(0))
  }
  parts <- lapply(parts, function(x) if (length(x) == 1L) rep(x, target) else x)
  cllps <- ifelse(which_addr_part(x) == "number", "", " ")
  vapply(
    seq_len(target),
    function(i) {
      vals <- vapply(parts, function(x) x[[i]], character(1))
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) == 0L) "" else paste(vals, collapse = cllps)
    },
    character(1)
  )
}

S7::method(length, addr_part) <- function(x, ...) {
  length(S7::props(x)[[1]])
}

S7::method(unique, addr_part) <- function(x, ...) {
  x[!duplicated(as.character(x))]
}

#' @rdname addr
#' @export
addr_number <- S7::new_class(
  "addr_number",
  package = NULL,
  parent = addr_part,
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
      .parent = addr_part,
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
      return("@digits must contain only numeric characters")
    }
    digits <- suppressWarnings(as.numeric(self@digits))
    out_of_range <- !is.na(self@digits) &
      self@digits != "" &
      (digits < 0 | digits > 999999)
    if (any(out_of_range)) {
      return("@digits must be between 0 and 999999")
    }
  }
)

#' @rdname addr
#' @export
addr_street <- S7::new_class(
  "addr_street",
  package = NULL,
  parent = addr_part,
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
    map_pretype = TRUE,
    map_ordinal = TRUE
  ) {
    stopifnot(
      "map_posttype must be TRUE or FALSE" = is.logical(map_posttype) &&
        length(map_posttype) == 1L &&
        !is.na(map_posttype),
      "map_directional must be TRUE or FALSE" = is.logical(map_directional) &&
        length(map_directional) == 1L &&
        !is.na(map_directional),
      "map_pretype must be TRUE or FALSE" = is.logical(map_pretype) &&
        length(map_pretype) == 1L &&
        !is.na(map_pretype),
      "map_ordinal must be TRUE or FALSE" = is.logical(map_ordinal) &&
        length(map_ordinal) == 1L &&
        !is.na(map_ordinal)
    )
    if (map_posttype) {
      posttype <- map_street_name_post_type(posttype)
    }
    if (map_pretype) {
      pretype <- map_street_name_pre_type(pretype)
    }
    if (map_directional) {
      predirectional <- map_direction(predirectional)
      postdirectional <- map_direction(postdirectional)
    }
    if (map_ordinal) {
      name <- map_ordinal_street_names(name)
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
      .parent = addr_part,
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
  parent = addr_part,
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
    stopifnot(
      "map_state must be TRUE or FALSE" = is.logical(map_state) &&
        length(map_state) == 1L &&
        !is.na(map_state)
    )
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
      !grepl("^[0-9]{5}$", self@zipcode) |
      grepl("^000", self@zipcode)
    if (any(bad)) {
      "@zipcode must be exactly five numeric digits and not lead with three zeros"
    }
  }
)

which_addr_part <- S7::new_generic("which_addr_part", "x")
S7::method(which_addr_part, addr_number) <- function(x) "number"
S7::method(which_addr_part, addr_street) <- function(x) "street"
S7::method(which_addr_part, addr_place) <- function(x) "place"
