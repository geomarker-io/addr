#' Address number class
#'
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
    prefix = character(0),
    digits = character(0),
    suffix = character(0)
  ) {
    fields <- recycle_fields(
      list(prefix = prefix, digits = digits, suffix = suffix),
      "addr_number"
    )
    S7::new_object(
      S7::S7_object(),
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
    bad <- !is.na(self@digits) & !grepl("^[0-9]+$", self@digits)
    if (any(bad)) {
      "@digits must contain only numeric characters"
    }
  }
)

#' @method as.data.frame addr_number
#' @export
as.data.frame.addr_number <- function(x, ...) {
  data.frame(
    number_prefix = S7::prop(x, "prefix"),
    number_digits = S7::prop(x, "digits"),
    number_suffix = S7::prop(x, "suffix"),
    stringsAsFactors = FALSE
  )
}

S7::method(format, addr_number) <- function(x, ...) {
  format_addr_number_tokens(
    S7::prop(x, "prefix"),
    S7::prop(x, "digits"),
    S7::prop(x, "suffix")
  )
}

S7::method(print, addr_number) <- function(x, ...) {
  print_addr_vector(format(x, ...))
}
