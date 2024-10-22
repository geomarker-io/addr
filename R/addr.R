#' Create a new addr vector
#'
#' An addr vector is created by converting messy, real-world mailing addresses in a
#' character vector into a list of standardized address tags that behaves like a vector.
#' `addr()` (and `as_addr()`) vectors are a list of address tags under the hood, constructed
#' by tagging address components using `addr_tag()` and combining them into specific fields:
#' - `street_number`: `AddressNumber`
#' - `street_name`: `StreetNamePreType`, `StreetNamePreDirectional`, `StreetName`
#' - `street_type`: `StreetNamePostType`, `StreetNamePostDirectional`
#' - `city`: `PlaceName`
#' - `state`: `StateName`
#' - `zip_code`: `ZipCode`
#'
#' @details
#' In addition to the cleaning steps described in the arguments, the street number is coerced
#' to a numeric after removing non-numeric characters.
#' See `addr_tag()` for details on address component tagging.
#'
#' In the case of an address having more than one word for a tag (e.g., "Riva Ridge" for `StreetName`),
#' then these are concatenated together, separated by a space in the order they appeared in the address.
#' @param x a character vector of address strings
#' @param clean_address_text logical; use `clean_address_text()` to clean address text prior to tagging?
#' @param expand_street_type logical; use `expand_post_type()` to expand `StreetNamePostType` tags? (e.g., "Ave" -> "Avenue")
#' @param abbrev_cardinal_dir logical; abbreviate cardinal directions? (e.g., "west" -> "w")
#' @param clean_zip_code logical; remove any non-digit (or hyphen) characters and truncate tagged ZIP Code to 5 characters?
#' @export
#' @examples
#' as_addr(c("3333 Burnet Ave Cincinnati OH 45229", "1324 Burnet Ave Cincinnati OH 45229"))
addr <- function(x = character(),
                 clean_address_text = TRUE,
                 expand_street_type = TRUE,
                 abbrev_cardinal_dir = TRUE,
                 clean_zip_code = TRUE) {
  x_tags <- addr_tag(vec_cast(x, character()), clean_address_text = clean_address_text)
  safe_extract_one <- function(x, name) {
    out <- paste(x[names(x) %in% name], collapse = " ")
    if (out == "") out <- NA
    return(out)
  }
  toi_names <- list(
    "street_number" = c("AddressNumber"),
    "street_name" = c("StreetNamePreType", "StreetNamePreDirectional", "StreetName"),
    "street_type" = c("StreetNamePostType", "StreetNamePostDirectional"),
    "city" = c("PlaceName"),
    "state" = c("StateName"),
    "zip_code" = c("ZipCode")
  )
  toi <-
    purrr::map(toi_names, \(.) purrr::map_chr(x_tags, safe_extract_one, .)) |>
    stats::setNames(names(toi_names))
  if (abbrev_cardinal_dir) {
    # replace cardinal dir at beginning of line with a space after or as its own word
    toi$street_name <-
      stringr::str_replace_all(
        toi$street_name,
        stringr::regex(c(
          "^east " = "e ", "^west " = "w ", "^north " = "n ", "^south " = "s ",
          " east " = " e ", " west " = " w ", " north " = " n ", " south " = " s "
        ), ignore_case = TRUE)
      )
  }
  if (expand_street_type) {
    toi$street_type <- expand_post_type(toi$street_type)
  }
  if (clean_zip_code && "zip_code" %in% names(toi_names)) {
    toi$zip_code <- gsub("[^0-9-]", "", toi$zip_code)
    toi$zip_code <- substr(toi$zip_code, 1, 5)
  }
  new_addr(
    street_number = vec_cast(as.numeric(stringr::str_remove(toi$street_number, "[^0-9.-]")), numeric()),
    street_name = vec_cast(toi$street_name, character()),
    street_type = vec_cast(toi$street_type, character()),
    city = vec_cast(toi$city, character()),
    state = vec_cast(toi$state, character()),
    zip_code = vec_cast(toi$zip_code, character())
  )
}

#' Coerce a character vector to an addr vector
#'
#' @details Compared to using `addr()`, `as_addr()` processes input character strings such that
#' parsing is done once per unique input, usually speeding up address parsing in real-world
#' datasets where address strings are often duplicated across observations.
#' @param ... used to pass arguments in `as_addr` to underlying `addr()`
#' @rdname addr
#' @export
as_addr <- function(x, ...) {
  if (inherits(x, "addr")) {
    return(x)
  }
  ux <- unique(x)
  u_out <- as.list(addr(ux, ...))
  names(u_out) <- ux
  purrr::list_c(u_out[x])
}


new_addr <- function(street_number = numeric(),
                     street_name = character(),
                     street_type = character(),
                     city = character(),
                     state = character(),
                     zip_code = character()) {
  if (!rlang::is_double(street_number)) rlang::abort("`street_number` must be a numeric vector.")
  if (!rlang::is_character(street_name)) rlang::abort("`street_name` must be a character vector.")
  if (!rlang::is_character(street_type)) rlang::abort("`street_type` must be a character vector.")
  if (!rlang::is_character(city)) rlang::abort("`city` must be a character vector.")
  if (!rlang::is_character(state)) rlang::abort("`state` must be a character vector.")
  if (!rlang::is_character(zip_code)) rlang::abort("`zip_code` must be a character vector.")
  if (any(grepl("[^0-9-]", zip_code))) rlang::abort("`zip_code` must contain only digit (or hyphen) characters.")
  if (any(!is.na(zip_code) & nchar(zip_code) > 5)) rlang::abort("`zip_code` must not be longer than five characters")
  c(street_number, street_name, street_type, city, state, zip_code) %<-%
    vec_recycle_common(street_number, street_name, street_type, city, state, zip_code)
  new_rcrd(
    list(
      street_number = street_number,
      street_name = tolower(street_name),
      street_type = tolower(street_type),
      city = tolower(city),
      state = tolower(state),
      zip_code = zip_code
    ),
    class = "addr"
  )
}

#' @importFrom methods setOldClass
# for compatibility with the S4 system
methods::setOldClass(c("addr", "vctrs_vctr"))

#' @export
format.addr <- function(x, ...) {
  xd <- vctrs::vec_data(x)
  paste(
    cli::style_bold(cli::col_magenta(xd$street_number)),
    cli::style_underline(cli::col_red(stringr::str_to_title(xd$street_name))),
    cli::style_dim(cli::col_red(stringr::str_to_title(xd$street_type))),
    cli::style_underline(cli::col_grey(stringr::str_to_title(xd$city))),
    cli::style_italic(cli::col_grey(stringr::str_to_upper(xd$state))),
    cli::style_underline(cli::col_blue(xd$zip_code))
  )
}

#' @export
print.addr <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
as.character.addr <- function(x, ...) {
  xd <- vec_data(x)
  out <- paste(
    xd$street_number,
    stringr::str_to_title(xd$street_name),
    stringr::str_to_title(xd$street_type),
    stringr::str_to_title(xd$city),
    stringr::str_to_upper(xd$state),
    xd$zip_code
  )
  gsub(" NA", "", out, fixed = TRUE)
}

#' @export
as.data.frame.addr <- function(x, ...) {
  vctrs::vec_data(x)
}

#' @export
vec_ptype_abbr.addr <- function(x, ...) "addr"

#' @export
vec_ptype_full.addr <- function(x, ...) "addr"
