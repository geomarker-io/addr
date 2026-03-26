#' clean address text
#'
#' remove excess whitespace; keep only letters, numbers, `#`, and `-`
#' @param x a vector of address character strings
#' @return a vector of cleaned addresses
#' @export
#' @examples
#' clean_address_text(c(
#'   "3333 Burnet Ave Cincinnati OH 45219",
#'   "33_33 Burnet Ave. Cincinnati OH 45219",
#'   "33\\33 B\"urnet Ave; Ci!ncinn&*ati OH 45219",
#'   "3333 Burnet Ave Cincinnati OH 45219",
#'   "33_33 Burnet Ave. Cincinnati OH 45219"
#' ))
clean_address_text <- function(x) {
  x <- gsub("\\\\|\"", "", x)
  x <- gsub("[^[:alnum:]# -]", "", x)
  trimws(gsub("[[:space:]]+", " ", x))
}
