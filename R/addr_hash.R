#' hash addr vectors
#'
#' Addr vectors are converted to character strings and hashed using the md5 algorithm from the digest package
#' @param x an addr vector (or character vector coerced to an addr vector)
#' @returns character vector
#' @export
#' @examples
#' addr_hash(c("3333 Burnet Ave Cincinnati OH 45229", "1324 Burnet Ave Cincinnati OH 45229"))
#' addr_hash(c("3333 Burnet Ave Cincinnati OH 45229", "3333 burnet Avenue Cincinnati OH 45229"))
addr_hash <- function(x) {
  rlang::check_installed("digest", "hash strings using the md5 algorithm")
  vapply(as_addr(x), \(.x) digest::digest(as.character(.x), algo = "md5", serialize = FALSE), character(1))
}
