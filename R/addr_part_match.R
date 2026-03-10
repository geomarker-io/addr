# match addr_number
# integer distance on digits
# osa distance on entire object
# soundex??

## match addr_street
# distance for non-ordinal streets = osa
# distance for ordinal streets = integer distance (and osa??)

## match addr_place@zipcode
# 1. exact match
# 2. candidates with same first 3 digits
# first digit is broad region: must match exactly
# 2nd-3rd digits sectional center facility area: must match exactly
# 4th-5th digits local delivery zone:

# my_addr <-
#   tibble::tibble(
#     address = voter_addresses()[1:1000],
#     addr = as_addr(address),
#     id = sprintf("id_%04d", seq_len(1000))
#   )
# the_addr <- nad_example_data()

# x <- unique(my_addr$addr@place@zipcode)
# y <- unique(the_addr$nad_addr@place@zipcode)

#' Match ZIP codes
#'
#' A single ZIP code in y is chosen for each ZIP code
#' in x. If exact matches are not found, common variants
#' of ZIP codes in x are searched for in y.
#' If multiple variants are present in y, the best one is selected
#' based on the lowest absolute numeric difference with the ZIP code in x;
#' ties are broken by OSA string distances and then preferring the minimum number
#' @param x,y character vectors of ZIP codes to match
#' @return a character vector, the same length as x, that is the
#' best match in y for each ZIP code in x
#' @export
#' @examples
#' match_zipcodes(
#'   c("45222", "45219", "45219", "45220", "45220", "", NA),
#'   c("42522", "45200", "45219", "45221", "45223", "45321", "")
#' )
match_zipcodes <- function(x, y) {
  ux <- unique(addr_place(zipcode = x)@zipcode)
  uy <- unique(addr_place(zipcode = y)@zipcode)
  lkp <- vapply(
    ux,
    FUN = \(xz) {
      if (xz %in% uy) {
        return(xz)
      }
      xzv <- zipcode_variant(xz)
      m <- xzv[xzv %in% uy]
      if (length(m) == 0) {
        return(NA_character_)
      }
      m <- m[order(
        abs(as.integer(xz) - as.integer(m)),
        stringdist::stringdist(xz, m, method = "osa"),
        as.integer(m)
      )]
      return(m[1])
    },
    FUN.VALUE = character(1),
    USE.NAMES = TRUE
  )
  out <- lkp[x]
  names(out) <- NULL
  out
}

zipcode_variant <- function(x) {
  stopifnot(typeof(x) == "character", length(x) == 1L)
  x <- addr_place(zipcode = x)@zipcode
  if (is.na(x)) {
    return(NA_character_)
  }
  if (x == "") {
    return("")
  }
  z <- strsplit(x, "")[[1]]
  swap <- paste0(z[c(1, 3, 2, 4, 5)], collapse = "")
  sub4 <- paste0(
    paste0(z[1:3], collapse = ""),
    as.character(0:9),
    z[5]
  )
  sub5 <- paste0(
    paste0(z[1:4], collapse = ""),
    as.character(0:9)
  )
  plus1 <- as.character(as.integer(x) + 1)
  minus1 <- as.character(as.integer(x) - 1)
  uout <- unique(c(swap, sub4, sub5, plus1, minus1))
  out <- as.character(sort(as.integer(uout[uout != x])))
  sprintf("%05s", out)
}
