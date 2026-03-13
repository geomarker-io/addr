# matching functions all have the same behavior:
# - return the matched entity (instead of its index value)
# - all take the inputs x and y
# - deduplicates computations on repeated components in x and y
# - match returns the best single result, always returning a vector of matches instead of a list with multiple possible matches

## match addr_street
# distance for non-ordinal streets = osa
# distance for ordinal streets = integer distance (and osa??)

#' Match addr_number vectors
#'
#' A single addr_number in y is chosen for each addr_number in x.
#' If exact matches (using `as.character`) are not found,
#' possible matches (within thresh OSA distance) are searched for in y.
#' If multiple matches are present in y, the best one is selected
#' based on the lowest absolute numeric difference with the @digits in x;
#' ties are broken by optimized string alignment (OSA) distances
#' and then preferring the lowest value sorted in Lexicographic
#' order with digits preceding alphabetic characters.
#'
#' addr_number objects with missing @digits or empty strings
#' for all of @prefix, @digits, @suffix are not matched and
#' returned as missing instead
#' @param x,y addr_number vectors to match
#' @param osa_max_dist integer maximum OSA distance to consider a match
#' @return an addr_number vector, the same length as x, that is the
#' best match in y for each addr_number code in x; if no best match
#' is found a missing value is returned (`addr_number()`)
#' @export
#' @examples
#'  x <- addr_number(
#'    prefix = "",
#'    digits = as.character(c(1, 10, 228, 11, 22, 22, 22, 10, 99897, NA)),
#'    suffix = ""
#'  )
#'
#' y <- addr_number(
#'   prefix = "",
#'   digits = as.character(c(12, 11, 10, 22)),
#'   suffix = ""
#' )
#'
#' match_addr_number(x, y)
#'
#' match_addr_number(x, y, osa_max_dist = 0L)
match_addr_number <- function(x, y, osa_max_dist = 1L) {
  stopifnot(
    "x must be an addr_number object" = inherits(x, "addr_number"),
    "y must be an addr_number object" = inherits(y, "addr_number"),
    "osa_max_dist must be an integer" = typeof(osa_max_dist) == "integer"
  )
  ux <- unique(x) # also gets rid of empty
  ux <- ux[!is.na(ux@digits)]
  uy <- unique(y)
  lkp <- lapply(
    seq_along(ux),
    function(.i) {
      if (as.character(ux[.i]) %in% as.character(uy)) {
        return(ux[.i])
      }
      if (osa_max_dist >= 1) {
        m <- uy[
          stringdist::stringdist(
            as.character(ux[.i]),
            as.character(uy),
            method = "osa"
          ) <=
            osa_max_dist
        ]
        if (length(m) == 0) {
          return(addr_number())
        }
        m_sort <-
          m[order(
            abs(as.integer(ux[.i]@digits) - as.integer(m@digits)),
            stringdist::stringdist(
              as.character(ux[.i]),
              as.character(m),
              method = "osa"
            ),
            sort(as.character(m))
          )]
        return(m_sort[1])
      }
      return(addr_number())
    }
  )
  names(lkp) <- as.character(ux)
  lkp[sapply(lkp, is.na)] <- NULL # remove unmatched
  out_l <- lkp[as.character(x)]
  empties <- which(sapply(out_l, is.null))
  out_l[empties] <- replicate(length(empties), addr_number())
  out <-
    do.call(rbind, lapply(out_l, as.data.frame)) |>
    vec_restore(to = addr_number())
  return(out)
}

#' Match ZIP codes
#'
#' A single ZIP code in y is chosen for each ZIP code
#' in x. By default, if exact matches are not found, common variants
#' of ZIP codes in x are searched for in y.
#' If multiple variants are present in y, the best one is selected
#' based on the lowest absolute numeric difference with the ZIP code in x;
#' ties are broken by OSA string distances and then preferring the
#' minimum number.
#' @param x,y character vectors of ZIP codes to match
#' @param zip_variants logical; fuzzy match to common variants of
#' x in y? (e.g., changing 4th or 5th digit)
#' @return a character vector, the same length as x, that is the
#' best match in y for each ZIP code in x
#' @export
#' @examples
#' match_zipcodes(
#'   c("45222", "45219", "45219", "45220", "45220", "", NA),
#'   c("42522", "45200", "45219", "45221", "45223", "45321", "")
#' )
#'
#' match_zipcodes(
#'   c("45222", "45219", "45219", "45220", "45220", "", NA),
#'   c("42522", "45200", "45219", "45221", "45223", "45321", ""),
#'   zip_variants = FALSE
#' )
match_zipcodes <- function(x, y, zip_variants = TRUE) {
  ux <- unique(addr_place(zipcode = x)@zipcode)
  uy <- unique(addr_place(zipcode = y)@zipcode)
  lkp <- vapply(
    ux,
    FUN = \(xz) {
      if (xz %in% uy) {
        return(xz)
      }
      if (zip_variants) {
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
      }
      return(NA_character_)
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
