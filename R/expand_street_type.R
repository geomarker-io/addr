#' Expand street name post type
#'
#' Abbreviations of street type (e.g., "Ave", "St") are converted to
#' expanded versions (e.g., "Avenue", "Street").
#' @param x character vector of `StreetnamePostType` abbreviations
#' @return a character vector of the same length containing the expanded street name post type
#' @export
#' @examples
#' expand_post_type(c("ave", "av", "Avenue", "tl"))
expand_post_type <- function(x) {
  x <- tolower(x)
  lookup <-
    list(
      "avenue" = c("av", "ave", "avnue"),
      "boulevard" = c("blvd", "blvrd", "bv"),
      "circle" = c("cir", "cr", "crcl"),
      "court" = c("ct"),
      "crescent" = c("cres"),
      "drive" = c("dr", "drv"),
      "highway" = c("hgwy", "hw", "hway", "hwy", "hywy"),
      "lane" = c("ln"),
      ## "Path" = c("path"),
      "parkway" = c("pkwy"),
      "pike" = c("pk"),
      # 2406 South Rd, 2410 South Rd, 2268 South Rd
      # 3624 Westwood Northern Bv
      # 5105 State Route 128
      # 214 FOURTEENTH ST ST
      # 3040 SOUTH RD, 3058 SOUTH RD
      # 1312 BROADWAY
      "place" = c("pl"),
      "point" = c("pointe", "pt"), # palisades pointe
      "road" = c("rd"),
      "route" = c("rt"),
      "street" = c("st", "str"),
      "square" = c("sq"),
      "terrace" = c("te", "ter", "terr", "trce"),
      "trail" = c("tr", "tl"),
      "way" = c("wy")
    ) |>
    purrr::imap(\(.x, .i) {
      stats::setNames(
        c(tolower(.i), as.character(.x)),
        rep(.i, times = length(.x) + 1)
      )
    }) |>
    purrr::flatten()
  out <- names(lookup[match(x, lookup)])
  # replace unmatched with original input
  out[is.na(out)] <- x[is.na(out)]
  return(out)
}
