#' strings in x and y are matched using optimized string alignment with provided threshold (ignoring capitalization)
#' @param ties if multiple strings in `y` are tied for the minimum osa distances with a string in `x`,
#' then specify "first" or "random" as a tiebreaker
#' @return an integer vector representing the position of the best matching string in `y` for each string in `x`;
#' when `ties` is "all", a list of integer vectors is returned instead
fuzzy_match <- function(x, y, osa_max_dist = 1, ties = c("first", "random", "all")) {
  ties <- rlang::arg_match(ties)
  the_dist <- stringdist::stringdistmatrix(tolower(x), tolower(y), method = "osa")
  min_dist_matches <- apply(the_dist,
                            MARGIN = 1,
                            FUN = \(.) which(min(.) <= osa_max_dist & . == min(.)),
                            simplify = FALSE)
  out <- purrr::modify_if(min_dist_matches, \(.) length(.) == 0, \(.) NA)
  if (ties == "random") {
    out <- purrr::modify_if(out, \(.) length(.) > 1, sample, size = 1)
  }
  if (ties == "first") {
    out <- purrr::modify_if(out, \(.) length(.) > 1, \(tmp) tmp[1])
  }
  if (ties %in% c("random", "first")) out <- purrr::list_c(out, ptype = integer(1))
  return(out)
}
