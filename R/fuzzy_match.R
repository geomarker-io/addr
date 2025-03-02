#' fuzzy match strings in x to y using optimized string alignment (ignoring capitalization)
#' @param x character vector to match
#' @param y character vector to match to
#' @param osa_max_dist maximum osa distance to consider a match
#' @param ties if multiple strings in `y` are tied for the minimum osa distances with a string in `x`,
#' then specify "first" or "random" as a tiebreaker
#' @return an integer vector representing the position of the best matching string in `y` for each string in `x`;
#' when `ties` is "all", a list of integer vectors is returned instead
#' @details `fuzzy_match_addr_field` is a helper to match addr vectors using fuzzy_match on a specific field
fuzzy_match <- function(x, y, osa_max_dist = 1, ties = c("first", "random", "all")) {
  if (!rlang::is_character(x)) rlang::abort("x must be a character")
  if (!rlang::is_character(y)) rlang::abort("y must be a character")
  if (!rlang::is_bare_numeric(osa_max_dist)) rlang::abort("osa_max_dist must be a numeric")
  ties <- rlang::arg_match(ties)

  the_dist <- stringdist::stringdistmatrix(tolower(x), tolower(y), method = "osa")
  min_dist_matches <- apply(the_dist,
    MARGIN = 1,
    FUN = \(.) which(min(.) <= osa_max_dist & . == min(.)),
    simplify = FALSE
  )
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

#' @param x_addr addr vector to match
#' @param y_addr addr vector to match to
#' @param addr_field character name of `addr()` field to match on
#' @rdname fuzzy_match
fuzzy_match_addr_field <- function(x_addr, y_addr, addr_field, osa_max_dist = 0, ties = "all") {
  if (!inherits(x_addr, "addr")) rlang::abort("x_addr must be an addr object")
  if (!inherits(y_addr, "addr")) rlang::abort("y_addr must be an addr object")
  rlang::check_required(addr_field)
  addr_field <- rlang::arg_match(addr_field, vctrs::fields(x_addr))
  out <-
    purrr::map(list(x_addr, y_addr), \(.x) as.character(vctrs::field(.x, addr_field))) |>
    purrr::list_modify(osa_max_dist = osa_max_dist, ties = ties) |>
    do.call(fuzzy_match, args = _)
  return(out)
}
