#' Tiger Block Groups
#'
#' Get the identifier of the closest census block group based on the intersection of the s2 cell locations with the
#' the US Census [TIGER/Line shapefiles](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
#' @param x s2_cell vector
#' @param year vintage of TIGER/Line block group geography files
#' @details TIGER geography files are saved to the R user cache directory for the addr package. This allows
#' R sessions to reuse previously downloaded files. See `?tools::R_user_dir()` to change where TIGER geography
#' files are saved.
#' @returns character vector of matched census block group identifiers
#' @export
#' @examples
#' s2_join_tiger_bg(x = s2::as_s2_cell(c("8841b39a7c46e25f", "8841a45555555555")))
s2_join_tiger_bg <- function(x, year = as.character(2024:2013)) {
  rlang::check_installed("sf", "read TIGER/Line census block group geographies")
  rlang::check_installed("s2", "s2 geometry calculations")
  if (!inherits(x, "s2_cell")) stop("x must be a s2_cell vector", call. = FALSE)
  year <- rlang::arg_match(year)
  x_s2_geo <-
    unique(stats::na.omit(x)) |>
    s2::s2_cell_to_lnglat() |>
    s2::as_s2_geography()
  names(x_s2_geo) <- as.character(unique(stats::na.omit(x)))
  states <- tiger_states(year)
  the_states <- states[s2::s2_closest_feature(x_s2_geo, states$s2_geography), "GEOID", drop = TRUE]
  state_bgs <-
    lapply(unique(the_states), tiger_block_groups, year = year) |>
    stats::setNames(unique(the_states))
  the_s2s <- split(x_s2_geo, the_states)
  out <-
    purrr::imap(the_s2s, \(x, i) {
      state_bgs[[i]][s2::s2_closest_feature(x, state_bgs[[i]]$s2_geography), "GEOID", drop = TRUE]
    }) |>
    unlist() |>
    stats::setNames(unlist(purrr::map(the_s2s, names)))
  return(stats::setNames(out[as.character(x)], NULL))
}


