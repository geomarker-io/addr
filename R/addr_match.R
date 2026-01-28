#' matching addr objects
#'
#' Optimized String Alignment (OSA) distances are used to
#' choose a set of matching reference addr with flexible, field-specific thresholds.
#' See `fuzzy_match()`/`fuzzy_match_addr_field()` for more details.
#' addr vectors are matched in groups by five digit ZIP codes.
#' @param x an addr vector to match
#' @param ref_addr an addr vector to search for matches in
#' @param simplify logical; choose the first addr from multi-matches and return an
#' integer vector instead of a list of integer vectors?
#' @returns a named list of possible addr matches for each addr in `x`;
#' a list value of NULL means the zip code was not matched and
#' a list value of a zero-length addr vector means the zip code was matched,
#' but the street number, name, and type were not matched
#' @examples
#' addr(c("3333 Burnet Ave Cincinnati OH 45229", "5130 RAPID RUN RD CINCINNATI OHIO 45238")) |>
#'   addr_match(nad_addr()$nad_addr)
#' @export
addr_match <- function(
  x,
  ref_addr,
  max_dist_street_number = 0,
  max_dist_street_name = 1,
  max_dist_street_type = 0,
  simplify = FALSE
) {
  ia <- stats::na.omit(unique(as_addr(x)))
  ra <- unique(as_addr(ref_addr))

  ia_zip_list <- split(ia, vctrs::field(ia, "zip_code"))

  ra_zip_list <- split(ra, vctrs::field(ra, "zip_code"))
  ra_zip_list <-
    names(ia_zip_list) |>
    purrr::map(\(.) purrr::pluck(ra_zip_list, ., .default = NA)) |>
    purrr::set_names(names(ia_zip_list))

  zip_list <-
    purrr::transpose(list(ia = ia_zip_list, ra = ra_zip_list)) |>
    purrr::discard(\(.) any(is.na(.$ia), is.na(.$ra)))

  matches <-
    purrr::map(
      zip_list,
      \(.x) {
        addr_match_line_one(
          .x$ia,
          .x$ra,
          max_dist_street_number = max_dist_street_number,
          max_dist_street_name = max_dist_street_name,
          max_dist_street_type = max_dist_street_type,
          simplify = FALSE
        )
      },
      .progress = list(
        clear = FALSE,
        format = "matching addresses in {cli::pb_current}/{cli::pb_total} ZIP codes [{cli::pb_elapsed} elapsed] "
      )
    ) |>
    purrr::list_flatten(name_spec = "{inner}")

  out <- matches[as.character(x)]
  names(out) <- as.character(x)

  if (simplify) {
    n_multimatches <- sum(sapply(out, length) > 1)
    if (n_multimatches > 0) {
      warning(n_multimatches, " multimatches being reduced to a single match")
    }
    out <-
      out |>
      purrr::modify_if(\(.) length(.) > 1, sample, size = 1) |>
      purrr::modify_if(\(.) length(.) == 0, \(.) NA) |>
      purrr::list_c(ptype = addr())
  }

  return(out)
}

#' match addr vectors based on street number, name, and type
#'
#' @param max_dist_street_number maximum OSA distance to consider a match for the addr street_number;
#' set to NULL to disregard street number
#' @param max_dist_street_name maximum OSA distance to consider a match for the addr street_name
#' @param max_dist_street_type maximum OSA distance to consider a match for the addr street_type
#' @rdname addr_match
#' @export
#' @examples
#' addr_match_line_one(as_addr(c("3333 Burnet Ave", "3333 Foofy Ave")),
#'                     as_addr(c("Main Street", "Burnet Avenue", "Burnet Way")),
#'                     max_dist_street_number = NULL,
#'                     simplify = TRUE)
addr_match_line_one <- function(
  x,
  ref_addr,
  max_dist_street_number = 0,
  max_dist_street_name = 1,
  max_dist_street_type = 0,
  simplify = FALSE
) {
  matches <- list()
  if (!is.null(max_dist_street_number)) {
    matches$street_number <- fuzzy_match_addr_field(
      x,
      ref_addr,
      "number_digits",
      max_dist_street_number
    )
  }
  matches$street_name <- fuzzy_match_addr_field(
    x,
    ref_addr,
    "street_name",
    max_dist_street_name
  )
  matches$street_type <- fuzzy_match_addr_field(
    x,
    ref_addr,
    "street_posttype",
    max_dist_street_type
  )

  out <- do.call(
    Map,
    c(list(function(...) Reduce(intersect, list(...))), matches)
  )

  if (simplify) {
    n_multimatches <- sum(sapply(out, length) > 1)
    if (n_multimatches > 0) {
      warning(n_multimatches, " multimatches being reduced to first match")
    }
    out <- lapply(out, `[`, 1)
  }
  return(out)
}

utils::globalVariables(c("ia_zips", "ra_zips"))
