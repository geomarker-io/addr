#' Left join two data frames using addr matching
#'
#' `addr_left_join()` is a convenience wrapper around [addr_match()] that
#' returns a left-join style result. It expands rows of `x` for duplicate rows
#' in the original `y` that share the exact matched `addr`, but it does not
#' return multiple distinct candidate addresses from `y`. `addr_match()` still
#' selects a single best address before this wrapper expands exact duplicates.
#'
#' @param x,y data frames or tibbles with an addr column
#' @param by addr column name in `x` (and `y` if the same); or a length-2
#'   character vector of `c(x_col, y_col)`
#' @param suffix character vector of length 2 used to suffix duplicate columns
#' @inheritParams addr_match
#' @param progress logical; show `addr_match()` progress?
#' @return A data frame with left-join semantics. Duplicate rows in `y` with
#'   the exact same matched `addr` are all returned. Partial ZIP-only or
#'   street-only matches do not expand to multiple candidate rows in `y`.
#' @export
addr_left_join <- function(
  x,
  y,
  by = "addr",
  suffix = c(".x", ".y"),
  zip_variants = TRUE,
  name_phonetic_dist = 2L,
  name_fuzzy_dist = 1L,
  number_fuzzy_dist = 1L,
  match_street_predirectional = TRUE,
  match_street_posttype = TRUE,
  match_street_pretype = FALSE,
  match_street_postdirectional = FALSE,
  progress = interactive()
) {
  if (is.character(by) && length(by) == 2L) {
    x_by <- by[[1]]
    y_by <- by[[2]]
  } else if (is.character(by) && length(by) == 1L) {
    x_by <- by
    y_by <- by
  } else {
    stop("by must be a character vector of length 1 or 2", call. = FALSE)
  }

  stopifnot(
    "x must be a data frame" = is.data.frame(x),
    "y must be a data frame" = is.data.frame(y),
    "x must contain the join column" = x_by %in% names(x),
    "y must contain the join column" = y_by %in% names(y),
    "suffix must be length 2" = is.character(suffix) && length(suffix) == 2,
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants),
    "name_phonetic_dist must be an integer" =
      typeof(name_phonetic_dist) == "integer" &&
        length(name_phonetic_dist) == 1L &&
        !is.na(name_phonetic_dist),
    "name_fuzzy_dist must be an integer" =
      typeof(name_fuzzy_dist) == "integer" &&
        length(name_fuzzy_dist) == 1L &&
        !is.na(name_fuzzy_dist),
    "number_fuzzy_dist must be an integer" =
      typeof(number_fuzzy_dist) == "integer" &&
        length(number_fuzzy_dist) == 1L &&
        !is.na(number_fuzzy_dist),
    "match_street_predirectional must be TRUE or FALSE" =
      is.logical(match_street_predirectional) &&
        length(match_street_predirectional) == 1L &&
        !is.na(match_street_predirectional),
    "match_street_posttype must be TRUE or FALSE" =
      is.logical(match_street_posttype) &&
        length(match_street_posttype) == 1L &&
        !is.na(match_street_posttype),
    "match_street_pretype must be TRUE or FALSE" =
      is.logical(match_street_pretype) &&
        length(match_street_pretype) == 1L &&
        !is.na(match_street_pretype),
    "match_street_postdirectional must be TRUE or FALSE" =
      is.logical(match_street_postdirectional) &&
        length(match_street_postdirectional) == 1L &&
        !is.na(match_street_postdirectional),
    "progress must be TRUE or FALSE" = is.logical(progress) &&
      length(progress) == 1L &&
      !is.na(progress)
  )

  if (nrow(x) == 0L) {
    y_out <- y[0, , drop = FALSE]
    if (y_by %in% names(y_out)) {
      names(y_out)[match(y_by, names(y_out))] <- paste0(y_by, suffix[2])
    }
    return(vctrs::vec_cbind(x, y_out))
  }

  if (nrow(y) == 0L) {
    y_out <- y[rep(NA_integer_, nrow(x)), , drop = FALSE]
    if (y_by %in% names(y_out)) {
      names(y_out)[match(y_by, names(y_out))] <- paste0(y_by, suffix[2])
    }
    return(vctrs::vec_cbind(x, y_out))
  }

  x_addr <- x[[x_by]]
  y_addr <- y[[y_by]]

  if (!inherits(x_addr, "addr")) {
    stop(
      sprintf(
        "x$%s is not an addr object; see ?as_addr for coercion",
        x_by
      ),
      call. = FALSE
    )
  }
  if (!inherits(y_addr, "addr")) {
    stop(
      sprintf(
        "y$%s is not an addr object; see ?as_addr for coercion",
        y_by
      ),
      call. = FALSE
    )
  }

  matched <- addr_match(
    x = x_addr,
    y = y_addr,
    zip_variants = zip_variants,
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    number_fuzzy_dist = number_fuzzy_dist,
    match_street_predirectional = match_street_predirectional,
    match_street_posttype = match_street_posttype,
    match_street_pretype = match_street_pretype,
    match_street_postdirectional = match_street_postdirectional,
    progress = progress
  )

  y_rows_by_key <- split(seq_len(nrow(y)), addr_match_key(y_addr), drop = TRUE)
  matched_keys <- addr_match_key(matched)
  matched_keys[is.na(matched)] <- NA_character_

  y_hits <- lapply(matched_keys, function(key) {
    if (is.na(key)) {
      return(NA_integer_)
    }
    idx <- y_rows_by_key[[key]]
    if (is.null(idx) || length(idx) == 0L) {
      return(NA_integer_)
    }
    idx
  })

  x_idx <- rep(seq_len(nrow(x)), times = lengths(y_hits))
  y_idx <- unlist(y_hits, use.names = FALSE)

  x_out <- x[x_idx, , drop = FALSE]
  y_out <- y[y_idx, , drop = FALSE]

  common <- intersect(names(x_out), names(y_out))
  common <- setdiff(common, c(x_by, y_by))
  if (length(common) > 0L) {
    names(x_out)[match(common, names(x_out))] <- paste0(common, suffix[1])
    names(y_out)[match(common, names(y_out))] <- paste0(common, suffix[2])
  }
  names(y_out)[match(y_by, names(y_out))] <- paste0(y_by, suffix[2])

  vctrs::vec_cbind(x_out, y_out)
}
