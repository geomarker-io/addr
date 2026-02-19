#' Left join two data frames using fuzzy addr matching
#'
#' This is a convenience wrapper around the addr fuzzy matching helpers that
#' returns a left-join style result. The addr columns are matched by index and
#' rows are expanded for one-to-many or many-to-many matches.
#'
#' @param x,y data frames or tibbles with an addr column
#' @param by addr column name in `x` (and `y` if the same);
#'   or a length-2 character vector of `c(x_col, y_col)`
#' @param addr_fields a named vector of osa_max_distances; if max distances
#' for each addr tag field is not present a default will be used (see details).
#' @details
#' addr_left_join works by matching addresses grouped by ZIP codes, so specified
#' osa_max_distances for any place fields are ignored
#' Defaults for `addr_fields`:
#' \itemize{
#'   \item number_prefix: 0
#'   \item number_digits: 0
#'   \item number_suffix: 0
#'   \item street_predirectional: 0
#'   \item street_premodifier: 0
#'   \item street_pretype: 0
#'   \item street_name: 1
#'   \item street_posttype: 0
#'   \item street_postdirectional: 0
#' }
#' @param suffix character vector of length 2 used to suffix duplicate columns
#' @return a data frame with left-join semantics
#' @export
#' @examples
#' my_addr <-
#'   tibble::tibble(address = voter_addresses()[1:1000],
#'                  addr = as_addr(address),
#'                  id = sprintf("id_%04d", seq_len(1000)))
#' the_addr <- nad_example_data()
#' addr_left_join(my_addr, the_addr, c("addr", "nad_addr"))
addr_left_join <- function(
  x,
  y,
  by = "addr",
  addr_fields = NULL,
  suffix = c(".x", ".y")
) {
  if (is.character(by) && length(by) == 2) {
    x_by <- by[[1]]
    y_by <- by[[2]]
  } else if (is.character(by) && length(by) == 1) {
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
    "suffix must be length 2" = is.character(suffix) && length(suffix) == 2
  )

  if (nrow(x) == 0L) {
    y_out <- y[0, , drop = FALSE]
    if (y_by %in% names(y_out)) {
      names(y_out)[match(y_by, names(y_out))] <- paste0(y_by, suffix[2])
    }
    return(vctrs::vec_cbind(x, y_out))
  }

  if (nrow(y) == 0L) {
    y_out <- y[0, , drop = FALSE]
    y_out <- y_out[rep(NA_integer_, nrow(x)), , drop = FALSE]
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

  x_zips <- unique(x_addr@place@zipcode)
  y_zips <- unique(y_addr@place@zipcode)
  x_idx_no_zip <- which(x_addr@place@zipcode %in% x_zips[!x_zips %in% y_zips])

  message(
    sum(x_zips %in% y_zips),
    " of ",
    length(x_zips),
    " unique ZIP codes in x matched to one of ",
    length(y_zips),
    " unique ZIP codes in y"
  )
  if (length(x_idx_no_zip) > 0) {
    message(
      length(x_idx_no_zip),
      " addr in x will not have ZIP code-based matching candidates"
    )
  }

  af <- addr_fuzzy_match_resolve_max_dist(addr_fields)
  af["place_name"] <- Inf
  af["place_state"] <- Inf
  af["place_zipcode"] <- Inf

  by_zip <- lapply(intersect(x_zips, y_zips), \(.) {
    list(
      x = which(x_addr@place@zipcode == .),
      y = which(y_addr@place@zipcode == .)
    )
  })

  # TODO better progress messages that list zipcode and number of addr in each chunk
  # compat purrr with progress messages or do for loop?
  by_zip_matches <- purrr::map(
    by_zip,
    \(.) addr_fuzzy_match(x = x_addr[.$x], y = y_addr[.$y], addr_fields = af),
    .progress = "matching by zipcode"
  )

  normalize_idx <- function(idx) {
    if (is.null(idx) || length(idx) == 0) {
      return(NA_integer_)
    }
    if (all(is.na(idx))) {
      return(NA_integer_)
    }
    as.integer(idx)
  }

  by_zip_matches <- lapply(by_zip_matches, \(.) lapply(., normalize_idx))

  y_idx <-
    mapply(bzm = by_zip_matches, bz = by_zip, \(bzm, bz) {
      lapply(bzm, \(.) bz$y[.])
    }) |>
    do.call(c, args = _)

  wmm <- which(lengths(y_idx) > 1)

  if (length(wmm) > 0) {
    warning(
      "Multi-matches detected for ",
      length(wmm),
      " addr in x;\n",
      "More than one row of y will be returned once for each matching row in x",
      call. = FALSE
    )
  }

  x_idx <- unlist(lapply(by_zip, \(.) .$x)) |>
    rep(times = lengths(y_idx))

  # add non-zip matched back in
  x_idx <- c(x_idx, x_idx_no_zip)
  y_idx <- c(y_idx, list(rep(NA_integer_, length(x_idx_no_zip))))

  x_out <- x[x_idx, , drop = FALSE]
  y_out <- y[unlist(y_idx), , drop = FALSE]

  common <- intersect(names(x_out), names(y_out))
  common <- setdiff(common, c(x_by, y_by))
  if (length(common) > 0) {
    names(x_out)[match(common, names(x_out))] <- paste0(common, suffix[1])
    names(y_out)[match(common, names(y_out))] <- paste0(common, suffix[2])
  }
  names(y_out)[match(y_by, names(y_out))] <- paste0(y_by, suffix[2])

  # TODO reorder to match the input rows to x

  vctrs::vec_cbind(x_out, y_out)
}
