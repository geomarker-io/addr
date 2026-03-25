# Helper to build exact match keys for addr parts while preserving empty
# strings and distinguishing them from missing values.
addr_match_key <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }
  x_df <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(x_df) == 0L) {
    return(character())
  }
  parts <- lapply(x_df, function(col) ifelse(is.na(col), "<NA>", col))
  do.call(paste, c(parts, sep = "\r"))
}

addr_missing <- function(n) {
  if (n == 0L) {
    return(addr::addr())
  }
  vec_restore(
    data.frame(
      number_prefix = rep(NA_character_, n),
      number_digits = rep(NA_character_, n),
      number_suffix = rep(NA_character_, n),
      street_predirectional = rep(NA_character_, n),
      street_premodifier = rep(NA_character_, n),
      street_pretype = rep(NA_character_, n),
      street_name = rep(NA_character_, n),
      street_posttype = rep(NA_character_, n),
      street_postdirectional = rep(NA_character_, n),
      place_name = rep(NA_character_, n),
      place_state = rep(NA_character_, n),
      place_zipcode = rep(NA_character_, n),
      stringsAsFactors = FALSE
    ),
    to = addr::addr()
  )
}

addr_progress_text <- function(zip, x_n, y_n) {
  sprintf(
    "matching addr vectors in %s (%s to %s)",
    zip,
    prettyNum(x_n, big.mark = ",", preserve.width = "none"),
    prettyNum(y_n, big.mark = ",", preserve.width = "none")
  )
}

addr_progress_bar <- function(current, total, width = 80L) {
  width <- as.integer(width[[1]])
  total <- max(as.integer(total[[1]]), 1L)
  current <- min(max(as.integer(current[[1]]), 0L), total)
  width <- max(width, 2L)
  inner_width <- width - 2L
  filled <- floor((current / total) * inner_width)
  sprintf(
    "[%s%s]",
    strrep("=", filled),
    strrep(".", inner_width - filled)
  )
}

addr_progress_update <- function(
  current,
  total,
  text,
  width = 80L,
  first = FALSE
) {
  text <- gsub("[\r\n]+", " ", text)
  if (!first) {
    cat("\033[1A\r\033[2K", sep = "")
  }
  cat(
    "\r\033[2K",
    text,
    "\n\033[2K",
    addr_progress_bar(current, total, width = width),
    sep = ""
  )
  flush.console()
}

#' Match addr vectors
#'
#' @description
#' A single addr in `y` is chosen for each addr in `x`. Matching is staged to
#' reduce the search space: ZIP codes are matched first, street names are then
#' matched within each matched ZIP code, and street numbers are finally matched
#' within each matched street and ZIP code combination. If more than one
#' candidate addr remains in `y` after these stages, the first candidate in `y`
#' is returned.
#'
#' Missing or empty address components that cannot be matched at any stage are
#' left missing in the returned `addr()` values. Rows with a matched ZIP code
#' but no street match return an addr with only `@place@zipcode` filled; rows
#' with matched ZIP code and street but no number match also return the matched
#' `@street`.
#'
#' @param x,y addr vectors to match
#' @param zip_variants logical; fuzzy match to common ZIP code variants in
#'   `match_zipcodes()`?
#' @param osa_max_dist integer maximum OSA distance used by
#'   `match_addr_number()`
#' @param progress logical; show a progress bar while processing matched ZIP
#'   groups?
#' @return an addr vector, the same length as x, that is the best match in y
#'   for each addr in x. Partial matches are returned with matched ZIP code
#'   and/or street fields filled when later stages do not match.
#' @export
#' @examples
#' my_addr <- as_addr(voter_addresses()[1:10])
#' the_addr <- nad_example_data()$nad_addr
#'
#' addr_match(my_addr, the_addr)
addr_match <- function(
  x,
  y,
  zip_variants = TRUE,
  osa_max_dist = 1L,
  progress = interactive()
) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "y must be an addr vector" = inherits(y, "addr"),
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants),
    "osa_max_dist must be an integer" = typeof(osa_max_dist) == "integer" &&
      length(osa_max_dist) == 1L &&
      !is.na(osa_max_dist),
    "progress must be TRUE or FALSE" = is.logical(progress) &&
      length(progress) == 1L &&
      !is.na(progress)
  )

  start_time <- proc.time()[["elapsed"]]
  if (length(x) == 0L) {
    return(x)
  }
  if (length(y) == 0L) {
    return(addr_missing(length(x)))
  }

  uy <- unique(y)
  matched_zipcodes <- match_zipcodes(
    x@place@zipcode,
    uy@place@zipcode,
    zip_variants = zip_variants
  )
  x_by_zip <- split(seq_along(x), matched_zipcodes, drop = TRUE)
  y_by_zip <- split(seq_along(uy), uy@place@zipcode, drop = TRUE)
  out_df <- as.data.frame(addr_missing(length(x)))
  out_df$place_zipcode <- matched_zipcodes

  processed <- sum(is.na(matched_zipcodes))
  if (progress) {
    on.exit(
      {
        elapsed <- proc.time()[["elapsed"]] - start_time
        addr_progress_update(
          length(x),
          length(x),
          "matching addr vectors complete",
          first = FALSE
        )
        cat(
          "\nmatched addr vectors in ",
          sprintf("%.2f", elapsed),
          " seconds\n",
          sep = ""
        )
      },
      add = TRUE
    )
    addr_progress_update(
      processed,
      length(x),
      "matching addr vectors",
      first = TRUE
    )
  }

  for (zip in names(x_by_zip)) {
    x_idx <- x_by_zip[[zip]]
    y_idx <- y_by_zip[[zip]]
    if (progress) {
      addr_progress_update(
        processed,
        length(x),
        addr_progress_text(zip, length(x_idx), length(y_idx)),
        first = FALSE
      )
    }
    if (is.null(y_idx) || length(y_idx) == 0L) {
      processed <- processed + length(x_idx)
      if (progress) {
        addr_progress_update(
          processed,
          length(x),
          addr_progress_text(zip, length(x_idx), length(y_idx)),
          first = FALSE
        )
      }
      next
    }

    street_matches <- match_addr_street(x[x_idx]@street, uy[y_idx]@street)
    matched_street_rows <- !is.na(street_matches)
    if (any(matched_street_rows)) {
      out_df[
        x_idx[matched_street_rows],
        names(as.data.frame(street_matches[matched_street_rows]))
      ] <- as.data.frame(street_matches[matched_street_rows])
    }
    street_keys <- addr_match_key(street_matches)
    street_keys[is.na(street_matches)] <- NA_character_
    y_street_keys <- addr_match_key(uy[y_idx]@street)
    y_by_street <- split(y_idx, y_street_keys, drop = TRUE)

    for (street_key in unique(stats::na.omit(street_keys))) {
      street_x_idx <- x_idx[street_keys == street_key]
      street_y_idx <- y_by_street[[street_key]]
      if (is.null(street_y_idx) || length(street_y_idx) == 0L) {
        next
      }

      number_matches <- match_addr_number(
        x[street_x_idx]@number,
        uy[street_y_idx]@number,
        osa_max_dist = osa_max_dist
      )
      number_keys <- addr_match_key(number_matches)
      number_keys[is.na(number_matches)] <- NA_character_
      y_number_keys <- addr_match_key(uy[street_y_idx]@number)
      y_by_number <- split(street_y_idx, y_number_keys, drop = TRUE)

      matched_numbers <- !is.na(number_matches)
      if (!any(matched_numbers)) {
        next
      }

      matched_y_idx <- vapply(
        number_keys[matched_numbers],
        function(key) y_by_number[[key]][1],
        integer(1)
      )
      out_df[
        street_x_idx[matched_numbers],
        names(as.data.frame(uy[matched_y_idx]))
      ] <- as.data.frame(uy[matched_y_idx])
    }

    processed <- processed + length(x_idx)
    if (progress) {
      addr_progress_update(
        processed,
        length(x),
        addr_progress_text(zip, length(x_idx), length(y_idx)),
        first = FALSE
      )
    }
  }

  vec_restore(out_df, to = addr::addr())
}

#' Classify addr match stage
#'
#' Classify an addr vector into the staged outcomes returned by
#' `addr_match()`: no match, ZIP-only match, ZIP-plus-street match, or
#' ZIP-plus-street-plus-number match.
#'
#' @param x addr vector to classify
#' @param strict logical; require `x` to follow the partial-result structure
#'   produced by `addr_match()`? If `FALSE`, classification is based only on the
#'   deepest non-missing core component (`@place@zipcode`, `@street@name`,
#'   `@number@digits`).
#' @return an ordered factor with levels `none`, `zip`, `street`, `number`
#' @export
#' @examples
#' y <- as_addr(c(
#'   "10 MAIN ST CINCINNATI OH 45220",
#'   "11 MAIN ST CINCINNATI OH 45220",
#'   "10 MAIN ST CINCINNATI OH 45229"
#' ))
#' x <- as_addr(c(
#'   "99 MAIN ST CINCINNATI OH 45220",
#'   "10 OAK ST CINCINNATI OH 45220",
#'   "10 MAIN ST CINCINNATI OH 45103"
#' ))
#'
#' out <- addr_match(x, y)
#' addr_match_stage(out)
addr_match_stage <- function(x, strict = TRUE) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "strict must be TRUE or FALSE" = is.logical(strict) &&
      length(strict) == 1L &&
      !is.na(strict)
  )

  x_df <- as.data.frame(x)
  any_nonmissing <- function(cols) {
    if (length(cols) == 0L) {
      return(rep(FALSE, nrow(x_df)))
    }
    rowSums(!is.na(x_df[, cols, drop = FALSE])) > 0L
  }

  has_zip <- !is.na(x_df$place_zipcode)
  has_street <- !is.na(x_df$street_name)
  has_number <- !is.na(x_df$number_digits)

  number_any <- any_nonmissing(c(
    "number_prefix",
    "number_digits",
    "number_suffix"
  ))
  street_any <- any_nonmissing(c(
    "street_predirectional",
    "street_premodifier",
    "street_pretype",
    "street_name",
    "street_posttype",
    "street_postdirectional"
  ))
  place_other_any <- any_nonmissing(c("place_name", "place_state"))
  all_missing <- rowSums(!is.na(x_df)) == 0L

  if (strict) {
    valid_none <- all_missing
    valid_zip <- has_zip & !street_any & !number_any & !place_other_any
    valid_street <- has_zip & has_street & !number_any & !place_other_any
    valid_number <- has_zip & has_street & has_number
    valid <- valid_none | valid_zip | valid_street | valid_number
    if (any(!valid)) {
      stop(
        paste(
          "x does not look like an addr_match result;",
          "use strict = FALSE to classify by deepest non-missing component"
        ),
        call. = FALSE
      )
    }
  }

  out <- rep("none", length(x))
  out[has_zip] <- "zip"
  out[has_street] <- "street"
  out[has_number] <- "number"
  ordered(out, levels = c("none", "zip", "street", "number"))
}
