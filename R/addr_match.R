# Helper to build exact match keys for addr parts while preserving empty
# strings and distinguishing them from missing values.
addr_match_df <- function(x) {
  if (inherits(x, "addr")) {
    return(data.frame(
      number_prefix = x@number@prefix,
      number_digits = x@number@digits,
      number_suffix = x@number@suffix,
      street_predirectional = x@street@predirectional,
      street_premodifier = x@street@premodifier,
      street_pretype = x@street@pretype,
      street_name = x@street@name,
      street_posttype = x@street@posttype,
      street_postdirectional = x@street@postdirectional,
      place_name = x@place@name,
      place_state = x@place@state,
      place_zipcode = x@place@zipcode,
      stringsAsFactors = FALSE
    ))
  }
  if (inherits(x, "addr_number")) {
    return(data.frame(
      number_prefix = x@prefix,
      number_digits = x@digits,
      number_suffix = x@suffix,
      stringsAsFactors = FALSE
    ))
  }
  if (inherits(x, "addr_street")) {
    return(data.frame(
      street_predirectional = x@predirectional,
      street_premodifier = x@premodifier,
      street_pretype = x@pretype,
      street_name = x@name,
      street_posttype = x@posttype,
      street_postdirectional = x@postdirectional,
      stringsAsFactors = FALSE
    ))
  }
  if (inherits(x, "addr_place")) {
    return(data.frame(
      place_name = x@name,
      place_state = x@state,
      place_zipcode = x@zipcode,
      stringsAsFactors = FALSE
    ))
  }
  stop("x must be an addr or addr_part vector", call. = FALSE)
}

addr_match_key <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }
  x_df <- addr_match_df(x)
  if (nrow(x_df) == 0L) {
    return(character())
  }
  parts <- lapply(x_df, function(col) ifelse(is.na(col), "<NA>", col))
  do.call(paste, c(parts, sep = "\r"))
}

addr_empty_df <- function(n) {
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
  )
}

addr_missing <- function(n) {
  if (n == 0L) {
    return(addr::addr())
  }
  vec_restore(
    addr_empty_df(n),
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
  utils::flush.console()
}

is_addr_match_index <- function(x) {
  inherits(x, "addr_match_index")
}

match_zipcodes_prepared <- function(x, y_zipcodes, zip_variants = TRUE) {
  x <- addr::addr_place(zipcode = x)@zipcode
  ux <- unique(x[!is.na(x) & x != ""])
  uy <- unique(y_zipcodes[!is.na(y_zipcodes) & y_zipcodes != ""])
  lkp <- vapply(
    ux,
    FUN = function(xz) {
      if (xz %in% uy) {
        return(xz)
      }
      if (zip_variants) {
        xzv <- zipcode_variant(xz)
        m <- xzv[xzv %in% uy]
        if (length(m) == 0L) {
          return(NA_character_)
        }
        m <- m[order(
          abs(as.integer(xz) - as.integer(m)),
          stringdist::stringdist(xz, m, method = "osa"),
          as.integer(m)
        )]
        return(m[1])
      }
      NA_character_
    },
    FUN.VALUE = character(1),
    USE.NAMES = TRUE
  )
  out <- lkp[x]
  out[is.na(x) | x == ""] <- NA_character_
  names(out) <- NULL
  out
}

addr_match_prepare_zip <- function(y) {
  street_keys <- addr_match_key(y@street)
  y_by_street <- split(seq_along(y), street_keys, drop = TRUE)
  y_by_number <- lapply(
    y_by_street,
    function(idx) {
      split(idx, addr_match_key(y[idx]@number), drop = TRUE)
    }
  )
  list(
    y = y,
    y_by_street = y_by_street,
    y_by_number = y_by_number
  )
}

addr_match_zip_chunk <- function(x, zip_data, osa_max_dist = 1L) {
  out_df <- addr_empty_df(length(x))
  if (length(x) == 0L || is.null(zip_data) || length(zip_data$y) == 0L) {
    return(out_df)
  }

  street_matches <- match_addr_street(x@street, zip_data$y@street)
  matched_street_rows <- !is.na(street_matches)
  if (any(matched_street_rows)) {
    out_df[
      matched_street_rows,
      names(addr_match_df(street_matches[matched_street_rows]))
    ] <- addr_match_df(street_matches[matched_street_rows])
  }
  street_keys <- addr_match_key(street_matches)
  street_keys[is.na(street_matches)] <- NA_character_

  for (street_key in unique(stats::na.omit(street_keys))) {
    street_x_idx <- which(street_keys == street_key)
    street_y_idx <- zip_data$y_by_street[[street_key]]
    if (is.null(street_y_idx) || length(street_y_idx) == 0L) {
      next
    }

    number_matches <- match_addr_number(
      x[street_x_idx]@number,
      zip_data$y[street_y_idx]@number,
      osa_max_dist = osa_max_dist
    )
    number_keys <- addr_match_key(number_matches)
    number_keys[is.na(number_matches)] <- NA_character_
    matched_numbers <- !is.na(number_matches)
    if (!any(matched_numbers)) {
      next
    }

    y_by_number <- zip_data$y_by_number[[street_key]]
    matched_y_idx <- vapply(
      number_keys[matched_numbers],
      function(key) y_by_number[[key]][1],
      integer(1)
    )
    out_df[
      street_x_idx[matched_numbers],
      names(addr_match_df(zip_data$y[matched_y_idx]))
    ] <- addr_match_df(zip_data$y[matched_y_idx])
  }

  out_df
}

addr_match_update_output <- function(out_df, x_idx, zip_out_df) {
  matched_rows <- rowSums(!is.na(zip_out_df)) > 0L
  if (!any(matched_rows)) {
    return(out_df)
  }
  current <- out_df[x_idx[matched_rows], names(zip_out_df), drop = FALSE]
  updates <- zip_out_df[matched_rows, , drop = FALSE]
  replace_idx <- !is.na(updates)
  current[replace_idx] <- updates[replace_idx]
  out_df[x_idx[matched_rows], names(zip_out_df)] <- current
  out_df
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
#' `addr_match()` accepts raw reference data and prepares it internally, which
#' is the right default for one-off matching jobs. `addr_match_prepare()`
#' becomes useful when the same reference `y` will be reused across multiple
#' calls to `addr_match()`, because it caches the deduplicated reference
#' addresses and ZIP/street/number candidate lookups once instead of rebuilding
#' them on every call.
#'
#' @param x addr vector to match
#' @param y addr vector to match against, or a prepared `addr_match_index`
#'   created by `addr_match_prepare()`
#' @param zip_variants logical; fuzzy match to common ZIP code variants in
#'   `match_zipcodes()`?
#' @param osa_max_dist integer maximum OSA distance used by
#'   `match_addr_number()`
#' @param progress logical; show reference-preparation timing for raw `y` and a
#'   progress bar while processing matched ZIP groups?
#' @return an addr vector, the same length as x, that is the best match in y
#'   for each addr in x. Partial matches are returned with matched ZIP code
#'   and/or street fields filled when later stages do not match.
#' @export
#' @examples
#' my_addr <- as_addr(voter_addresses()[1:10])
#' the_addr <- nad_example_data(match_prepare = TRUE)
#'
#' addr_match(my_addr, the_addr)
#'
addr_match <- function(
  x,
  y,
  zip_variants = TRUE,
  osa_max_dist = 1L,
  progress = interactive()
) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
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

  if (is_addr_match_index(y)) {
    y_index <- y
  } else {
    if (!inherits(y, "addr")) {
      stop("y must be an addr vector or addr_match_index", call. = FALSE)
    }
    prep_start_time <- proc.time()[["elapsed"]]
    if (progress) {
      cat("preparing reference addr vector\n")
    }
    y_index <- addr_match_prepare(y)
    if (progress) {
      prep_elapsed <- proc.time()[["elapsed"]] - prep_start_time
      cat(
        "prepared reference addr vector in ",
        sprintf("%.2f", prep_elapsed),
        " seconds\n",
        sep = ""
      )
    }
  }

  if (y_index$n_unique == 0L) {
    return(addr_missing(length(x)))
  }

  matched_zipcodes <- match_zipcodes_prepared(
    x@place@zipcode,
    y_index$zipcodes,
    zip_variants = zip_variants
  )
  x_by_zip <- split(seq_along(x), matched_zipcodes, drop = TRUE)
  out_df <- addr_empty_df(length(x))
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
    zip_data <- y_index$by_zip[[zip]]
    y_n <- if (is.null(zip_data)) 0L else length(zip_data$y)
    if (progress) {
      addr_progress_update(
        processed,
        length(x),
        addr_progress_text(zip, length(x_idx), y_n),
        first = FALSE
      )
    }
    if (is.null(zip_data) || y_n == 0L) {
      processed <- processed + length(x_idx)
      if (progress) {
        addr_progress_update(
          processed,
          length(x),
          addr_progress_text(zip, length(x_idx), y_n),
          first = FALSE
        )
      }
      next
    }

    out_df <- addr_match_update_output(
      out_df = out_df,
      x_idx = x_idx,
      zip_out_df = addr_match_zip_chunk(
        x = x[x_idx],
        zip_data = zip_data,
        osa_max_dist = osa_max_dist
      )
    )

    processed <- processed + length(x_idx)
    if (progress) {
      addr_progress_update(
        processed,
        length(x),
        addr_progress_text(zip, length(x_idx), y_n),
        first = FALSE
      )
    }
  }

  vec_restore(out_df, to = addr::addr())
}

#' Prepare addr reference data for repeated matching
#'
#' Preparing `y` once avoids recomputing `unique(y)`, ZIP-code groups, and
#' exact street/number candidate lookups each time you call `addr_match()`
#' with the same reference addresses. For a single end-to-end match, preparing
#' `y` explicitly does not remove that work; it only moves it outside
#' `addr_match()`.
#'
#' @rdname addr_match
#' @export
addr_match_prepare <- function(y) {
  stopifnot("y must be an addr vector" = inherits(y, "addr"))

  if (length(y) == 0L) {
    return(structure(
      list(
        by_zip = list(),
        zipcodes = character(),
        n_unique = 0L
      ),
      class = "addr_match_index"
    ))
  }

  uy <- unique(y)
  y_by_zip_idx <- split(seq_along(uy), uy@place@zipcode, drop = TRUE)
  by_zip <- lapply(y_by_zip_idx, function(idx) addr_match_prepare_zip(uy[idx]))
  zipcodes <- unique(uy@place@zipcode[
    !is.na(uy@place@zipcode) & uy@place@zipcode != ""
  ])

  structure(
    list(
      by_zip = by_zip,
      zipcodes = zipcodes,
      n_unique = length(uy)
    ),
    class = "addr_match_index"
  )
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
