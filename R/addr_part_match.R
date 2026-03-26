#' Match addr_street vectors
#'
#' @description
#' A single addr_street in y is chosen for each addr_street in x.
#' If exact matches (using `as.character`) are not found,
#' possible matches are chosen by
#' fuzzy matching on street name (using phonetic street key and street name)
#' and exact matching on street type and predirectional.
#' Ordinal street names use restricted phonetic candidates:
#' an ordinal phonetic key like `#0007` may fuzzy match only to plausible
#' ordinal neighbors such as digit shifts (`#0070`, `#0700`, `#7000`)
#' or same-width substitutions (`#0008`, `#0009`), not arbitrary
#' OSA-distance-one ordinal keys such as `#0017` or `#0077`.
#' Ties are broken by .......... the first for now.
#'
#' addr_street objects within missing or empty @name are not matched and
#' returned as missing instead.
#' @param x,y addr_street vectors to match
#' @param name_phonetic_dist integer; maximum optimized string alignment
#' distance between `phonetic_street_key()` of x and y to consider a possible
#' match
#' @param name_fuzzy_dist integer; maximum optimized string alignment distance
#' between `@name` of x and y to consider a possible match
#' @return an addr_street vector, the same length as x, that is the
#' best match in y for each addr_street code in x; if no best match
#' is found a missing value is returned (`addr_street()`)
#' @export
#' @examples
#' my_streets <- addr_street(
#'    predirectional = "",
#'    premodifier = "",
#'    pretype = "",
#'    name = c("Beechview", "Vivian", "Springfield", "Round Bottom", "Pfeiffer", "Beachview",
#'             "Vevan", "Srpingfield", "Square Top", "Pfeffer", "Wuhlper", ""),
#'   posttype = c("Cir", "Pl", "Pike", "Rd", "Rd", "Cir", "Pl", "Pike", "Rd", "Rd", "Ave", ""),
#'   postdirectional = ""
#'  )
#' the_streets <- nad_example_data()$nad_addr@street
#' match_addr_street(my_streets, the_streets)
match_addr_street <- function(
  x,
  y,
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L
) {
  stopifnot(
    "x must be an addr_street object" = inherits(x, "addr_street"),
    "y must be an addr_street object" = inherits(y, "addr_street")
  )
  street_key <- function(df) {
    parts <- lapply(df, \(col) ifelse(is.na(col) | col == "", "", col))
    trimws(gsub(" +", " ", do.call(paste, c(parts, sep = " "))))
  }

  x_df <- as.data.frame(x)
  y_df <- as.data.frame(y)
  x_key <- tolower(street_key(x_df))
  y_key <- tolower(street_key(y_df))

  ux_idx <- !duplicated(x_key)
  ux_df <- x_df[ux_idx, , drop = FALSE]
  ux_key <- x_key[ux_idx]
  keep_ux <- !is.na(ux_df$street_name) & ux_df$street_name != ""
  ux_df <- ux_df[keep_ux, , drop = FALSE]
  ux_key <- ux_key[keep_ux]

  uy_idx <- !duplicated(y_key)
  uy_df <- y_df[uy_idx, , drop = FALSE]
  uy_key <- y_key[uy_idx]
  uy_name_psk <- phonetic_street_key(uy_df$street_name)

  lkp <- match(
    ux_key,
    uy_key,
    incomparables = c("", NA)
  )

  uy_bucket_key <- ifelse(
    is.na(uy_df$street_predirectional) | is.na(uy_df$street_posttype),
    NA_character_,
    paste(uy_df$street_predirectional, uy_df$street_posttype, sep = "\r")
  )
  uy_bucket_idx <- split(
    seq_len(nrow(uy_df)),
    uy_bucket_key,
    drop = TRUE
  )

  if (any(is.na(lkp))) {
    nomatch_idx <- which(is.na(lkp))
    nomatch_df <- ux_df[nomatch_idx, , drop = FALSE]
    nomatch_name_psk <- phonetic_street_key(nomatch_df$street_name)
    nomatch_is_ordinal <- is_ordinal_street_number(nomatch_df$street_name)
    nomatch_bucket_key <- ifelse(
      is.na(nomatch_df$street_predirectional) |
        is.na(nomatch_df$street_posttype),
      NA_character_,
      paste(
        nomatch_df$street_predirectional,
        nomatch_df$street_posttype,
        sep = "\r"
      )
    )
    m <- replicate(nrow(nomatch_df), integer(0), simplify = FALSE)

    for (bucket_key in unique(stats::na.omit(nomatch_bucket_key))) {
      bucket_idx <- uy_bucket_idx[[bucket_key]]
      if (is.null(bucket_idx) || length(bucket_idx) == 0) {
        next
      }
      bucket_nomatch_idx <- which(nomatch_bucket_key == bucket_key)
      bucket_uy_names <- uy_df$street_name[bucket_idx]
      bucket_uy_psk <- uy_name_psk[bucket_idx]

      bucket_nomatch_names <- nomatch_df$street_name[bucket_nomatch_idx]
      bucket_nomatch_name_psk <- nomatch_name_psk[bucket_nomatch_idx]
      bucket_nomatch_psk <-
        bucket_uy_psk[
          match(
            bucket_nomatch_name_psk,
            bucket_uy_psk,
            incomparables = c("", NA, "0000")
          )
        ]
      # make potential matches based on phonetic and fuzzy OSA distances
      bucket_phonetic_matches <- phonetic_street_key_fuzzy_match(
        ifelse(
          is_ordinal_phonetic_key(bucket_nomatch_name_psk),
          bucket_nomatch_name_psk,
          bucket_nomatch_psk
        ),
        bucket_uy_psk,
        osa_max_dist = name_phonetic_dist
      )
      bucket_fuzzy_matches <- fuzzy_match(
        bucket_nomatch_names,
        bucket_uy_names,
        osa_max_dist = name_fuzzy_dist
      )
      # Avoid matching one ordinal street number to a different one
      # just because the exact ordinal is absent from this bucket.
      bucket_fuzzy_matches[
        nomatch_is_ordinal[bucket_nomatch_idx] & is.na(bucket_nomatch_psk)
      ] <- list(integer(0))
      bucket_matches <-
        mapply(
          union,
          bucket_fuzzy_matches,
          bucket_phonetic_matches,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE
        )

      m[bucket_nomatch_idx] <- lapply(
        bucket_matches,
        \(idx) bucket_idx[idx[!is.na(idx)]]
      )
    }
    # take first in multiple matches (prefers fuzzy match)
    lkp[nomatch_idx] <- vapply(
      m,
      \(idx) if (length(idx) == 0) NA_integer_ else idx[1],
      integer(1)
    )
  }

  names(lkp) <- ux_key
  out_idx <- unname(lkp[x_key])
  out <- uy_df[out_idx, , drop = FALSE] |>
    vec_restore(to = addr::addr_street())
  return(out)
}

#' Match addr_number vectors
#'
#' @description
#' A single addr_number in y is chosen for each addr_number in x.
#' If exact matches (using `as.character`) are not found,
#' possible matches (within `number_fuzzy_distance`) are searched for in y.
#' If multiple matches are present in y, the best one is selected
#' based on the lowest absolute numeric difference with the @digits in x;
#' ties are broken by optimized string alignment (OSA) distances
#' and then preferring the lowest value sorted in Lexicographic
#' order with digits preceding alphabetic characters.
#'
#' addr_number objects with missing @digits or empty strings
#' for all of @prefix, @digits, @suffix are not matched and
#' returned as missing instead.
#' @param x,y addr_number vectors to match
#' @param number_fuzzy_dist integer; maximum optimized string alignment distance
#' between `@number` of x and y to consider a possible match
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
#' match_addr_number(x, y, number_fuzzy_dist = 0L)
match_addr_number <- function(x, y, number_fuzzy_dist = 1L) {
  stopifnot(
    "x must be an addr_number object" = inherits(x, "addr_number"),
    "y must be an addr_number object" = inherits(y, "addr_number"),
    "osa_max_dist must be an integer" = typeof(number_fuzzy_dist) == "integer"
  )
  ux <- unique(x[!is.na(x@digits) & x@digits != ""])
  uy <- unique(y[!is.na(y@digits) & y@digits != ""])
  if (length(ux) == 0L) {
    return(x[rep(NA_integer_, length(x))])
  }
  lkp <- lapply(
    seq_along(ux),
    function(.i) {
      if (as.character(ux[.i]) %in% as.character(uy)) {
        return(ux[.i])
      }
      if (number_fuzzy_dist) {
        m <- uy[
          stringdist::stringdist(
            as.character(ux[.i]),
            as.character(uy),
            method = "osa"
          ) <=
            number_fuzzy_dist
        ]
        if (length(m) == 0) {
          return(addr::addr_number())
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
      return(addr::addr_number())
    }
  )
  names(lkp) <- as.character(ux)
  lkp[sapply(lkp, is.na)] <- NULL # remove unmatched
  out_l <- lkp[as.character(x)]
  empties <- which(sapply(out_l, is.null))
  out_l[empties] <- replicate(length(empties), addr::addr_number())
  out <-
    do.call(rbind, lapply(out_l, as.data.frame)) |>
    vec_restore(to = addr::addr_number())
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
  x <- addr::addr_place(zipcode = x)@zipcode
  y <- addr::addr_place(zipcode = y)@zipcode
  ux <- unique(x[!is.na(x) & x != ""])
  uy <- unique(y[!is.na(y) & y != ""])
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
  out[is.na(x) | x == ""] <- NA_character_
  names(out) <- NULL
  out
}

zipcode_variant <- function(x) {
  stopifnot(typeof(x) == "character", length(x) == 1L)
  x <- addr::addr_place(zipcode = x)@zipcode
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
