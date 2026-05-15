#' Match addr_street vectors
#'
#' @description
#' A single `addr_street` in `y` is chosen for each `addr_street` in `x`.
#' If exact matches (using `as.character`) are not found,
#' candidate matches are chosen by
#' fuzzy matching on street name (using phonetic street key and street name)
#' and matching the street type and directional components according to
#' `match_street_type` and `match_street_directional`.
#' Ordinal street names use restricted phonetic candidates:
#' an ordinal phonetic key like `#0007` may fuzzy match only to plausible
#' ordinal neighbors such as digit shifts (`#0070`, `#0700`, `#7000`)
#' or same-width substitutions (`#0008`, `#0009`), not arbitrary
#' OSA-distance-one ordinal keys such as `#0017` or `#0077`.
#' If multiple candidates remain after fuzzy matching, the first candidate in
#' `y` is returned.
#'
#' `addr_street` objects with missing or empty `@name` are not matched and
#' returned as missing instead.
#' @param x,y addr_street vectors to match
#' @param name_phonetic_dist integer; maximum optimized string alignment
#' distance between `phonetic_street_key()` of x and y to consider a possible
#' match
#' @param name_fuzzy_dist integer; maximum optimized string alignment distance
#' between `@name` of x and y to consider a possible match
#' @param match_street_type character; how to compare street pretype and
#'   posttype when selecting street candidates. `"exact"` requires pretype to
#'   match pretype and posttype to match posttype; `"swap"` also permits pretype
#'   to match posttype and posttype to match pretype; `"ignore"` does not use
#'   street type fields when selecting candidates.
#' @param match_street_directional character; how to compare street
#'   predirectional and postdirectional when selecting street candidates.
#'   `"exact"` requires predirectional to match predirectional and
#'   postdirectional to match postdirectional; `"swap"` also permits
#'   predirectional to match postdirectional and postdirectional to match
#'   predirectional; `"ignore"` does not use street directional fields when
#'   selecting candidates.
#' @return An `addr_street` vector, the same length as `x`, containing the
#'   selected match in `y` for each element of `x`. Unmatched elements are
#'   returned as missing `addr_street()` values.
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
#'
#' toggle_y <- addr_street(
#'   predirectional = c("E", "", "", "E"),
#'   premodifier = "",
#'   pretype = c("", "", "US Hwy", "US Hwy"),
#'   name = c("14th", "Oak", "Main", "Main"),
#'   posttype = c("St", "Rd", "Rd", "Rd"),
#'   postdirectional = c("", "", "", "E"),
#'   map_pretype = FALSE,
#'   map_posttype = FALSE,
#'   map_directional = FALSE,
#'   map_ordinal = FALSE
#' )
#'
#' # directionals are required by default, so blank "14th St" stays unmatched
#' format(match_addr_street(
#'   addr_street(
#'     predirectional = "",
#'     premodifier = "",
#'     pretype = "",
#'     name = "14th",
#'     posttype = "St",
#'     postdirectional = "",
#'     map_pretype = FALSE,
#'     map_posttype = FALSE,
#'     map_directional = FALSE,
#'     map_ordinal = FALSE
#'   ),
#'   toggle_y
#' ))
#' format(match_addr_street(
#'   addr_street(
#'     predirectional = "",
#'     premodifier = "",
#'     pretype = "",
#'     name = "14th",
#'     posttype = "St",
#'     postdirectional = "",
#'     map_pretype = FALSE,
#'     map_posttype = FALSE,
#'     map_directional = FALSE,
#'     map_ordinal = FALSE
#'   ),
#'   toggle_y,
#'   match_street_directional = "ignore"
#' ))
#'
#' # type can also be ignored during fuzzy street-name matching
#' format(match_addr_street(
#'   addr_street(
#'     predirectional = "",
#'     premodifier = "",
#'     pretype = "",
#'     name = "Oka",
#'     posttype = "Ave",
#'     postdirectional = "",
#'     map_pretype = FALSE,
#'     map_posttype = FALSE,
#'     map_directional = FALSE,
#'     map_ordinal = FALSE
#'   ),
#'   toggle_y,
#'   name_fuzzy_dist = 1L
#' ))
#' format(match_addr_street(
#'   addr_street(
#'     predirectional = "",
#'     premodifier = "",
#'     pretype = "",
#'     name = "Oka",
#'     posttype = "Ave",
#'     postdirectional = "",
#'     map_pretype = FALSE,
#'     map_posttype = FALSE,
#'     map_directional = FALSE,
#'     map_ordinal = FALSE
#'   ),
#'   toggle_y,
#'   name_fuzzy_dist = 1L,
#'   match_street_type = "ignore"
#' ))
#'
#' # type and directional matching can be relaxed independently
#' format(match_addr_street(
#'   addr_street(
#'     predirectional = "E",
#'     premodifier = "",
#'     pretype = "US Hwy",
#'     name = "Mian",
#'     posttype = "Rd",
#'     postdirectional = "E",
#'     map_pretype = FALSE,
#'     map_posttype = FALSE,
#'     map_directional = FALSE,
#'     map_ordinal = FALSE
#'   ),
#'   toggle_y,
#'   match_street_type = "ignore",
#'   name_fuzzy_dist = 1L
#' ))
#' format(match_addr_street(
#'   addr_street(
#'     predirectional = "E",
#'     premodifier = "",
#'     pretype = "US Hwy",
#'     name = "Mian",
#'     posttype = "Rd",
#'     postdirectional = "E",
#'     map_pretype = FALSE,
#'     map_posttype = FALSE,
#'     map_directional = FALSE,
#'     map_ordinal = FALSE
#'   ),
#'   toggle_y,
#'   name_fuzzy_dist = 1L
#' ))
#' format(match_addr_street(
#'   addr_street(
#'     predirectional = "E",
#'     premodifier = "",
#'     pretype = "US Hwy",
#'     name = "Mian",
#'     posttype = "Rd",
#'     postdirectional = "E",
#'     map_pretype = FALSE,
#'     map_posttype = FALSE,
#'     map_directional = FALSE,
#'     map_ordinal = FALSE
#'   ),
#'   toggle_y,
#'   name_fuzzy_dist = 1L,
#'   match_street_type = "exact",
#'   match_street_directional = "exact"
#' ))
match_addr_street <- function(
  x,
  y,
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_type = c("exact", "swap", "ignore"),
  match_street_directional = c("exact", "swap", "ignore")
) {
  stopifnot(
    "x must be an addr_street object" = inherits(x, "addr_street"),
    "y must be an addr_street object" = inherits(y, "addr_street")
  )
  match_args <- validate_match_addr_street_args(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_type = match_street_type,
    match_street_directional = match_street_directional
  )
  match_street_type <- match_args$match_street_type
  match_street_directional <- match_args$match_street_directional

  x_df <- as.data.frame(x)
  y_df <- as.data.frame(y)

  keep_ux <- !is.na(x_df$street_name) & x_df$street_name != ""
  ux_df <- x_df[keep_ux, , drop = FALSE]
  if (nrow(ux_df) == 0L) {
    return(x[rep(NA_integer_, length(x))])
  }

  keep_uy <- !is.na(y_df$street_name) & y_df$street_name != ""
  uy_df <- y_df[keep_uy, , drop = FALSE]
  if (nrow(uy_df) == 0L) {
    return(x[rep(NA_integer_, length(x))])
  }
  uy_name_psk <- phonetic_street_key(uy_df$street_name)

  x_key_variants <- street_match_key_variants(
    ux_df,
    type = match_street_type,
    directional = match_street_directional,
    include_name = TRUE
  )
  y_key_variants <- street_match_key_variants(
    uy_df,
    type = match_street_type,
    directional = match_street_directional,
    include_name = TRUE
  )
  lkp <- street_ranked_key_match(x_key_variants, y_key_variants, nrow(ux_df))

  uy_bucket_variants <- street_match_key_variants(
    uy_df,
    type = match_street_type,
    directional = match_street_directional,
    include_name = FALSE
  )
  uy_bucket_idx <- split(
    seq_len(nrow(uy_bucket_variants)),
    uy_bucket_variants$key
  )

  if (any(is.na(lkp))) {
    nomatch_idx <- which(is.na(lkp))
    nomatch_df <- ux_df[nomatch_idx, , drop = FALSE]
    nomatch_name_psk <- phonetic_street_key(nomatch_df$street_name)
    nomatch_is_ordinal <- is_ordinal_street_number(nomatch_df$street_name)
    nomatch_bucket_variants <- street_match_key_variants(
      nomatch_df,
      type = match_street_type,
      directional = match_street_directional,
      include_name = FALSE
    )
    ranked_matches <- list()

    for (bucket_key in unique(nomatch_bucket_variants$key)) {
      y_variant_idx <- uy_bucket_idx[[bucket_key]]
      if (is.null(y_variant_idx) || length(y_variant_idx) == 0) {
        next
      }
      x_variant_idx <- which(nomatch_bucket_variants$key == bucket_key)
      y_variants <- uy_bucket_variants[y_variant_idx, , drop = FALSE]
      x_variants <- nomatch_bucket_variants[x_variant_idx, , drop = FALSE]

      bucket_idx <- y_variants$row
      bucket_uy_names <- uy_df$street_name[bucket_idx]
      bucket_uy_psk <- uy_name_psk[bucket_idx]

      bucket_nomatch_idx <- x_variants$row
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

      bucket_matches <- lapply(seq_along(bucket_fuzzy_matches), \(i) {
        fuzzy_idx <- bucket_fuzzy_matches[[i]]
        phonetic_idx <- setdiff(bucket_phonetic_matches[[i]], fuzzy_idx)
        idx <- c(fuzzy_idx, phonetic_idx)
        idx <- idx[!is.na(idx)]
        if (length(idx) == 0L) {
          return(NULL)
        }
        data.frame(
          x = bucket_nomatch_idx[[i]],
          y = bucket_idx[idx],
          component_rank = x_variants$rank[[i]] + y_variants$rank[idx],
          name_rank = c(
            rep(0L, length(fuzzy_idx)),
            rep(1L, length(phonetic_idx))
          )[seq_along(idx)],
          stringsAsFactors = FALSE
        )
      })
      ranked_matches <- c(ranked_matches, bucket_matches)
    }

    ranked_matches <-
      ranked_matches[!vapply(ranked_matches, is.null, logical(1))]
    if (length(ranked_matches) > 0L) {
      ranked_matches <- do.call(rbind, ranked_matches)
      ranked_matches <- ranked_matches[order(
        ranked_matches$x,
        ranked_matches$component_rank,
        ranked_matches$name_rank,
        ranked_matches$y
      ), , drop = FALSE]
      first <- !duplicated(ranked_matches$x)
      lkp[nomatch_idx[ranked_matches$x[first]]] <- ranked_matches$y[first]
    }
  }

  out_idx <- rep(NA_integer_, nrow(x_df))
  out_idx[keep_ux] <- which(keep_uy)[lkp]
  out <- y_df[out_idx, , drop = FALSE] |>
    vec_restore(to = addr::addr_street())
  return(out)
}

street_match_modes <- c("exact", "swap", "ignore")

validate_street_match_mode <- function(x, arg) {
  tryCatch(
    match.arg(x, street_match_modes),
    error = function(e) {
      stop(
        arg,
        " must be one of \"exact\", \"swap\", or \"ignore\"",
        call. = FALSE
      )
    }
  )
}

street_match_empty <- function(x) {
  is.na(x) | x == ""
}

street_match_token <- function(x) {
  ifelse(street_match_empty(x), "<EMPTY>", x)
}

street_match_pair_variants <- function(pre, post, mode) {
  n <- length(pre)
  row <- seq_len(n)
  if (mode == "ignore") {
    return(data.frame(
      row = row,
      key = rep("", n),
      rank = rep(0L, n),
      stringsAsFactors = FALSE
    ))
  }

  pre_key <- street_match_token(pre)
  post_key <- street_match_token(post)
  has_info <- !(street_match_empty(pre) & street_match_empty(post))
  out <- data.frame(
    row = row,
    key = paste(pre_key, post_key, sep = "\r"),
    rank = ifelse(has_info, 0L, 2L),
    stringsAsFactors = FALSE
  )

  if (mode == "swap") {
    out <- rbind(
      out,
      data.frame(
        row = row,
        key = paste(post_key, pre_key, sep = "\r"),
        rank = ifelse(has_info, 1L, 2L),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- out[order(out$row, out$key, out$rank), , drop = FALSE]
  out[!duplicated(paste(out$row, out$key, sep = "\r")), , drop = FALSE]
}

street_match_key_variants <- function(
  df,
  type = c("exact", "swap", "ignore"),
  directional = c("exact", "swap", "ignore"),
  include_name = TRUE
) {
  type <- validate_street_match_mode(type, "match_street_type")
  directional <- validate_street_match_mode(
    directional,
    "match_street_directional"
  )
  n <- nrow(df)
  if (n == 0L) {
    return(data.frame(
      row = integer(),
      key = character(),
      rank = integer(),
      stringsAsFactors = FALSE
    ))
  }

  type_variants <- street_match_pair_variants(
    df$street_pretype,
    df$street_posttype,
    type
  )
  directional_variants <- street_match_pair_variants(
    df$street_predirectional,
    df$street_postdirectional,
    directional
  )
  names(type_variants)[names(type_variants) == "key"] <- "type_key"
  names(type_variants)[names(type_variants) == "rank"] <- "type_rank"
  names(directional_variants)[names(directional_variants) == "key"] <-
    "directional_key"
  names(directional_variants)[names(directional_variants) == "rank"] <-
    "directional_rank"

  out <- merge(
    type_variants,
    directional_variants,
    by = "row",
    all = FALSE,
    sort = FALSE
  )
  base <- if (include_name) {
    paste(
      street_match_token(df$street_premodifier),
      street_match_token(df$street_name),
      sep = "\r"
    )
  } else {
    rep("", n)
  }
  out$key <- tolower(paste(
    base[out$row],
    out$type_key,
    out$directional_key,
    sep = "\r"
  ))
  out$rank <- out$type_rank + out$directional_rank
  out <- out[order(
    out$row,
    out$key,
    out$rank,
    out$type_rank,
    out$directional_rank
  ), , drop = FALSE]
  out <- out[
    !duplicated(paste(out$row, out$key, sep = "\r")),
    ,
    drop = FALSE
  ]
  out[c("row", "key", "rank", "type_rank", "directional_rank")]
}

street_ranked_key_match <- function(x_keys, y_keys, n_x) {
  out <- rep(NA_integer_, n_x)
  if (nrow(x_keys) == 0L || nrow(y_keys) == 0L) {
    return(out)
  }

  hits <- merge(
    x_keys,
    y_keys,
    by = "key",
    suffixes = c("_x", "_y"),
    sort = FALSE
  )
  if (nrow(hits) == 0L) {
    return(out)
  }

  hits$rank <- hits$rank_x + hits$rank_y
  hits$type_rank <- hits$type_rank_x + hits$type_rank_y
  hits$directional_rank <- hits$directional_rank_x + hits$directional_rank_y
  hits <- hits[order(
    hits$row_x,
    hits$rank,
    hits$type_rank,
    hits$directional_rank,
    hits$row_y
  ), , drop = FALSE]
  first <- !duplicated(hits$row_x)
  out[hits$row_x[first]] <- hits$row_y[first]
  out
}

validate_match_addr_street_args <- function(
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_type = c("exact", "swap", "ignore"),
  match_street_directional = c("exact", "swap", "ignore")
) {
  stopifnot(
    "name_phonetic_dist must be an integer" = typeof(name_phonetic_dist) ==
      "integer",
    "name_phonetic_dist must be length one" = length(name_phonetic_dist) == 1L,
    "name_phonetic_dist must not be missing" = !is.na(name_phonetic_dist),
    "name_fuzzy_dist must be an integer" = typeof(name_fuzzy_dist) == "integer",
    "name_fuzzy_dist must be length one" = length(name_fuzzy_dist) == 1L,
    "name_fuzzy_dist must not be missing" = !is.na(name_fuzzy_dist)
  )
  invisible(list(
    match_street_type = validate_street_match_mode(
      match_street_type,
      "match_street_type"
    ),
    match_street_directional = validate_street_match_mode(
      match_street_directional,
      "match_street_directional"
    )
  ))
}

#' Match addr_number vectors
#'
#' @description
#' A single `addr_number` in `y` is chosen for each `addr_number` in `x`.
#' If exact matches (using `as.character`) are not found,
#' possible matches within `number_fuzzy_dist` are searched for in `y`.
#' If multiple matches are present in `y`, the selected match has the
#' lowest absolute numeric difference from `@digits` in `x`; ties are broken
#' by optimized string alignment (OSA) distance and then by lexicographic
#' order with digits preceding alphabetic characters.
#'
#' `addr_number` objects with missing `@digits`, or with empty strings
#' for all of `@prefix`, `@digits`, and `@suffix`, are not matched and
#' returned as missing instead.
#' @param x,y addr_number vectors to match
#' @param number_fuzzy_dist integer; maximum optimized string alignment
#'   distance between `addr_number` strings in `x` and `y` to consider a
#'   possible match.
#' @return An `addr_number` vector, the same length as `x`, containing the
#'   selected match in `y` for each element of `x`. Unmatched elements are
#'   returned as missing `addr_number()` values.
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
    "number_fuzzy_dist must be an integer" = typeof(number_fuzzy_dist) ==
      "integer",
    "number_fuzzy_dist must be length one" = length(number_fuzzy_dist) == 1L,
    "number_fuzzy_dist must not be missing" = !is.na(number_fuzzy_dist)
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
#' A single ZIP code in `y` is chosen for each ZIP code in `x`.
#' By default, if exact matches are not found, common variants of ZIP codes
#' in `x` are searched for in `y` (?`zipcode_variant`)
#' If multiple variants are present in `y`, the selected match has the lowest
#' absolute numeric difference from the ZIP code in `x`; ties are broken by
#' OSA string distance and then by the
#' minimum number.
#' @param x,y character vectors of ZIP codes to match
#' @param zip_variants logical; fuzzy match to common variants of
#' `x` in `y`?
#' @param zip_variant character vector; zipcode variant types to use when
#'   `zip_variants` is `TRUE`; see `?zipcode_variant`
#' @return A character vector, the same length as `x`, containing the selected
#'   match in `y` for each ZIP code in `x`.
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
match_zipcodes <- function(
  x,
  y,
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap")
) {
  stopifnot(
    "x must be a character vector" = is.character(x),
    "y must be a character vector" = is.character(y),
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants)
  )
  zip_variant <- validate_zip_variant(zip_variant)
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
        xzv <- zipcode_variant(xz, variant = zip_variant)
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

zip_variant_choices <- function() {
  c("minus1", "plus1", "sub5", "sub4", "swap")
}

validate_zip_variant <- function(zip_variant) {
  stopifnot(
    "zip_variant must be a character vector" = is.character(zip_variant),
    "zip_variant must not be empty" = length(zip_variant) > 0L,
    "zip_variant must not contain missing values" = !any(is.na(zip_variant))
  )
  match.arg(zip_variant, zip_variant_choices(), several.ok = TRUE)
}

#' Create ZIP code variants
#'
#' @description
#'
#' An input ZIP code is used to generate variants (for, e.g., 45220):
#' - `minus1`: subtracting one from zipcode (45219)
#' - `plus1`: adding one to zipcode (45221)
#' - `sub5`: substituting the fifth digit of the ZIP code (45221, 45222, 45223, 45224, 45225, 45226, 45227, 45228, 45229)
#' - `sub4`: substituting the fourth digit of the ZIP code (45200, 45210, 45230, 45240, 45250, 45260, 45270, 45280, 45290)
#' - `swap`: swapping the second and third digits of the ZIP code (42520)
#'
#' More than one variant type can be created at once and variants will be
#' returned in the same order as they were requested (see examples).
#' @param x character length one; five digit ZIP code
#' @param variant character one or more variants to create; see description
#' @returns character vector of five digit ZIP code variants
#' @export
#' @examples
#' zipcode_variant("45220")
#'
#' # order matters!
#' zipcode_variant("45220", c("minus1", "plus1"))
#' zipcode_variant("45220", c("plus1", "minus1"))
#'
#' zipcode_variant("45220", "sub5")
#'
#' zipcode_variant("45220", "swap")
zipcode_variant <- function(
  x,
  variant = c("minus1", "plus1", "sub5", "sub4", "swap")
) {
  stopifnot(
    "x must be a character vector" = is.character(x),
    "x must be length one" = length(x) == 1L
  )
  x <- addr::addr_place(zipcode = x)@zipcode
  if (is.na(x)) {
    return(NA_character_)
  }
  if (x == "") {
    return("")
  }
  variant <- validate_zip_variant(variant)
  variant_fns <- list(
    "minus1" = function(z) as.character(as.integer(x) - 1),
    "plus1" = function(z) as.character(as.integer(x) + 1),
    "sub4" = function(z) {
      paste0(paste0(z[1:3], collapse = ""), as.character(0:9), z[5])
    },
    "sub5" = function(z) {
      paste0(paste0(z[1:4], collapse = ""), as.character(0:9))
    },
    "swap" = function(z) paste0(z[c(1, 3, 2, 4, 5)], collapse = "")
  )
  result <- lapply(variant, \(.) variant_fns[[.]](strsplit(x, "")[[1]]))
  uout <- unique(do.call(c, result))
  out <- as.character(as.integer(uout[uout != x]))
  sprintf("%05s", out)
}
