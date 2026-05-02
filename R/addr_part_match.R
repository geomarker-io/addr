#' Match addr_street vectors
#'
#' @description
#' A single `addr_street` in `y` is chosen for each `addr_street` in `x`.
#' If exact matches (using `as.character`) are not found,
#' candidate matches are chosen by
#' fuzzy matching on street name (using phonetic street key and street name)
#' and exact matching on the enabled street components.
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
#' @param match_street_predirectional logical; require street predirectional to
#'   match when selecting street candidates?
#' @param match_street_posttype logical; require street posttype to match when
#'   selecting street candidates?
#' @param match_street_pretype logical; require street pretype to match when
#'   selecting street candidates?
#' @param match_street_postdirectional logical; require street postdirectional
#'   to match when selecting street candidates?
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
#' # predirectional is required by default, so blank "14th St" stays unmatched
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
#'   match_street_predirectional = FALSE
#' ))
#'
#' # posttype can also be made optional during fuzzy street-name matching
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
#'   match_street_posttype = FALSE
#' ))
#'
#' # pretype is required by default; postdirectional can also be required
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
#'   match_street_pretype = FALSE,
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
#'   match_street_pretype = TRUE,
#'   match_street_postdirectional = TRUE
#' ))
match_addr_street <- function(
  x,
  y,
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_predirectional = TRUE,
  match_street_posttype = TRUE,
  match_street_pretype = TRUE,
  match_street_postdirectional = FALSE
) {
  stopifnot(
    "x must be an addr_street object" = inherits(x, "addr_street"),
    "y must be an addr_street object" = inherits(y, "addr_street")
  )
  validate_match_addr_street_args(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_predirectional = match_street_predirectional,
    match_street_posttype = match_street_posttype,
    match_street_pretype = match_street_pretype,
    match_street_postdirectional = match_street_postdirectional
  )
  street_match_fields <- c(
    if (match_street_predirectional) "street_predirectional",
    if (match_street_pretype) "street_pretype",
    if (match_street_posttype) "street_posttype",
    if (match_street_postdirectional) "street_postdirectional"
  )
  street_key <- function(df) {
    parts <- lapply(
      df[c("street_premodifier", "street_name", street_match_fields)],
      \(col) ifelse(is.na(col) | col == "", "", col)
    )
    trimws(gsub(" +", " ", do.call(paste, c(parts, sep = " "))))
  }
  street_bucket_key <- function(df) {
    if (length(street_match_fields) == 0L) {
      return(rep("", nrow(df)))
    }
    bucket_df <- df[street_match_fields]
    out <- do.call(
      paste,
      c(
        lapply(bucket_df, \(col) {
          ifelse(is.na(col), "", ifelse(col == "", "<EMPTY>", col))
        }),
        sep = "\r"
      )
    )
    out[rowSums(is.na(bucket_df)) > 0L] <- NA_character_
    out
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

  uy_bucket_key <- street_bucket_key(uy_df)
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
    nomatch_bucket_key <- street_bucket_key(nomatch_df)
    m <- replicate(nrow(nomatch_df), integer(0), simplify = FALSE)

    for (bucket_key in unique(nomatch_bucket_key[!is.na(nomatch_bucket_key)])) {
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

validate_match_addr_street_args <- function(
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_predirectional = TRUE,
  match_street_posttype = TRUE,
  match_street_pretype = TRUE,
  match_street_postdirectional = FALSE
) {
  stopifnot(
    "name_phonetic_dist must be an integer" = typeof(name_phonetic_dist) ==
      "integer",
    "name_phonetic_dist must be length one" = length(name_phonetic_dist) == 1L,
    "name_phonetic_dist must not be missing" = !is.na(name_phonetic_dist),
    "match_street_predirectional must be TRUE or FALSE" = is.logical(
      match_street_predirectional
    ) &&
      length(match_street_predirectional) == 1L &&
      !is.na(match_street_predirectional),
    "name_fuzzy_dist must be an integer" = typeof(name_fuzzy_dist) == "integer",
    "name_fuzzy_dist must be length one" = length(name_fuzzy_dist) == 1L,
    "name_fuzzy_dist must not be missing" = !is.na(name_fuzzy_dist),
    "match_street_posttype must be TRUE or FALSE" = is.logical(
      match_street_posttype
    ) &&
      length(match_street_posttype) == 1L &&
      !is.na(match_street_posttype),
    "match_street_pretype must be TRUE or FALSE" = is.logical(
      match_street_pretype
    ) &&
      length(match_street_pretype) == 1L &&
      !is.na(match_street_pretype),
    "match_street_postdirectional must be TRUE or FALSE" = is.logical(
      match_street_postdirectional
    ) &&
      length(match_street_postdirectional) == 1L &&
      !is.na(match_street_postdirectional)
  )
  invisible(TRUE)
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
#' `x` in `y`? (for example, changing the fourth or fifth digit)
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
match_zipcodes <- function(x, y, zip_variants = TRUE) {
  stopifnot(
    "x must be a character vector" = is.character(x),
    "y must be a character vector" = is.character(y),
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants)
  )
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

#' Create ZIP code variants
#'
#' @description
#'
#' An input ZIP code is used to generate variants (for, e.g., 45220):
#' - `minus1`: substracting one from zipcode (45219)
#' - `plus1`: adding one to zipcode (45219)
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
  variant <- match.arg(variant, several.ok = TRUE)
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
