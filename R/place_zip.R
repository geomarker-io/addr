normalize_place_name <- function(x) {
  stopifnot("x must be a character vector" = is.character(x))

  x <- toupper(x)
  x <- trimws(gsub("[[:space:]]+", " ", x))

  suffix_pattern <-
    "[[:space:]]+(CITY|VILLAGE|TOWN|TOWNSHIP|BOROUGH|CDP)[[:punct:]]*$"
  has_suffix <- !is.na(x) & grepl(suffix_pattern, x)
  x[has_suffix] <- sub(suffix_pattern, "", x[has_suffix])
  x[has_suffix] <- sub("[[:punct:][:space:]]+$", "", x[has_suffix])

  trimws(x)
}

place_zip_variant_choices <- function() {
  c("place", "county-sub")
}

validate_place_zip_variant <- function(place_zip_variant) {
  stopifnot(
    "place_zip_variant must be a character vector" =
      is.character(place_zip_variant),
    "place_zip_variant must not be empty" = length(place_zip_variant) > 0L,
    "place_zip_variant must not contain missing values" =
      !any(is.na(place_zip_variant))
  )
  match.arg(
    place_zip_variant,
    place_zip_variant_choices(),
    several.ok = TRUE
  )
}

geocode_zip_candidates <- function(
  x,
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  place_zip_variants = TRUE,
  place_zip_variant = c("place", "county-sub")
) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants),
    "place_zip_variants must be TRUE or FALSE" =
      is.logical(place_zip_variants) &&
      length(place_zip_variants) == 1L &&
      !is.na(place_zip_variants)
  )
  zip_variant <- validate_zip_variant(zip_variant)
  place_zip_variant <- validate_place_zip_variant(place_zip_variant)

  empty <- tibble::tibble(
    input_row = integer(),
    source_zip = character(),
    source_zip_variant = character(),
    source_zip_variant_rank = integer(),
    candidate_rank = integer(),
    ZIP = character()
  )
  if (length(x) == 0L) {
    return(empty)
  }

  zip <- x@place@zipcode
  place_name <- normalize_place_name(x@place@name)
  state <- toupper(trimws(x@place@state))
  lookup_key <- paste(place_name, state, sep = "\r")
  place_lookup <- list()
  if (place_zip_variants) {
    valid_lookup <- !is.na(place_name) & place_name != "" &
      !is.na(state) & state != "" &
      !is.na(zip) & zip != ""
    wanted_keys <- unique(lookup_key[valid_lookup])
    if (length(wanted_keys) > 0L) {
      candidate_key <- paste(
        place_zip_candidates$place_name,
        place_zip_candidates$state,
        sep = "\r"
      )
      candidate_rows <- which(candidate_key %in% wanted_keys)
      place_lookup <- split(
        candidate_rows,
        candidate_key[candidate_rows],
        drop = TRUE
      )
    }
  }

  rows <- lapply(seq_along(x), function(i) {
    source_zip <- zip[[i]]
    if (is.na(source_zip) || source_zip == "") {
      return(empty)
    }

    out <- tibble::tibble(
      input_row = i,
      source_zip = source_zip,
      source_zip_variant = "exact",
      source_zip_variant_rank = 0L,
      candidate_rank = 0L,
      ZIP = source_zip
    )
    seen <- source_zip
    next_rank <- 1L
    tier_rank <- 0L

    if (
      place_zip_variants &&
        !is.na(place_name[[i]]) &&
        place_name[[i]] != "" &&
        !is.na(state[[i]]) &&
        state[[i]] != ""
    ) {
      lookup_rows <- place_lookup[[lookup_key[[i]]]]
      lookup <- if (is.null(lookup_rows)) {
        place_zip_candidates[0, , drop = FALSE]
      } else {
        place_zip_candidates[lookup_rows, , drop = FALSE]
      }
      for (place_type in place_zip_variant) {
        tier_rank <- tier_rank + 1L
        sources <- if (place_type == "place") {
          c("place", "both")
        } else {
          c("cousub", "both")
        }
        candidates <- lookup$zcta[lookup$source %in% sources]
        candidates <- candidates[!duplicated(candidates)]
        candidates <- candidates[!candidates %in% seen]
        if (length(candidates) == 0L) {
          next
        }
        add <- tibble::tibble(
          input_row = i,
          source_zip = source_zip,
          source_zip_variant = place_type,
          source_zip_variant_rank = tier_rank,
          candidate_rank = seq.int(next_rank, length.out = length(candidates)),
          ZIP = candidates
        )
        out <- vctrs::vec_rbind(out, add)
        seen <- c(seen, candidates)
        next_rank <- next_rank + length(candidates)
      }
    } else if (place_zip_variants) {
      tier_rank <- length(place_zip_variant)
    }

    if (zip_variants) {
      for (variant in zip_variant) {
        tier_rank <- tier_rank + 1L
        candidates <- zipcode_variant(source_zip, variant = variant)
        candidates <- candidates[!candidates %in% seen]
        if (length(candidates) == 0L) {
          next
        }
        add <- tibble::tibble(
          input_row = i,
          source_zip = source_zip,
          source_zip_variant = variant,
          source_zip_variant_rank = tier_rank,
          candidate_rank = seq.int(next_rank, length.out = length(candidates)),
          ZIP = candidates
        )
        out <- vctrs::vec_rbind(out, add)
        seen <- c(seen, candidates)
        next_rank <- next_rank + length(candidates)
      }
    }
    out
  })

  do.call(vctrs::vec_rbind, rows)
}
