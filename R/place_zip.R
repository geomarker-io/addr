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
