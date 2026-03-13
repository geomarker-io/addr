#' Convert street names into phonetic matching keys
#'
#' Ordinal street names (e.g., "11TH", "5TH") are encoded as zero-padded numeric
#' identifiers with a special prefix, while non-ordinal street names are encoded
#' using a Soundex phonetic code (see `?stringdist::phonetic`).
#' Ordinal words (e.g., "Eleventh", "Fifth") are
#' detected and converted automatically.
#' Each phonetic key is exactly four characters long.
#' @param x character vector
#' @returns character vector
#' @export
#' @examples
#' phonetic_street_key(
#'   c("MEADOWLARK", "TOWNSEND", "IMMACULATE", "7TH", "WERK",
#'     "PAXTON", "5th", "BURNET", "FIFTH", "CLIFTON")
#' )
phonetic_street_key <- function(x) {
  xm <- map_ordinal_street_names(x)
  xord <- is_ordinal_street_number(xm)
  xs <- soundex(xm)
  xs[xord] <- sprintf("#%04d", ordinal_street_number(xm[xord]))
  xs
}

soundex <- function(x) {
  stopifnot("x must be a character vector" = is.character(x))
  x <- gsub("[^A-Z]", "", toupper(x))
  ux <- unique(stats::na.omit(x))
  sdx <- stringdist::phonetic(ux, method = "soundex", useBytes = FALSE)
  out <- sdx[match(x, ux)]
  out[is.na(x)] <- NA
  return(out)
}

map_ordinal_street_names <- function(x) {
  stopifnot("x must be a character vector" = is.character(x))
  x_ord <- is_ordinal_street_number(x)
  x[x_ord] <- toupper(trimws(x[x_ord]))
  xwo <- canonical_ordinal(parse_word_ordinal(x))
  x[!is.na(xwo)] <- xwo[!is.na(xwo)]
  x
}

is_ordinal_street_number <- function(x) {
  grepl("^[0-9]+(ST|ND|RD|TH)$", x, ignore.case = TRUE)
}

ordinal_street_number <- function(x) {
  as.integer(sub("(ST|ND|RD|TH)$", "", toupper(x)))
}

canonical_ordinal <- function(n) {
  stopifnot(typeof(n) == "integer")
  suffix <- ifelse(
    n %% 100 %in% c(11, 12, 13),
    "TH",
    c("TH", "ST", "ND", "RD", "TH", "TH", "TH", "TH", "TH", "TH")[n %% 10 + 1]
  )
  out <- paste0(n, suffix)
  out[is.na(n)] <- NA_character_
  return(out)
}

parse_word_ordinal <- function(x) {
  x <- toupper(x)
  x <- gsub("-", " ", x, fixed = TRUE)
  x <- gsub("[^A-Z ]", "", x)
  x <- trimws(gsub("\\s+", " ", x))
  words <- strsplit(x, " ", fixed = TRUE)
  vapply(
    words,
    function(w) {
      if (length(w) == 1 && is.na(w)) {
        return(NA_integer_)
      }
      if (length(w) == 1 && w %in% names(.ordinal_words)) {
        return(.ordinal_words[w])
      }
      if (length(w) == 1 && w %in% names(.tens_words)) {
        return(.tens_words[w])
      }
      if (
        length(w) == 2 &&
          w[1] %in% names(.tens_cardinal_words) &&
          w[2] %in% names(.unit_ordinal_words)
      ) {
        return(.tens_cardinal_words[w[1]] + .unit_ordinal_words[w[2]])
      }
      NA_integer_
    },
    integer(1),
    USE.NAMES = FALSE
  )
}

.ordinal_words <- c(
  FIRST = 1L,
  SECOND = 2L,
  THIRD = 3L,
  FOURTH = 4L,
  FIFTH = 5L,
  SIXTH = 6L,
  SEVENTH = 7L,
  EIGHTH = 8L,
  NINTH = 9L,
  TENTH = 10L,
  ELEVENTH = 11L,
  TWELFTH = 12L,
  THIRTEENTH = 13L,
  FOURTEENTH = 14L,
  FIFTEENTH = 15L,
  SIXTEENTH = 16L,
  SEVENTEENTH = 17L,
  EIGHTEENTH = 18L,
  NINETEENTH = 19L
)

.tens_words <- c(
  TWENTIETH = 20L,
  THIRTIETH = 30L,
  FORTIETH = 40L,
  FIFTIETH = 50L,
  SIXTIETH = 60L,
  SEVENTIETH = 70L,
  EIGHTIETH = 80L,
  NINETIETH = 90L
)

.tens_cardinal_words <- c(
  TWENTY = 20L,
  THIRTY = 30L,
  FORTY = 40L,
  FIFTY = 50L,
  SIXTY = 60L,
  SEVENTY = 70L,
  EIGHTY = 80L,
  NINETY = 90L
)

.unit_ordinal_words <- .ordinal_words[1:9]
