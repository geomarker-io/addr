as_norm_char <- function(x) tolower(trimws(as.character(x)))

to_int <- function(x) {
  suppressWarnings(as.integer(gsub("[^0-9.-]", "", x)))
}


is_zip5 <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^[0-9]{5}$", x)
}

is_valid_zipcode <- function(x) {
  out <- rep(FALSE, length(x))
  present <- !is.na(x) & x != ""
  out[present] <- grepl("^[0-9]{5}$", x[present]) &
    !grepl("^000", x[present])
  out
}

is_invalid_zipcode <- function(x) {
  !is.na(x) & x != "" & !is_valid_zipcode(x)
}

addr_problem_examples <- function(x, i, n = 5L, width = 120L) {
  i <- i[!is.na(i)]
  if (length(i) == 0L) {
    return("")
  }
  i_show <- utils::head(i, n)
  values <- as.character(x[i_show])
  values[is.na(values)] <- "<NA>"
  values <- gsub("[\r\n\t]+", " ", values)
  long <- nchar(values, type = "width") > width
  values[long] <- paste0(substr(values[long], 1L, width - 3L), "...")
  out <- paste(
    sprintf("%s: %s", i_show, encodeString(values, quote = "\"")),
    collapse = ", "
  )
  if (length(i) > n) {
    out <- paste0(out, ", ... and ", length(i) - n, " more")
  }
  out
}

recycle_fields <- function(fields, class_name) {
  lens <- vapply(fields, length, integer(1))
  target <- max(lens, 0L)
  if (target == 0L) {
    return(fields)
  }
  if (any(lens == 0L)) {
    stop(
      sprintf(
        "%s fields must all be length 0 or length 1/%d",
        class_name,
        target
      ),
      call. = FALSE
    )
  }
  ok <- lens %in% c(1L, target)
  if (!all(ok)) {
    stop(
      sprintf(
        "%s fields must have length 1 or %d for recycling",
        class_name,
        target
      ),
      call. = FALSE
    )
  }
  lapply(fields, function(x) if (length(x) == 1L) rep(x, target) else x)
}

check_recyclable_lengths <- function(self, field_names, class_name) {
  lens <- vapply(
    field_names,
    function(nm) length(S7::prop(self, nm)),
    integer(1)
  )
  target <- max(lens, 0L)
  if (target == 0L) {
    return(NULL)
  }
  if (any(lens == 0L)) {
    return(
      sprintf(
        "%s fields must all be length 0 or length 1/%d",
        class_name,
        target
      )
    )
  }
  ok <- lens %in% c(1L, target)
  if (!all(ok)) {
    return(
      sprintf(
        "%s fields must have length 1 or %d for recycling",
        class_name,
        target
      )
    )
  }
  NULL
}

# print_addr_vector <- function(x) {
#   if (length(x) == 0L) {
#     cat("\n")
#     return(invisible(x))
#   }
#   cat(paste(x, collapse = ", "), sep = "\n")
#   invisible(x)
# }
