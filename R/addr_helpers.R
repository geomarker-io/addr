is_zip5 <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^[0-9]{5}$", x)
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

addr_component_length <- function(x, field_names) {
  length(S7::prop(x, field_names[[1]]))
}

addr_component_fields <- function(x, field_names) {
  stats::setNames(
    lapply(field_names, function(nm) S7::prop(x, nm)),
    field_names
  )
}

recycle_addr_component <- function(x, field_names, target, ctor) {
  fields <- addr_component_fields(x, field_names)
  fields <- lapply(fields, function(v) {
    if (length(v) == 1L) rep(v, target) else v
  })
  do.call(ctor, fields)
}

format_addr_tokens <- function(...) {
  parts <- list(...)
  lens <- vapply(parts, length, integer(1))
  target <- max(lens, 0L)
  if (target == 0L) {
    return(character(0))
  }
  parts <- lapply(parts, function(x) if (length(x) == 1L) rep(x, target) else x)
  vapply(
    seq_len(target),
    function(i) {
      vals <- vapply(parts, function(x) x[[i]], character(1))
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) == 0L) "" else paste(vals, collapse = " ")
    },
    character(1)
  )
}

format_addr_number_tokens <- function(prefix, digits, suffix) {
  parts <- list(prefix = prefix, digits = digits, suffix = suffix)
  lens <- vapply(parts, length, integer(1))
  target <- max(lens, 0L)
  if (target == 0L) {
    return(character(0))
  }
  parts <- lapply(parts, function(x) if (length(x) == 1L) rep(x, target) else x)
  vapply(
    seq_len(target),
    function(i) {
      vals <- vapply(parts, function(x) x[[i]], character(1))
      vals <- vals[!is.na(vals) & vals != ""]
      if (length(vals) == 0L) "" else paste(vals, collapse = "")
    },
    character(1)
  )
}

print_addr_vector <- function(x) {
  if (length(x) == 0L) {
    cat("\n")
    return(invisible(x))
  }
  cat(paste(x, collapse = ", "), sep = "\n")
  invisible(x)
}
