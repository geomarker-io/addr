.onLoad <- function(...) {
  S7::methods_register()
}

check_installed <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("The package `%s` is required", pkg)
    if (!is.null(reason)) {
      msg <- paste0(msg, " ", reason)
    }
    msg <- paste0(msg, ". Please install it first.")
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

matrix_row_min_thresh <- function(x, thresh) {
  xmin <- min(x, na.rm = TRUE)
  best <- which(x == xmin)
  if (length(best) == 0 || xmin > thresh) {
    return(integer(0))
  }
  best
}
