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
