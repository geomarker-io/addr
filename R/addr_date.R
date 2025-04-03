
#' Impute date ranges from a chronological sequence of dates
#'
#' Imputed date ranges are calculated as the midpoints between the
#' leading and lagging dates, except for the start of the first date range
#' and the end of the last date range, which are used directly.
#' @param x a (chronologically sorted) Date vector
#' @param start_early start the first imputed date range this many days early (coerced to integer)
#' @param end_late end the last imputed date range this many days late (coerced to integer)
#' @returns a list of `start` and `end` Date vectors for the imputed ranges for each input date in x
#' @details Use this function to impute an effective date range for a set of
#' addresses or location identifiers that were collected on unrelated days.
#' For example, residential addresses collected during a specific healthcare
#' encounter do not reflect when a patient actually changed addresses.
#' Imputing address date ranges for linking to other (spatio)temporal
#' data ensures *non-differential* exposure misclassification error with
#' respect to the changing exposures associated with each address.
#' @examples
#' impute_date_ranges(c("2024-01-01", "2024-03-17", "2024-09-21"),
#'                   start_early = 30, end_late = 60)
#'
#' # use within a data.frame with multiple individuals
#' tibble::tribble(
#'   ~id, ~encounter, ~date,
#'   "A", 1, "2024-01-01",
#'   "A", 2, "2024-03-17",
#'   "A", 3, "2024-09-21",
#'   "B", 1, "2023-11-29",
#'   "B", 2, "2024-09-22",
#'   "B", 3, "2024-09-29"
#' ) |>
#'   dplyr::mutate(
#'     imputed_start_date = impute_date_ranges(date)$start,
#'     imputed_end_date = impute_date_ranges(date)$end,
#'     .by = "id"
#'   )
impute_date_ranges <- function(x, start_early = 0, end_late = 0) {
  x <- as.Date(x)
  start_early <- as.integer(start_early)
  end_late <- as.integer(end_late)
  if (!identical(x, sort(x))) rlang::abort("date vectors must be ordered chronologically")

  # TODO special case where x is length 1 (or 0??)

  i_start <- x + ((dplyr::lag(x) - x) / 2)
  i_start[1] <- x[1] - start_early

  i_end <- dplyr::lead(i_start)
  i_end[length(i_end)] <- x[length(x)] + end_late

  out <- list("start" = i_start, "end" = i_end)

  return(out)
}




## d_geomarker_imputed_dates <-
##   d_geomarker |>
##   group_by(PAT_MRN_ID) |>
##   filter(n() > 1) |>
##   arrange(EFF_START_DATE, .by_group = TRUE) |>
##   ungroup() |>
##   mutate(
##     imputed_start = c(
##       min(EFF_START_DATE) - start_early,
##       (EFF_START_DATE + (lag(EFF_START_DATE) - EFF_START_DATE) / 2)[-1]
##     ),
##     imputed_end = c(
##       lead(imputed_start)[-n()],
##       max(EFF_START_DATE) + end_late
##     ),
##     .by = "PAT_MRN_ID"
##   )
