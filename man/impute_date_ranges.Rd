% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addr_date.R
\name{impute_date_ranges}
\alias{impute_date_ranges}
\title{Impute date ranges from a chronological sequence of dates}
\usage{
impute_date_ranges(x, start_early = 0, end_late = 0)
}
\arguments{
\item{x}{a (chronologically sorted) Date vector}

\item{start_early}{start the first imputed date range this many days early (coerced to integer)}

\item{end_late}{end the last imputed date range this many days late (coerced to integer)}
}
\value{
a list of \code{start} and \code{end} Date vectors for the imputed ranges for each input date in x
}
\description{
Imputed date ranges are calculated as the midpoints between the
leading and lagging dates, except for the start of the first date range
and the end of the last date range, which are used directly.
}
\details{
Use this function to impute an effective date range for a set of
addresses or location identifiers that were collected on unrelated days.
For example, residential addresses collected during a specific healthcare
encounter do not reflect when a patient actually changed addresses.
Imputing address date ranges for linking to other (spatio)temporal
data ensures \emph{non-differential} exposure misclassification error with
respect to the changing exposures associated with each address.
}
\examples{
impute_date_ranges(c("2024-01-01", "2024-03-17", "2024-09-21"),
  start_early = 30, end_late = 60
)

# use within a data.frame with multiple individuals
tibble::tribble(
  ~id, ~encounter, ~date,
  "A", 1, "2024-01-01",
  "A", 2, "2024-03-17",
  "A", 3, "2024-09-21",
  "B", 1, "2023-11-29",
  "B", 2, "2024-09-22",
  "B", 3, "2024-09-29"
) |>
  dplyr::mutate(
    imputed_start_date = impute_date_ranges(date)$start,
    imputed_end_date = impute_date_ranges(date)$end,
    .by = "id"
  )
}
