#' Example National Address Database addresses
#'
#' @description
#' An example of the data returned using `nad()` for
#' Hamilton County, Ohio (release `NAD_r22.zip`). See `?nad` for more
#' information about the National Address Database.
#'
#' `nad("Hamilton", "OH", refresh_source = "no", refresh_binary = "no")`
#' and `nad("39061", refresh_source = "no", refresh_binary = "no")` are
#' equivalent to `nad_example_data()`.
#'
#' @param match_prepared logical; return the example data preprocessed with
#'   `addr_match_prepare()`?
#' @returns If `match_prepared = FALSE`, a tibble with 349,407 rows and 7
#'   columns. If `match_prepared = TRUE`, an `addr_match_index`.
#' @export
#' @examples
#' nad_example_data()
#' nad_example_data(match_prepared = TRUE)
nad_example_data <- function(match_prepared = FALSE) {
  stopifnot(
    "match_prepared must be TRUE or FALSE" = is.logical(match_prepared) &&
      length(match_prepared) == 1L &&
      !is.na(match_prepared)
  )

  if (match_prepared) {
    return(
      readRDS(
        system.file(
          "extdata",
          "nad_39061_mp.rds",
          package = "addr",
          mustWork = TRUE
        )
      )
    )
  }

  readRDS(
    system.file(
      "extdata",
      "nad_39061.rds",
      package = "addr",
      mustWork = TRUE
    )
  )
}

#' Example addresses
#'
#' `voter_addresses()` returns an example character vector of real-world
#' addresses downloaded from the Hamilton County, Ohio voter registration
#' database on 2024-09-12. `AddressPreDirectional`, `AddressNumber`,
#' `AddressStreet`, `AddressSuffix`, `CityName`, `"OH"`, and `AddressZip`
#' were pasted together to create 242,133 unique registered-voter addresses.
#' @returns a character vector
#' @export
#' @examples
#' voter_addresses() |>
#'   head()
voter_addresses <- function() {
  readRDS(
    system.file(
      "extdata",
      "voter_addresses.rds",
      package = "addr",
      mustWork = TRUE
    )
  )
}

#' Example line-one addresses
#'
#' The Cincinnati Eviction Hotspots data was downloaded from
#' [Eviction Labs](https://evictionlab.org/uploads/cincinnati_hotspots_media_report.csv)
#' and contains characteristics of the top 100 buildings that are responsible for about 25% of
#' all eviction filings in Cincinnati (from their "current through 8-31-2024" release).
#' @details https://evictionlab.org/eviction-tracking/cincinnati-oh/
#' @returns a tibble with 100 rows and 9 columns
#' @export
#' @examples
#' elh_data()
elh_data <- function() {
  readRDS(
    system.file(
      "extdata",
      "elh_data.rds",
      package = "addr",
      mustWork = TRUE
    )
  )
}
