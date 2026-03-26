#' Example National Address Database addresses
#'
#' An example of the data returned using `nad_read()` for
#' Hamilton County, Ohio.  See `?nad_read` for more information about
#' the National Address Database.
#' @param match_prepare logical; return the example data preprocessed with
#'   `addr_match_prepare()`?
#' @returns If `match_prepare = FALSE`, a tibble with 349,407 rows and 7
#'   columns. If `match_prepare = TRUE`, an `addr_match_index`.
#' @export
#' @examples
#' nad_example_data()
#' nad_example_data(match_prepare = TRUE)
nad_example_data <- function(match_prepare = FALSE) {
  stopifnot(
    "match_prepare must be TRUE or FALSE" = is.logical(match_prepare) &&
      length(match_prepare) == 1L &&
      !is.na(match_prepare)
  )

  if (match_prepare) {
    return(readRDS(fs::path_package("addr", "nad_39061_match_prepared.rds")))
  }

  readRDS(fs::path_package("addr", "nad_39061.rds"))
}

#' Example addresses
#'
#' The voter_addresses data was generated as an example character vector of real-world addresses.
#' These addresses were downloaded from the Hamilton County, Ohio voter registration database on 2024-09-12.
#' See `inst/make_example_addresses.R` for more details.
#' `AddressPreDirectional`, `AddressNumber`, `AddressStreet`, `AddressSuffix`, `CityName`, "OH", and `AddressZip`
#' are pasted together to create 242,133 unique addresses of registered voters in Hamilton County, OH.
#' @returns a character vector
#' @export
#' @examples
#' voter_addresses() |>
#'   head()
voter_addresses <- function() {
  readRDS(fs::path_package("addr", "voter_addresses.rds"))
}

#' Example line-one addresses
#'
#' The Cincinnati Evicition Hotspots data was downloaded from
#' [Eviction Labs](https://evictionlab.org/uploads/cincinnati_hotspots_media_report.csv)
#' and contains characteristics of the top 100 buildings that are responsible for about 25% of
#' all eviction filings in Cincinnati (from their "current through 8-31-2024" release).
#' @details https://evictionlab.org/eviction-tracking/cincinnati-oh/
#' @returns a tibble with 100 rows and 9 columns
#' @export
#' @examples
#' elh_data()
elh_data <- function() {
  readRDS(fs::path_package("addr", "elh_data.rds"))
}
