#' National Address Database for select counties
#'
#' The U.S. Department of Transportation partners with address programs from state,
#' local, and tribal governments to compile their authoritative data into a database.
#' Find more information here: https://www.transportation.gov/gis/national-address-database
#' @param state_county; prespecified state county combinations that are precalculated and included in the package
#' @return Tibble of addresses from the National Address Database with columns for the concatenated address,
#' the tagged/parsed addr, the unique id, coordinate placement method, parcel identifier, source,
#' and s2_geography.
#' @details created with `inst/make_NAD_addr.R`; address components are pasted together and parsed/tagged with
#' `as_addr()`; duplicated addresses are removed and only the first NAD UUID is retained
#' @export
#' @examples
#' nad_addr()
#' x <- addr_match(as_addr("224 Woopler Ave Cinti Oh 45220"), nad_addr()$nad_addr, simplify = TRUE)
#' x_which_nad <- which(nad_addr()$nad_addr == x)
#' nad_addr()[x_which_nad, ]
#' nad_addr("OH_Franklin")
nad_addr <- function(state_county = c("OH_Hamilton", "OH_Franklin")) {
  state_county <- rlang::arg_match(state_county)
  readRDS(fs::path_package("addr", glue::glue("NAD_{state_county}.rds")))
}

#' Example real-world addresses
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

#' Example real-world data with line-one-only addresses
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
