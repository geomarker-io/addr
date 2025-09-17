#' National Address Database for Hamilton County, OH
#'
#' The U.S. Department of Transportation partners with address programs from state,
#' local, and tribal governments to compile their authoritative data into a database.
#' Find more information here: https://www.transportation.gov/gis/national-address-database
#' @return Tibble of addresses from the National Address Database with columns for
#' addr, the unique id, coordinate placement method, parcel identifier, source,
#' and s2_geography.
#' @details created with `inst/make_NAD_addr.R`
#' @export
#' @examples
#' nad_addr()
#' x <- addr_match(as_addr("224 Woopler Ave Cinti Oh 45220"), nad_addr()$nad_addr, simplify = TRUE)
#' x_which_nad <- which(nad_addr()$nad_addr == x)
#' nad_addr()[x_which_nad, ]
nad_addr <- function() {
  readRDS(fs::path_package("addr", "NAD_OH_Hamilton.rds"))
}

#' CAGIS Addresses (Q4 2024)
#'
#' @returns An example tibble created from the CAGIS addresses with a pre-calculated, unique `cagis_addr` vector column.
#' The `cagis_addr_data` column is a list of tibbles because one CAGIS address can correspond to multiple
#' parcel identifiers and address-level data (place, type, s2, etc.).
#' See `inst/make_cagis_addr.R` for source code to create data, including filtering criteria:
#'
#' - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
#' - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#'   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
#' - s2 cell is derived from LONGITUDE and LATITUDE fields in CAGIS address database
#'
#' This export is not the latest version of CAGIS address data, but that can be installed with `codec::cincy_addr_geo()`
#' @export
#' @examples
#' cagis_addr()
cagis_addr <- function() {
  readRDS(fs::path_package("addr", "cagis_addr.rds"))
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
