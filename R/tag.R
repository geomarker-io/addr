#' Tag components of an address string
#'
#' The address components are tagged using a [rust port](https://github.com/boydjohnson/usaddress-rs) of
#' [usaddress](https://github.com/datamade/usaddress).
#' Component names are based upon the [United States Thoroughfare,
#' Landmark, and Postal Address Data Standard](https://www.fgdc.gov/standards/projects/address-data).
#' @param x a character vector of addresses
#' @param clean_address_text logical; use clean_address_text() to clean addresses prior to tagging?
#' @return a list, the same length as x, of named character vectors of address component tags;
#' each vector contains all space-separated elements of the cleaned address
#' and are each named based on inferred address labels (see Details)
#' @details
#' Possible address labels include:
#'
#'  - `AddressNumberPrefix`
#'  - `AddressNumberSuffix`
#'  - `AddressNumber`
#'  - `BuildingName`
#'  - `CornerOf`
#'  - `IntersectionSeparator`
#'  - `LandmarkName`
#'  - `NotAddress`
#'  - `OccupancyIdentifier`
#'  - `OccupancyType`
#'  - `PlaceName`
#'  - `Recipient`
#'  - `StateName`
#'  - `StreetNamePostDirectional`
#'  - `StreetNamePostType`
#'  - `StreetNamePreDirectional`
#'  - `StreetNamePreModifier`
#'  - `StreetNamePreType`
#'  - `StreetName`
#'  - `SubaddressIdentifier`
#'  - `SubaddressType`
#'  - `USPSBoxGroupID`
#'  - `USPSBoxGroupType`
#'  - `USPSBoxID`
#'  - `USPSBoxType`
#'  - `ZipCode`
#'
#' Find more information about the definitions [here](https://www.fgdc.gov/schemas/address/)
#' @export
#' @examples
#' addr_tag(c("290 Ludlow Avenue Apt #2 Cincinnati OH 45220", "3333 Burnet Ave Cincinnati OH 45219"))
addr_tag <- function(x, clean_address_text = TRUE) {
  if (clean_address_text) {
    x <- clean_address_text(x)
  }
  tags <- usaddress_tag(x)
  lapply(tags, \(.) stats::setNames(names(.), .))
}
