#' Tag US addresses
#'
#' Addresses are tagged using the usaddress conditional random field
#' in a [rust port](https://github.com/boydjohnson/usaddress-rs) of
#' [usaddress](https://github.com/datamade/usaddress).
#' Possible address labels include: `AddressNumberPrefix`,
#' `AddressNumberSuffix`, `AddressNumber`, `BuildingName`,
#' `CornerOf`, `IntersectionSeparator`, `LandmarkName`,
#' `NotAddress`, `OccupancyIdentifier`, `OccupancyType`,
#' `PlaceName`, `Recipient`, `StateName`, `StreetNamePostDirectional`,
#' `StreetNamePostType`, `StreetNamePreDirectional`, `StreetNamePreModifier`,
#' `StreetNamePreType`, `StreetName`, `SubaddressIdentifier`, `SubaddressType`
#' `USPSBoxGroupID`, `USPSBoxGroupType`, `USPSBoxID`, `USPSBoxType`,
#' or `ZipCode`.
#'
#' Find more information about the definitions at
#' <https://www.fgdc.gov/schemas/address/>
#'
#' @param x character string of addresses
#' @param clean logical; clean address text with clean_address_text()
#' before tagging?
#' @return a list of vectors of named address tags
#' @export
#' @examples
#' tag_usaddress(c("290 Ludlow Avenue Apt 2 Cincinnati OH 45220",
#'                 "3333 Burnet Ave Cincinnati Ohio 45219",
#'                 NA, ""))
tag_usaddress <- function(x = NA_character_, clean = TRUE) {
  stopifnot("x must be a character vector" = is.character(x))
  if (clean) {
    x <- clean_address_text(x)
  }
  ux <- unique(na.omit(x))
  if (length(ux) == 0L) {
    return(as.list(rep(NA, times = length(x))))
  }
  tags <- usaddress_tag(ux)
  out <- tags[match(x, ux)]
  out[is.na(x)] <- NA
  return(out)
}
