#' Tag US addresses
#'
#' Addresses are tagged using the usaddress conditional random field
#' in a [rust port](https://github.com/boydjohnson/usaddress-rs) of
#' [usaddress](https://github.com/datamade/usaddress).
#' Possible address labels include:
#' - `AddressNumberPrefix`
#' - `AddressNumberSuffix`
#' - `AddressNumber`
#' - `BuildingName`
#' - `CornerOf`
#' - `IntersectionSeparator`
#' - `LandmarkName`
#' - `NotAddress`
#' - `OccupancyIdentifier`
#' - `OccupancyType`
#' - `PlaceName`
#' - `Recipient`
#' - `StateName`
#' - `StreetNamePostDirectional`
#' - `StreetNamePostType`
#' - `StreetNamePreDirectional`
#' - `StreetNamePreModifier`
#' - `StreetNamePreType`
#' - `StreetName`
#' - `SubaddressIdentifier`
#' - `SubaddressType`
#' - `USPSBoxGroupID`
#' - `USPSBoxGroupType`
#' - `USPSBoxID`
#' - `USPSBoxType`
#' - `ZipCode`
#'
#' Find more information about the definitions at <https://www.fgdc.gov/schemas/address/>
#'
#' @param x character string of addresses
#' @return a list of vectors of named address tags
#' @export
#' @examples
#' tag_usaddress(c("290 Ludlow Avenue Apt 2 Cincinnati OH 45220",
#'                 "3333 Burnet Ave Cincinnati Ohio 45219",
#'                 NA, ""))
tag_usaddress <- function(x = NA_character_) {
  stopifnot("x must be a character vector" = typeof(x) == "character")
  ux <- unique(na.omit(x))
  if (all(is.na(ux))) {
    return(as.list(rep(
      structure(character(0), names = character(0)),
      times = length(x)
    )))
  }
  tags <- usaddress_tag(ux)
  out <- tags[match(x, ux)]
  out[is.na(x)] <- NA
  return(out)
}
