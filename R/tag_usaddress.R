#' Tag US addresses
#'
#' @description
#' Addresses are tagged using the usaddress conditional random field
#' in a [rust port](https://github.com/boydjohnson/usaddress-rs) of
#' [usaddress](https://github.com/datamade/usaddress).
#' Possible address labels include:
#'
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
#' Find more information about the definitions at
#' <https://www.fgdc.gov/standards/projects/address-data>
#'
#' @param x character string of addresses
#' @param clean logical; clean address text with clean_address_text()
#' before tagging?
#' @return a list of vectors of named address tags
#' @export
#' @examples
#' tag_usaddress(
#'   c("290 Ludlow Avenue Apt 2 Cincinnati OH 45220",
#'   "3333 Burnet Ave Cincinnati Ohio 45219",
#'   "120 North Main Street, Greenville, SC 29601",
#'   "200 Southwest North Street, Topeka, KS 66603",
#'   "215 Highway 88 Road, Jackson, CA 95642"
#'   )
#' )
#'
#' # edge cases!
#' tag_usaddress(
#'   c(
#'     "1600 Pennsylvania Avenue NW, Washington, DC 20500", # post-directional quadrant
#'     "1 Infinite Loop, Cupertino, CA 95014", # corporate campus street name
#'     "210 East 400 South, Salt Lake City, UT 84111", # grid addressing (Utah)
#'     "N6W23001 Bluemound Road, Wauwatosa, WI 53226", # address number prefix grid (Wisconsin)
#'     "350 Fifth Avenue, New York, NY 10118", # ordinal street name
#'     "4059 Mt Lee Drive, Hollywood, CA 90068", # abbreviated street element
#'     "233 South Wacker Drive, Chicago, IL 60606", # pre-directional
#'     "700 Exposition Park Drive, Los Angeles, CA 90037", # multi-word street name
#'     "2 South Biscayne Boulevard, Miami, FL 33131" # directional + boulevard
#'   )
#' )
tag_usaddress <- function(x = NA_character_, clean = TRUE) {
  stopifnot(
    "x must be a character vector" = is.character(x),
    "clean must be TRUE or FALSE" = is.logical(clean) &&
      length(clean) == 1L &&
      !is.na(clean)
  )
  if (clean) {
    x <- clean_address_text(x)
  }
  ux <- unique(stats::na.omit(x))
  if (length(ux) == 0L) {
    return(as.list(rep(NA, times = length(x))))
  }
  tags <- usaddress_tag(ux)
  out <- tags[match(x, ux)]
  out[is.na(x)] <- NA
  return(out)
}
