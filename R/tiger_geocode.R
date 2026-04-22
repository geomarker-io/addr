# Geocode using TIGER address features
#
# @description
# geocode_tiger() geocodes addr vectors using TIGER address
# range geometries downloaded from census.gov for a specific county.
#
# The geocoding precision is the level of matching achieved for each address:
# - `range`: is the most precise; the address matched on street name and
# ZIP code, and the number was within one of the ranges; point locations are
# interpolated from the geometry of the best match using the street number
# - `street`: is less precise; the address matched on street name and ZIP code,
# but the number was *not* within one of the ranges; point locations
# are interpolated as the centroid of all matching range geometries
# - `unmatched`: no precision; the address did not match any street name
# and ZIP code combinations; point locations are `<null feature>` (which is
# equivalent to `NA`)
# @param x an addr vector
# @inheritParams tiger_addr_feat
# @param offset number of meters to offset geocode from street line
# @returns a s2_geography vector of geocoded point locations;
# an attribute ("geocode_precision") is added as a factor with
# levels `range`, `street`, or `unmatched`.
# @details
# If a street number-name has multiple in range and parity matches, then
# the best match is chosen based on the smallest width of ranges and
# then on the range with the closest midpoint.
#
# @export
# @examples
# \dontrun{
# gcd <-
#   geocode_tiger(as_addr(voter_addresses()[1:100]),
#                 county = "39061", year = "2024", offset = 20)
#
# head(gcd)
#
# table(attr(gcd, "geocode_precision"))
#
# # consider addresses matched to street but out of range missing
# gcd[attr(gcd, "geocode_precision") == "street"] <- NA_character_
#
# # convert to s2_cell
# s2::as_s2_cell(gcd)
#
# # create leaflet map
# leaflet::leaflet(wk::wk_coords(gcd)) |>
#   leaflet::addTiles() |>
#   leaflet::addCircleMarkers( lng = ~x, lat = ~y, label = ~ feature_id)
# }
NULL
