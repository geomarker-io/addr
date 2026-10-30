# Geocode addresses from your own CSV.
library(addr)
addresses <- read.csv("my_addresses.csv")
addresses

# First use downloads TIGER data; this session uses cached data.
g <- geocode(as_addr(addresses$address))

# See which addresses got an estimated point.
table(geocode_stage(g))

# Add coordinates and match status to your original rows.
addresses$longitude <- s2::s2_x(g$matched_geography)
addresses$latitude <- s2::s2_y(g$matched_geography)
addresses$match <- geocode_stage(g)
addresses

# Locations are estimated from street address ranges.
write.csv(addresses, "geocoded_addresses.csv", row.names = FALSE)
