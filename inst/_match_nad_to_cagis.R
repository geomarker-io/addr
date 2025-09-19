devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

# get cincy addresses from CAGIS and filter out misc types (~ 38k addrs)
packageVersion("codec")
rlang::check_installed("codec", "to get cincy addr geo")
d_cagis <-
  codec::cincy_addr_geo() |>
  filter(
    !cagis_address_type %in%
      c("MM", "PAR", "PRJ", "CTW", "LOT", "MIS", "RR", "TBA")
  ) |>
  mutate(cagis_addr = as_addr(cagis_address))

# use Hamilton County subset of NAD, which is 911-based addresses and geocodes
d_nad <- nad_addr()

# link each NAD address to its CAGIS addresses (and parcel IDs) based on addr matching
d <-
  d_nad |>
  mutate(
    cagis_addr = addr_match(
      nad_addr,
      d_cagis$cagis_addr,
      simplify = FALSE,
      max_dist_street_name = 2,
      max_dist_street_type = 0,
      max_dist_street_number = 0
    )
  )
saveRDS(d, "jic.rds")

# use NAD geocodes to link to nearest CAGIS addr
# match nearest parcel and calculate some distances

cagis_parcels <-
  dpkg::stow("gh://geomarker-io/parcel/cagis_parcels-v1.1.1") |>
  arrow::read_parquet() |>
  sf::st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = 4326) |>
  select(parcel_id)

d <- d |> sf::st_as_sf(coords = c("nad_lon", "nad_lat"), crs = 4326)

d$which_closest_parcel <- sf::st_nearest_feature(d, cagis_parcels)

d$dist_to_closest_parcel <-
  sf::st_distance(d, cagis_parcels[d$which_closest_parcel, ], by_element = TRUE)

summary(d$dist_to_closest_parcel)

d$closest_parcel_id <- slice(cagis_parcels, d$which_closest_parcel)$parcel_id

# consider multi-matches missing parcel ID
d$cagis_addr <-
  d$cagis_addr |>
  purrr::modify_if(\(.) length(.) != 1, \(.) NA) |>
  purrr::list_c(ptype = addr())

d <-
  left_join(
    d,
    select(sf::st_drop_geometry(d_cagis), cagis_parcel_id, cagis_addr),
    by = "cagis_addr"
  )

sum(paste0("0", d$closest_parcel_id) == paste0(d$cagis_parcel_id, "00")) /
  nrow(d)

d |>
  filter(is.na(cagis_parcel_id)) |>
  sf::st_drop_geometry() |>
  select(nad_address, closest_parcel_id, cagis_parcel_id) |>
  write.csv("NAD hamilton addresses not matched by addr.csv")

d[as.numeric(d$dist_to_closest_parcel) > 100, "closest_parcel_id"] <- NA

sum(is.na(d$closest_parcel_id)) / nrow(d)

sum(is.na(d$cagis_parcel_id)) / nrow(d)

sum(is.na(d$cagis_parcel_id) & is.na(d$closest_parcel_id)) / nrow(d)
