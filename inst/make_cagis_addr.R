devtools::load_all()
library(dplyr, warn.conflicts = FALSE)
options(
  timeout = max(2500, getOption("timeout")),
  download.file.method = "libcurl"
)

install_cagis_data <- function(cagis_data_url) {
  cagis_gdb_name <- tools::file_path_sans_ext(basename(cagis_data_url))
  dest <- file.path(tools::R_user_dir(package = "addr", "data"), cagis_gdb_name)
  if (file.exists(dest)) {
    return(dest)
  }
  tmp <- tempfile(fileext = ".zip")
  utils::download.file(cagis_data_url, destfile = tmp, mode = "wb")
  unzip(tmp, exdir = dirname(dest))
  return(dest)
}

# read in cagis addresses:
# - use only addresses that have `STATUS` of `ASSIGNED` or `USING` and are not orphaned (`ORPHANFLG == "N"`)
# - omit addresses with `ADDRTYPE`s that are milemarkers (`MM`), parks (`PAR`), infrastructure projects (`PRJ`),
#   cell towers (`CTW`), vacant or commercial lots (`LOT`), and other miscellaneous non-residential addresses (`MIS`, `RR`, `TBA`)
# - s2 cell derived from LONGITUDE and LATITUDE fields in CAGIS address database
cagis_addr <-
  install_cagis_data(
    "https://www.cagis.org/Opendata/Quarterly_GIS_DATA/CAGISOpenDataQ4_2024.gdb.zip"
  ) |>
  sf::st_read(layer = "Addresses") |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  filter(STATUS %in% c("ASSIGNED", "USING")) |>
  filter(ORPHANFLG == "N") |>
  filter(
    !ADDRTYPE %in% c("MM", "PAR", "PRJ", "CTW", "LOT", "MIS", "RR", "TBA")
  ) |>
  transmute(
    cagis_address = FULLMAILADR,
    cagis_addr = addr(cagis_address),
    cagis_address_place = BLDGPLACE,
    cagis_address_type = ADDRTYPE,
    cagis_s2 = s2::as_s2_cell(s2::s2_geog_point(LONGITUDE, LATITUDE)),
    cagis_parcel_id = PARCELID,
    cagis_is_condo = CONDOFLG %in% c("Y")
  )

cagis_addr <-
  cagis_addr |>
  nest_by(cagis_addr, .key = "cagis_addr_data") |>
  ungroup()

saveRDS(cagis_addr, fs::path("inst", "cagis_addr.rds"))
