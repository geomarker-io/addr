library(addr)

nad_build_catalog <- getFromNamespace(
  "nad_build_catalog",
  ns = "addr"
)
nad_write_catalog <- getFromNamespace("nad_write_catalog", ns = "addr")

catalog <- nad_build_catalog(version = 23L, refresh_source = "no")
nad_write_catalog(catalog, version = 23L)
options(addr.nad_catalog_dir = file.path(getwd(), "inst", "extdata"))
catalog <- addr::nad_catalog(version = 23L)

cnty_fips <- catalog$county_fips

options(nwarnings = 10000)

for (i in seq_along(cnty_fips)) {
  county <- cnty_fips[[i]]
  message(sprintf(
    "revision 23: installing NAD county %s (%d/%d)",
    county,
    i,
    length(cnty_fips)
  ))
  addr::nad_install(
    county = county,
    version = 23L,
    overwrite = FALSE,
    refresh_source = "no"
  )
}

manifest <- addr::nad_manifest(version = 23L, validate = TRUE)
stopifnot(
  "NAD manifest must contain every catalog county" = setequal(
    manifest$county_fips,
    cnty_fips
  ),
  "NAD manifest counties must contain source records" = all(
    manifest$row_count > 0L
  )
)
