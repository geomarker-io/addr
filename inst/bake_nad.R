library(addr)

county_fips_reference <- getFromNamespace(
  "county_fips_reference",
  ns = "addr"
)
nad_download <- getFromNamespace("nad_download", ns = "addr")

nad_download(version = 23L, refresh_source = "no")

cnty_fips <- county_fips_reference$county_fips

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
  "NAD manifest must contain every reference county" = setequal(
    manifest$county_fips,
    cnty_fips
  )
)
