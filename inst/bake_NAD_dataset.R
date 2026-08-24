devtools::load_all()

nad_download(version = 23L, refresh_source = "no")

cnty_fips <- county_fips_reference$county_fips

options(nwarnings = 10000)

purrr::walk(
  cnty_fips,
  \(.) {
    nad_install(
      county = .,
      version = 23L,
      overwrite = FALSE,
      refresh_source = "no"
    )
  },
  .progress = "revision 23: installing full nad dataset"
)

manifest <- nad_manifest(version = 23L, validate = TRUE)
stopifnot(
  "NAD manifest must contain every reference county" = setequal(
    manifest$county_fips,
    cnty_fips
  )
)
