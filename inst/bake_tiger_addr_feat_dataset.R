devtools::load_all()

cnty_fips <- county_fips_reference$county_fips
cnty_fips <- cnty_fips[
  !substr(cnty_fips, 1, 2) %in% c("60", "66", "69", "72", "78")
]

purrr::walk(
  cnty_fips,
  \(.) {
    taf_install(
      .,
      year = "2025",
      version = "v1",
      overwrite = TRUE,
      redownload = FALSE
    )
  },
  .progress = "creating tiger address feature dataset"
)

manifest <- taf_read_county_zip_manifest(year = "2025", version = "v1")
taf_write_catalog(manifest, year = "2025", version = "v1")

taf_catalog(year = "2025", version = "v1")
