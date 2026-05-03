devtools::load_all()
cnty_fips <- county_fips_reference$county_fips
cnty_fips <- cnty_fips[
  !substr(cnty_fips, 1, 2) %in% c("60", "66", "69", "72", "78")
]
for (yr in as.character(c(2024:2025))) {
  purrr::walk(
    cnty_fips,
    \(.) {
      taf_install(
        .,
        year = yr,
        version = "v1",
        overwrite = TRUE,
        redownload = FALSE
      )
    },
    .progress = sprintf("%s: installing full taf and catalog", yr)
  )
  manifest <- taf_read_county_zip_manifest(year = yr, version = "v1")
  taf_write_catalog(manifest, year = yr, version = "v1")
}
