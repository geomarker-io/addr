devtools::load_all()

cnty_fips <- county_fips_reference$county_fips
taf_years <- 2025L

# Census publishes FEATNAMES but not ADDRFEAT files for these county
# equivalents in the 2025 TIGER release.
addrfeat_unavailable <- c(
  "60010",
  "60020",
  "60030",
  "60040",
  "60050",
  "69085",
  "69100",
  "69120"
)

# Census recommends anonymous FTP for a large number of TIGER files. This
# protocol is intentionally selected only for this bulk development script;
# normal package use remains on encrypted HTTPS.
options(
  nwarnings = 10000,
  addr.tiger_download_protocol = "ftp",
  addr.tiger_download_interval = 1
)

for (yr in as.character(taf_years)) {
  available_fips <- setdiff(cnty_fips, addrfeat_unavailable)

  for (i in seq_along(available_fips)) {
    county <- available_fips[[i]]
    message(sprintf(
      "%s: installing TAF county %s (%d/%d)",
      yr,
      county,
      i,
      length(available_fips)
    ))
    addr::taf_install(
      county,
      year = yr,
      version = "v2",
      overwrite = TRUE,
      redownload = FALSE
    )
  }

  manifest <- addr::taf_manifest(
    year = yr,
    version = "v2",
    validate = TRUE
  )
  stopifnot(
    "TAF manifest must contain every county with published ADDRFEAT data" = setequal(
      unique(manifest$county_fips),
      available_fips
    )
  )
  taf_write_catalog(manifest, year = yr, version = "v2")
  catalog <- nanoparquet::read_parquet(
    taf_catalog_source_path(year = yr, version = "v2")
  )
  stopifnot(
    "TAF catalog must contain every installed county" = setequal(
      unique(catalog$county_fips),
      unique(manifest$county_fips)
    ),
    "TAF catalog and manifest must have the same number of rows" = nrow(
      catalog
    ) ==
      nrow(manifest)
  )
}
