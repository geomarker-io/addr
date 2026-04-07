devtools::load_all()

#### NAD USPS crosswalk

# login and download
browseURL(
  "https://www.huduser.gov/apps/public/uspscrosswalk/download_file/ZIP_COUNTY_122025.xlsx"
)

zip_code_reference <- as.data.frame(readxl::read_excel(
  "~/Downloads/ZIP_COUNTY_122025.xlsx"
))
row.names(zip_code_reference) <- NULL

#### Census County names

county_ref_url <-
  "https://www2.census.gov/geo/docs/reference/county_adjacency/county_adjacency2025.txt"

d <- utils::read.delim(
  county_ref_url,
  sep = "|",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

d <- unique(d[c("County Name", "County GEOID")])
names(d) <- c("county_label", "county_fips")

parts <- strcapture(
  "^(.*), ([A-Z]{2})$",
  d$county_label,
  proto = list(county_full = character(), state = character())
)

county_fips_reference <- data.frame(
  county = strip_county_equivalent_suffix(parts$county_full),
  county_full = parts$county_full,
  state = parts$state,
  county_fips = sprintf("%05s", d$county_fips),
  stringsAsFactors = FALSE
)

county_fips_reference$county_norm <-
  normalize_county_name(county_fips_reference$county)
county_fips_reference$county_full_norm <-
  normalize_county_name(county_fips_reference$county_full)

county_fips_reference <- county_fips_reference[
  order(
    county_fips_reference$state,
    county_fips_reference$county,
    county_fips_reference$county_fips
  ),
]
row.names(county_fips_reference) <- NULL

save(
  zip_code_reference,
  county_fips_reference,
  file = file.path("R", "sysdata.rda"),
  compress = "bzip2"
)
