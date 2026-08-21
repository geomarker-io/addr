devtools::load_all()

#### Census ZCTA to place and county subdivision relationships

zcta_relationship_urls <- c(
  place = paste0(
    "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/",
    "tab20_zcta520_place20_natl.txt"
  ),
  cousub = paste0(
    "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/",
    "tab20_zcta520_cousub20_natl.txt"
  )
)

state_fips_to_usps <- c(
  "01" = "AL",
  "02" = "AK",
  "04" = "AZ",
  "05" = "AR",
  "06" = "CA",
  "08" = "CO",
  "09" = "CT",
  "10" = "DE",
  "11" = "DC",
  "12" = "FL",
  "13" = "GA",
  "15" = "HI",
  "16" = "ID",
  "17" = "IL",
  "18" = "IN",
  "19" = "IA",
  "20" = "KS",
  "21" = "KY",
  "22" = "LA",
  "23" = "ME",
  "24" = "MD",
  "25" = "MA",
  "26" = "MI",
  "27" = "MN",
  "28" = "MS",
  "29" = "MO",
  "30" = "MT",
  "31" = "NE",
  "32" = "NV",
  "33" = "NH",
  "34" = "NJ",
  "35" = "NM",
  "36" = "NY",
  "37" = "NC",
  "38" = "ND",
  "39" = "OH",
  "40" = "OK",
  "41" = "OR",
  "42" = "PA",
  "44" = "RI",
  "45" = "SC",
  "46" = "SD",
  "47" = "TN",
  "48" = "TX",
  "49" = "UT",
  "50" = "VT",
  "51" = "VA",
  "53" = "WA",
  "54" = "WV",
  "55" = "WI",
  "56" = "WY",
  "60" = "AS",
  "66" = "GU",
  "69" = "MP",
  "72" = "PR",
  "78" = "VI"
)

read_zcta_relationship <- function(source, url) {
  geography <- switch(
    source,
    place = "PLACE",
    cousub = "COUSUB",
    stop(sprintf("unsupported relationship source: %s", source))
  )
  geoid_length <- switch(source, place = 7L, cousub = 10L)

  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  utils::download.file(url, path, mode = "wb", quiet = TRUE)

  d <- utils::read.delim(
    path,
    sep = "|",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM",
    colClasses = "character"
  )

  geoid_column <- paste0("GEOID_", geography, "_20")
  name_column <- paste0("NAMELSAD_", geography, "_20")
  required_columns <- c("GEOID_ZCTA5_20", geoid_column, name_column)
  missing_columns <- setdiff(required_columns, names(d))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "%s relationship file is missing columns: %s",
        source,
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  zcta <- d[["GEOID_ZCTA5_20"]]
  geoid <- d[[geoid_column]]
  place_name <- normalize_place_name(d[[name_column]])
  state_fips <- substr(geoid, 1L, 2L)
  state <- unname(state_fips_to_usps[state_fips])

  valid <-
    !is.na(place_name) &
    nzchar(place_name) &
    !is.na(zcta) &
    grepl("^[0-9]{5}$", zcta) &
    !is.na(geoid) &
    grepl(
      sprintf("^[0-9]{%d}$", geoid_length),
      geoid
    ) &
    !is.na(state)

  unique(data.frame(
    place_name = place_name[valid],
    state = state[valid],
    zcta = zcta[valid],
    source = source,
    stringsAsFactors = FALSE
  ))
}

place_candidates <- read_zcta_relationship(
  "place",
  zcta_relationship_urls[["place"]]
)
cousub_candidates <- read_zcta_relationship(
  "cousub",
  zcta_relationship_urls[["cousub"]]
)

place_keys <- place_candidates[c("place_name", "state", "zcta")]
place_keys$has_place <- TRUE
cousub_keys <- cousub_candidates[c("place_name", "state", "zcta")]
cousub_keys$has_cousub <- TRUE

place_zip_candidates <- merge(
  place_keys,
  cousub_keys,
  by = c("place_name", "state", "zcta"),
  all = TRUE,
  sort = FALSE
)
place_zip_candidates$source <- ifelse(
  !is.na(place_zip_candidates$has_place) &
    !is.na(place_zip_candidates$has_cousub),
  "both",
  ifelse(
    !is.na(place_zip_candidates$has_place),
    "place",
    "cousub"
  )
)
place_zip_candidates$has_place <- NULL
place_zip_candidates$has_cousub <- NULL
place_zip_candidates <- place_zip_candidates[
  order(
    place_zip_candidates$state,
    place_zip_candidates$place_name,
    place_zip_candidates$zcta,
    method = "radix"
  ),
]
row.names(place_zip_candidates) <- NULL

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
  county_fips_reference,
  place_zip_candidates,
  file = file.path("R", "sysdata.rda"),
  compress = "bzip2"
)
