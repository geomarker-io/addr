devtools::load_all()

nad_download(refresh_source = "no")

cnty_fips <- c(
  "39025",
  "39017",
  "39015",
  "39061",
  "21117",
  "21037",
  "18137"
)
# cnty_fips <- county_fips_reference$county_fips

local({
  for (cnt in cnty_fips) {
    message(cnt)
    nad(
      county = cnt,
      state = NULL,
      refresh_binary = "yes",
      refresh_source = "no"
    )
  }
})
