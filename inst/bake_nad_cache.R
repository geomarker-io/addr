devtools::load_all()

nad_release <- "NAD_r22.zip"

nad_gdb_file <- nad_download(release = nad_release, refresh = "yes")

cnty_fips <- c(
  "39017",
  "39015",
  "39001",
  "39071",
  "39027",
  "39061",
  "39025",
  "39165",
  "21117",
  "21081",
  "21015",
  "21037",
  "18137",
  "18047",
  "18029"
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
