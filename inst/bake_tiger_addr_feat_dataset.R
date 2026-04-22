devtools::load_all()

mirai::daemons(parallelly::availableCores() - 1)
# mirai::daemons(4)

cnty_fips <- county_fips_reference$county_fips
cnty_fips <- cnty_fips[
  !substr(cnty_fips, 1, 2) %in% c("60", "66", "69", "72", "78")
]

system.time({
  mm <- mirai::mirai_map(
    cnty_fips,
    \(.) {
      devtools::load_all()
      taf_install(., year = "2025")
    }
  )
  mmo <- mm[.stop, .progress]
})

mirai::daemons(0)

taf_fls <-
  fs::dir_info(
    fs::path(
      tools::R_user_dir("addr", "data"),
      "v1",
      "tiger_addr_feat",
      "2025"
    ),
    recurse = TRUE
  ) |>
  dplyr::filter(type == "file")

message(
  file.path(
    tools::R_user_dir("addr", "data"),
    "v1",
    "tiger_addr_feat",
    "2025"
  ),
  " has ",
  prettyNum(nrow(taf_fls), big.mark = ","),
  " parquet files",
  " totaling ",
  sum(taf_fls$size)
)
