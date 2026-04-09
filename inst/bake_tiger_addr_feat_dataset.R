devtools::load_all()

mirai::daemons(parallelly::availableCores() - 1)

cnty_fips <- county_fips_reference$county_fips

system.time({
  mm <- mirai::mirai_map(
    cnty_fips,
    \(.) {
      devtools::load_all()
      taf_install(., year = "2025")
    }
  )
  mmo <- mm[.flat, .stop, .progress]
})
