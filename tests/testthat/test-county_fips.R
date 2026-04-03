test_that("county_fips_lookup translates county names to FIPS", {
  hamilton <- county_fips_lookup("Hamilton", "OH")
  expect_equal(hamilton$county_fips, "39061")
  expect_equal(hamilton$county, "Hamilton")
  expect_equal(hamilton$county_full, "Hamilton County")
  expect_equal(hamilton$state, "OH")

  orleans <- county_fips_lookup("Orleans", "LA")
  expect_equal(orleans$county_fips, "22071")
  expect_equal(orleans$county_full, "Orleans Parish")
})

test_that("county_fips_lookup translates FIPS to county metadata", {
  hamilton <- county_fips_lookup("39061")
  expect_equal(hamilton$county, "Hamilton")
  expect_equal(hamilton$county_full, "Hamilton County")
  expect_equal(hamilton$state, "OH")

  orleans <- county_fips_lookup("22071")
  expect_equal(orleans$county, "Orleans")
  expect_equal(orleans$county_full, "Orleans Parish")
  expect_equal(orleans$state, "LA")
})

test_that("county_fips_lookup flags ambiguous and invalid names", {
  expect_error(
    county_fips_lookup("St. Louis", "MO"),
    "is ambiguous in `MO`"
  )
  expect_error(
    county_fips_lookup("Not A County", "OH"),
    "was not found in `OH`"
  )
  expect_error(
    county_fips_lookup("99999"),
    "was not found"
  )
})
