test_that("addr_tag works", {
  addr_tag(c(
    "290 Ludlow Avenue Apt #2 Cincinnati OH 45220",
    "200 W 14th Street Cincinnati OH 45222",
    "3333 Burnet Ave Cincinnati OH 45219",
    "111 State Route 32 Cincinnati OH 45912"
  )) |>
    expect_equal(
      list(
        c(
          AddressNumber = "290",
          StreetName = "Ludlow",
          StreetNamePostType = "Avenue",
          OccupancyType = "Apt",
          OccupancyIdentifier = "#",
          OccupancyIdentifier = "2",
          PlaceName = "Cincinnati",
          StateName = "OH",
          ZipCode = "45220"
        ),
        c(
          AddressNumber = "200",
          StreetNamePreDirectional = "W",
          StreetName = "14th",
          StreetNamePostType = "Street",
          PlaceName = "Cincinnati",
          StateName = "OH",
          ZipCode = "45222"
        ),
        c(
          AddressNumber = "3333",
          StreetName = "Burnet",
          StreetNamePostType = "Ave",
          PlaceName = "Cincinnati",
          StateName = "OH",
          ZipCode = "45219"
        ),
        c(
          AddressNumber = "111",
          StreetNamePreType = "State",
          StreetNamePreType = "Route",
          StreetName = "32",
          PlaceName = "Cincinnati",
          StateName = "OH",
          ZipCode = "45912"
        )
      )
    )
})
