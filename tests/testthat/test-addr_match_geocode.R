test_that("addr_match_geocode() works", {
  my_geocodes <-
    addr_match_geocode(
      voter_addresses()[1000:1250],
      as_addr(codec::cincy_addr_geo()$cagis_address),
      ref_s2 = codec::cincy_addr_geo()$cagis_s2
    )
  table(my_geocodes$match_method) |>
    expect_identical(
      structure(
        c(ref_addr = 249L, tiger_range = 2L, tiger_street = 0L, none = 0L),
        dim = 4L,
        dimnames = structure(
          list(c("ref_addr", "tiger_range", "tiger_street", "none")),
          names = ""
        ),
        class = "table"
      )
    )
})
