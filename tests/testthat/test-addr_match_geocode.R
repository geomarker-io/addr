test_that("addr_match_geocode() works", {
  cagis_s2 <-
    cagis_addr()$cagis_addr_data |>
    purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
    purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())
  my_geocodes <- addr_match_geocode(x = voter_addresses()[1000:1250], ref_s2 = cagis_s2)
  table(my_geocodes$match_method) |>
    expect_identical(
      structure(
        c(ref_addr = 216L, tiger_range = 16L, tiger_street = 2L, none = 17L),
        dim = 4L,
        dimnames = structure(list(c("ref_addr", "tiger_range", "tiger_street", "none")), names = ""),
        class = "table"
      )
    )
})
