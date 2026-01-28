devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

d <- tibble::tibble(address = voter_addresses())

cagis_s2 <-
  cagis_addr()$cagis_addr_data |>
  purrr::modify_if(
    \(.) length(.) > 0 && nrow(.) > 1,
    dplyr::slice_sample,
    n = 1
  ) |>
  purrr::map_vec(
    purrr::pluck,
    "cagis_s2",
    .default = NA,
    .ptype = s2::s2_cell()
  )

d_amg <-
  addr_match_geocode(
    d$address,
    ref_addr = cagis_addr()$cagis_addr,
    ref_s2 = cagis_s2,
    county = "39061",
    year = "2022"
  )

d_amg |>
  summarize(n = n(), .by = match_method) |>
  mutate(percent = scales::percent(n / sum(n)))

d_geocodes <-
  bind_cols(d, d_amg) |>
  mutate(census_block_group_id = s2_join_tiger_bg(s2, year = "2022"))

saveRDS(d_geocodes, "inst/voter_geocode_addr.rds")
