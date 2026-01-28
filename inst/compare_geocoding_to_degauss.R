devtools::load_all()
library(dplyr, warn.conflicts = FALSE)
library(s2)

sessionInfo()

addr_s2 <-
  readRDS("inst/voter_geocode_addr.rds") |>
  rename(addr_s2 = s2)

#### I used https://github.com/degauss-org/geocoder.sif, but something like:
## ia_d <- tibble::tibble(address = voter_addresses())
## readr::write_csv(ia_d, "inst/addr-v0.3.0_voter_addresses.csv")

## system2(
##   "docker",
##   c(
##     "run", "--rm",
##     "-v ${PWD}/inst:/tmp",
##     "ghcr.io/degauss-org/geocoder:3.3.0-v8",
##     "addr-v0.3.0_voter_addresses.csv",
##     "all")
##   )
## )

degauss_s2 <-
  readr::read_csv(
    "inst/addr-v0.3.0_voter_addresses_geocoder_3.3.0_score_threshold_all.csv",
    col_types = readr::cols_only(
      address = readr::col_character(),
      lat = readr::col_double(),
      lon = readr::col_double(),
      precision = readr::col_factor(),
      score = readr::col_double()
    )
  ) |>
  mutate(
    geocoded = {
      precision == "range" & score > 0.7
    } |
      {
        precision == "street" & score > 0.55
      }
  )

degauss_s2[!degauss_s2$geocoded, "lat"] <- NA
degauss_s2[!degauss_s2$geocoded, "lon"] <- NA

degauss_s2 <-
  degauss_s2 |>
  mutate(
    addr = as_addr(address),
    degauss_s2 = as_s2_cell(s2_lnglat(lon, lat)),
    .keep = "unused"
  )

# doing this instead of a left_join because when some addr objects with missing address numbers
# are converted to character, they are no longer unique
d <-
  dplyr::bind_cols(
    arrange(addr_s2, addr),
    degauss_s2 = arrange(degauss_s2, addr)$degauss_s2
  ) |>
  mutate(
    addr_bg = s2_join_tiger_bg(addr_s2, year = "2020"),
    degauss_bg = s2_join_tiger_bg(degauss_s2, year = "2020"),
    s2_dist = s2_cell_distance(addr_s2, degauss_s2)
  )

# matching results
d |>
  mutate(
    degauss_matched = !is.na(degauss_s2),
    addr_matched = !is.na(addr_s2)
  ) |>
  summarize(
    n = n(),
    .by = c(addr_matched, degauss_matched)
  ) |>
  mutate(perc = scales::percent(n / sum(n), accuracy = 0.1)) |>
  arrange(desc(n)) |>
  knitr::kable()

# inspect those matched by degauss, but not addr
d |>
  filter(is.na(addr_s2) & !is.na(degauss_s2)) |>
  select(address, addr) |>
  mutate(addr = as.data.frame(addr)) |>
  tidyr::unnest(addr) |>
  filter(zip_code == "45220") |>
  print(n = 100)


# agreement on census geographies among both matched addresses
d |>
  filter(!is.na(degauss_s2) & !is.na(addr_s2)) |>
  mutate(
    bg_agree = addr_bg == degauss_bg,
    ct_agree = substr(addr_bg, 1, 11) == substr(degauss_bg, 1, 11)
  ) |>
  summarize(
    n = n(),
    across(
      c(s2_dist),
      list(
        "ptiles (5th, 25th, 50th, 75th, 95th)" = \(.) {
          paste(
            round(quantile(., probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 1),
            collapse = ", "
          )
        }
      )
    ),
    .by = c(ct_agree, bg_agree)
  ) |>
  mutate(perc = scales::percent(n / sum(n), accuracy = 0.1)) |>
  arrange(desc(n)) |>
  knitr::kable(digits = 1)
