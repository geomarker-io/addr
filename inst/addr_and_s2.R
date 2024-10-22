devtools::load_all()
library(s2)
library(dplyr, warn.conflicts = FALSE)

cagis_s2 <-
  cagis_addr()$cagis_addr_data |>
  purrr::modify_if(\(.) length(.) > 0 && nrow(.) > 1, dplyr::slice_sample, n = 1) |>
  purrr::map_vec(purrr::pluck, "cagis_s2", .default = NA, .ptype = s2::s2_cell())

d <- addr_match_geocode(
  x = sample(voter_addresses(), 500),
  ref_addr = cagis_addr()$cagis_addr,
  ref_s2 = cagis_s2,
  county = "39061",
  year = "2022"
)

table(d$match_method)

d <- d |>
  filter(match_method %in% c("ref_addr", "tiger_range")) |>
  mutate(
    s2_fine_grid = s2_cell_parent(s2, 14),
    s2_coarse_grid = s2_cell_parent(s2, 13),
    across(c(s2_fine_grid, s2_coarse_grid), as.character, .names = "{.col}_char")
  )

# median cell side length in m
sqrt(median(s2_cell_area_approx(d$s2_fine_grid)))
# median area in sq km
median(s2_cell_area_approx(d$s2_fine_grid)) / 1000000

sqrt(median(s2_cell_area_approx(d$s2_coarse_grid)))
median(s2_cell_area_approx(d$s2_coarse_grid)) / 1000000

d$geometry <- sf::st_as_sfc(s2::s2_cell_center(d$s2))

bg <-
  get_tiger_block_groups("39", "2022") |>
  dplyr::filter(substr(GEOID, 1, 5) == "39061") |>
  dplyr::mutate(geometry = sf::st_as_sfc(s2_geography))

## s2_plot(bg$s2_geography, border = codec::codec_colors("grey blue"), col = codec::codec_colors("white"))
## s2::s2_plot(s2_cell_to_lnglat(d$s2), add = TRUE, col = codec::codec_colors("darkish blue"), pch = 19)

library(rdeck)

rdeck(
  map_style = "mapbox://styles/brokamrc/cm2jtbccr007801pebyz0hk43",
  initial_bounds = sf::st_bbox(bg$geometry)
) |>
  add_polygon_layer(
    data = bg,
    name = "block groups",
    get_polygon = geometry,
    opacity = 0.7,
    filled = FALSE,
    stroked = TRUE,
    get_line_color = codec::codec_colors("darkish blue"),
    get_line_width = 20,
    visible = TRUE,
    visibility_toggle = TRUE,
    pickable = FALSE
  ) |>
  add_s2_layer(
    data = d,
    name = "S2 geometry level 13 (~ 1 sq km)",
    get_s2_token = s2_coarse_grid_char,
    opacity = 0.2,
    get_fill_color = codec::codec_colors("light blue")
  ) |>
  add_s2_layer(
    data = d,
    name = "S2 geometry level 14 (~ 0.25 sq km)",
    get_s2_token = s2_fine_grid_char,
    opacity = 0.2,
    get_fill_color = codec::codec_colors("orange")
  ) |>
  add_scatterplot_layer(
    name = "S2 geometry cell center",
    data = d,
    opacity = 1,
    get_radius = 34,
    get_position = geometry,
    get_fill_color = codec::codec_colors("red"),
  )
