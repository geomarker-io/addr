# Left join two data frames using addr matching

`addr_left_join()` is a convenience wrapper around
[`addr_match()`](https://geomarker.io/addr/reference/addr_match.md) that
returns a left-join style result. It expands rows of `x` for duplicate
rows in the original `y` that share the exact matched `addr`, but it
does not return multiple distinct candidate addresses from `y`.
[`addr_match()`](https://geomarker.io/addr/reference/addr_match.md)
still selects a single best address before this wrapper expands exact
duplicates.

## Usage

``` r
addr_left_join(
  x,
  y,
  by = "addr",
  suffix = c(".x", ".y"),
  zip_variants = TRUE,
  osa_max_dist = 1L,
  progress = interactive()
)
```

## Arguments

- x, y:

  data frames or tibbles with an addr column

- by:

  addr column name in `x` (and `y` if the same); or a length-2 character
  vector of `c(x_col, y_col)`

- suffix:

  character vector of length 2 used to suffix duplicate columns

- zip_variants:

  logical; fuzzy match to common ZIP code variants in
  [`match_zipcodes()`](https://geomarker.io/addr/reference/match_zipcodes.md)?

- osa_max_dist:

  integer maximum OSA distance used by
  [`match_addr_number()`](https://geomarker.io/addr/reference/match_addr_number.md)

- progress:

  logical; show
  [`addr_match()`](https://geomarker.io/addr/reference/addr_match.md)
  progress?

## Value

A data frame with left-join semantics. Duplicate rows in `y` with the
exact same matched `addr` are all returned. Partial ZIP-only or
street-only matches do not expand to multiple candidate rows in `y`.
