# Left join two data frames using fuzzy addr matching

**This function has been replaced by
[`addr_match()`](https://geomarker.io/addr/reference/addr_match.md)**

## Usage

``` r
addr_fuzzy_left_join(
  x,
  y,
  by = "addr",
  addr_fields = NULL,
  suffix = c(".x", ".y")
)
```

## Arguments

- x, y:

  data frames or tibbles with an addr column

- by:

  addr column name in `x` (and `y` if the same); or a length-2 character
  vector of `c(x_col, y_col)`

- addr_fields:

  a named vector of osa_max_distances; if max distances for each addr
  tag field is not present a default will be used (see details).

- suffix:

  character vector of length 2 used to suffix duplicate columns

## Value

a data frame with left-join semantics

## Details

This is a convenience wrapper around the addr fuzzy matching helpers
that returns a left-join style result. The addr columns are matched by
index and rows are expanded for one-to-many or many-to-many matches.

addr_fuzzy_left_join works by matching addresses grouped by ZIP codes,
so specified osa_max_distances for any place fields are ignored Defaults
for `addr_fields`:

- number_prefix: 0

- number_digits: 0

- number_suffix: 0

- street_predirectional: 0

- street_premodifier: 0

- street_pretype: 0

- street_name: 1

- street_posttype: 0

- street_postdirectional: 0

## Examples

``` r
my_addr <-
  tibble::tibble(address = voter_addresses()[1:10],
                 addr = as_addr(address),
                 id = sprintf("id_%04d", seq_len(10)))
the_addr <- nad_example_data()
addr_fuzzy_left_join(my_addr, the_addr, c("addr", "nad_addr"))
#> Warning: addr_fuzzy_left_join() is deprecated; use addr_join() instead
#> 9 of 9 unique ZIP codes in x matched to one of 60 unique ZIP codes in y
#> matching by zipcode ■■■■■■■■                          22% |  ETA:  6s
#> matching by zipcode ■■■■■■■■■■■■■■■■■■■■■             67% |  ETA:  2s
#> # A tibble: 10 × 10
#>    address             addr  id    nad_addr.y subaddress uuid  date_update s2   
#>    <chr>               <add> <chr> <addr>     <chr>      <chr> <date>      <s2_>
#>  1 3359 QUEEN CITY AV… 3359… id_0… 3359 QUEE… NA         {E3A… 2025-03-30  -6.7…
#>  2 1040 KREIS LN CINC… 1040… id_0… 1040 KREI… NA         {D05… 2025-03-30  -6.7…
#>  3 9960 DALY RD CINCI… 9960… id_0… 9960 DALY… NA         {109… 2025-03-30  -6.1…
#>  4 413 VOLKERT PL CIN… 413 … id_0… 413 VOLKE… NA         {1BB… 2025-03-30  -6.7…
#>  5 8519 LINDERWOOD LN… 8519… id_0… 8519 LIND… NA         {F78… 2025-03-30  -6.6…
#>  6 6361 BEECHMONT AVE… 6361… id_0… 6361 BEEC… NA         {6D4… 2025-03-30  -6.6…
#>  7 10466 ADVENTURE LN… 1046… id_0… 10466 ADV… NA         {1D1… 2025-03-30  -6.1…
#>  8 3156 LOOKOUT CIR C… 3156… id_0… 3156 LOOK… NA         {AE8… 2025-03-30  -6.6…
#>  9 310 WYOMING AVE CI… 310 … id_0… 310 WYOMI… NA         {331… 2025-03-30  -6.1…
#> 10 118 SPRINGFIELD PI… 118 … id_0… 118 SPRIN… NA         {F3E… 2025-03-30  -6.1…
#> # ℹ 2 more variables: address_type <chr>, parcel_id <chr>
```
