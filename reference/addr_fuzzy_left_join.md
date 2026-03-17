# Left join two data frames using fuzzy addr matching

This is a convenience wrapper around the addr fuzzy matching helpers
that returns a left-join style result. The addr columns are matched by
index and rows are expanded for one-to-many or many-to-many matches.

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
  tibble::tibble(address = voter_addresses()[1:1000],
                 addr = as_addr(address),
                 id = sprintf("id_%04d", seq_len(1000)))
#> Warning: street name post type parsed but not mapped: la
the_addr <- nad_example_data()
addr_fuzzy_left_join(my_addr, the_addr, c("addr", "nad_addr"))
#> 49 of 49 unique ZIP codes in x matched to one of 60 unique ZIP codes in y
#> matching by zipcode ■■                                 2% |  ETA:  3m
#> matching by zipcode ■■■                                6% |  ETA:  2m
#> matching by zipcode ■■■                                8% |  ETA:  1m
#> matching by zipcode ■■■■■                             14% |  ETA:  1m
#> matching by zipcode ■■■■■■■                           18% |  ETA:  1m
#> matching by zipcode ■■■■■■■■                          22% |  ETA:  1m
#> matching by zipcode ■■■■■■■■■■                        31% |  ETA: 46s
#> matching by zipcode ■■■■■■■■■■■                       33% |  ETA: 46s
#> matching by zipcode ■■■■■■■■■■■■■                     41% |  ETA: 38s
#> matching by zipcode ■■■■■■■■■■■■■■■                   47% |  ETA: 33s
#> matching by zipcode ■■■■■■■■■■■■■■■■■                 53% |  ETA: 28s
#> matching by zipcode ■■■■■■■■■■■■■■■■■■■               61% |  ETA: 22s
#> matching by zipcode ■■■■■■■■■■■■■■■■■■■■■             67% |  ETA: 18s
#> matching by zipcode ■■■■■■■■■■■■■■■■■■■■■■■           73% |  ETA: 15s
#> matching by zipcode ■■■■■■■■■■■■■■■■■■■■■■■■■■■       88% |  ETA:  6s
#> Warning: Multi-matches detected for 23 addr in x;
#> More than one row of y will be returned once for each matching row in x
#> # A tibble: 1,796 × 10
#>    address             addr  id    nad_addr.y subaddress uuid  date_update s2   
#>    <chr>               <add> <chr> <addr>     <chr>      <chr> <date>      <s2_>
#>  1 3359 QUEEN CITY AV… 3359… id_0… 3359 QUEE… NA         {E3A… 2025-03-30  -6.7…
#>  2 5179 RIVERWATCH DR… 5179… id_0… 5179 RIVE… NA         {CDE… 2025-03-30  -6.7…
#>  3 4724 GLENWAY AVE C… 4724… id_0… 4724 GLEN… NA         {EB2… 2025-03-30  -6.7…
#>  4 2375 FAIRGREEN DR … 2375… id_0… 2375 FAIR… NA         {204… 2025-03-30  -6.7…
#>  5 259 CALVERTON LN C… 259 … id_0…            NA         NA    NA              …
#>  6 1034 FASHION AVE C… 1034… id_0… 1034 FASH… NA         {DDD… 2025-03-30  -6.7…
#>  7 2720 QUEEN CITY AV… 2720… id_0… 2720 QUEE… NA         {D94… 2025-03-30  -6.7…
#>  8 4450 CLOVERHILL TE… 4450… id_0… 4450 CLOV… NA         {55F… 2025-03-30  -6.7…
#>  9 175 ANDERSON FERRY… 175 … id_0… 175 ANDER… NA         {715… 2025-03-30  -6.7…
#> 10 5270 WILLNET DR CI… 5270… id_0… 5270 WILL… NA         {A79… 2025-03-30  -6.7…
#> # ℹ 1,786 more rows
#> # ℹ 2 more variables: address_type <chr>, parcel_id <chr>
```
