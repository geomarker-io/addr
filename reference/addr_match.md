# Match addr vectors

A single addr in `y` is chosen for each addr in `x`. Matching is staged
to reduce the search space: ZIP codes are matched first, street names
are then matched within each matched ZIP code, and street numbers are
finally matched within each matched street and ZIP code combination. If
more than one candidate addr remains in `y` after these stages, the
first candidate in `y` is returned.

Missing or empty address components that cannot be matched at any stage
are left missing in the returned
[`addr()`](https://geomarker.io/addr/reference/addr.md) values. Rows
with a matched ZIP code but no street match return an addr with only
`@place@zipcode` filled; rows with matched ZIP code and street but no
number match also return the matched `@street`.

`addr_match()` accepts raw reference data and prepares it internally,
which is the right default for one-off matching jobs.
`addr_match_prepare()` becomes useful when the same reference `y` will
be reused across multiple calls to `addr_match()`, because it caches the
deduplicated reference addresses and ZIP/street/number candidate lookups
once instead of rebuilding them on every call.

Preparing `y` once avoids recomputing `unique(y)`, ZIP-code groups, and
exact street/number candidate lookups each time you call `addr_match()`
with the same reference addresses. For a single end-to-end match,
preparing `y` explicitly does not remove that work; it only moves it
outside `addr_match()`.

## Usage

``` r
addr_match(
  x,
  y,
  zip_variants = TRUE,
  osa_max_dist = 1L,
  progress = interactive()
)

addr_match_prepare(y)
```

## Arguments

- x:

  addr vector to match

- y:

  addr vector to match against, or a prepared `addr_match_index` created
  by `addr_match_prepare()`

- zip_variants:

  logical; fuzzy match to common ZIP code variants in
  [`match_zipcodes()`](https://geomarker.io/addr/reference/match_zipcodes.md)?

- osa_max_dist:

  integer maximum OSA distance used by
  [`match_addr_number()`](https://geomarker.io/addr/reference/match_addr_number.md)

- progress:

  logical; show reference-preparation timing for raw `y` and a progress
  bar while processing matched ZIP groups?

## Value

an addr vector, the same length as x, that is the best match in y for
each addr in x. Partial matches are returned with matched ZIP code
and/or street fields filled when later stages do not match.

## Examples

``` r
my_addr <- as_addr(voter_addresses()[1:10])
the_addr <- nad_example_data()$nad_addr

addr_match(my_addr, the_addr)
#> <addr>
#>  @ number: <addr_number> function ()  
#>  .. @ prefix: chr [1:10] "" "" "" "" "" "" "" "" "" ""
#>  .. @ digits: chr [1:10] "3359" "1040" "9960" "413" "8519" "6361" "10466" ...
#>  .. @ suffix: chr [1:10] "" "" "" "" "" "" "" "" "" ""
#>  @ street: <addr_street> function ()  
#>  .. @ predirectional : chr [1:10] "" "" "" "" "" "" "" "" "" ""
#>  .. @ premodifier    : chr [1:10] "" "" "" "" "" "" "" "" "" ""
#>  .. @ pretype        : chr [1:10] "" "" "" "" "" "" "" "" "" ""
#>  .. @ name           : chr [1:10] "QUEEN CITY" "KREIS" "DALY" "VOLKERT" "LINDERWOOD" ...
#>  .. @ posttype       : chr [1:10] "Ave" "Ln" "Rd" "Pl" "Ln" "Ave" "Ln" "Cir" "Ave" ...
#>  .. @ postdirectional: chr [1:10] "" "" "" "" "" "" "" "" "" ""
#>  @ place : <addr_place> function ()  
#>  .. @ name   : chr [1:10] "CINCINNATI" "CINCINNATI" "CINCINNATI" "CINCINNATI" ...
#>  .. @ state  : chr [1:10] "OH" "OH" "OH" "OH" "OH" "OH" "OH" "OH" "OH" "OH"
#>  .. @ zipcode: chr [1:10] "45238" "45205" "45231" "45219" "45255" "45230" ...
```
