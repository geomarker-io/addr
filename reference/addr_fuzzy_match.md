# Fuzzy match addr vectors using field-specific string distances

`addr_fuzzy_match()` matches two addr vectors using more than one
address tag

`fuzzy_match_addr_field()` matches two addr vectors using a single
address tag

Distances between address tags are defined using optimized string
alignment; see
[`fuzzy_match()`](https://geomarker.io/addr/reference/fuzzy_match.md)
and
[`stringdist::stringdist()`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
for more details.

## Usage

``` r
addr_fuzzy_match(x, y, addr_fields = NULL)

fuzzy_match_addr_field(
  x,
  y,
  addr_field = c("number_prefix", "number_digits", "number_suffix",
    "street_predirectional", "street_premodifier", "street_pretype", "street_name",
    "street_posttype", "street_postdirectional", "place_name", "place_state",
    "place_zipcode"),
  osa_max_dist = 0
)
```

## Arguments

- x:

  addr vector to match

- y:

  addr vector to match to

- addr_fields:

  a named vector of osa_max_distances; if max distances for each addr
  tag field is not present a default will be used (see details).

- addr_field:

  character name of single addr field to match on

- osa_max_dist:

  maximum optimized string alignment distance used as threshold for
  matching on single addr field

## Value

a list of integer vectors representing the position of the best matching
address(es) in `y` for each address in `x`

## Details

Defaults for `addr_fields`:

- number_prefix: 0

- number_digits: 0

- number_suffix: 0

- street_predirectional: 0

- street_premodifier: 0

- street_pretype: 0

- street_name: 1

- street_posttype: 0

- street_postdirectional: 0

- place_name: 0

- place_state: 0

- place_zipcode: 0

When fuzzy matching `street_name`, the "phonetic_street_key" prefilter
is automatically used (see
[`?fuzzy_match`](https://geomarker.io/addr/reference/fuzzy_match.md)).

## Examples

``` r
x_addr <- as_addr(c("123 Main St.", "333 Burnet Ave", "3333 Foofy Ave"))
y_addr <- as_addr(c("0000 Main Street", "3333 Burnet Avenue", "222 Burnet Ave"))

# no matches with defaults
addr_fuzzy_match(x_addr, y_addr)
#> [[1]]
#> integer(0)
#> 
#> [[2]]
#> integer(0)
#> 
#> [[3]]
#> integer(0)
#> 

# match on osa_max_dist of 2 for the address number
addr_fuzzy_match(x_addr, y_addr, addr_fields = c("number_digits" = 2))
#> [[1]]
#> integer(0)
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> integer(0)
#> 

# ignore address number when matching
addr_fuzzy_match(x_addr, y_addr, addr_fields = c("number_digits" = Inf))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2 3
#> 
#> [[3]]
#> integer(0)
#> 


fuzzy_match_addr_field(
  as_addr(c("123 Main St.", "3333 Burnet Ave", "3333 Foofy Ave")),
  as_addr(c("0000 Main Street", "0000 Burnet Avenue", "222 Burnet Ave")),
  addr_field = "street_name", osa_max_dist = 1
)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2 3
#> 
#> [[3]]
#> integer(0)
#> 

# empty address fields have an OSA distance of zero and always match
fuzzy_match_addr_field(
  as_addr(c("123 Main St.", "3333 Burnet Ave", "3333 Foofy Ave")),
  as_addr(c("0000 Main Street", "0000 Burnet Avenue", "222 Burnet Ave")),
  addr_field = "number_prefix"
)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 1 2 3
#> 
#> [[3]]
#> [1] 1 2 3
#> 
```
