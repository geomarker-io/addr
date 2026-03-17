# clean address text

remove excess whitespace; keep only letters, numbers, `#`, and `-`

## Usage

``` r
clean_address_text(x)
```

## Arguments

- x:

  a vector of address character strings

## Value

a vector of cleaned addresses

## Examples

``` r
clean_address_text(c(
  "3333 Burnet Ave Cincinnati OH 45219",
  "33_33 Burnet Ave. Cincinnati OH 45219",
  "33\\33 B\"urnet Ave; Ci!ncinn&*ati OH 45219",
  "3333 Burnet Ave Cincinnati OH 45219",
  "33_33 Burnet Ave. Cincinnati OH 45219"
))
#> [1] "3333 Burnet Ave Cincinnati OH 45219" "3333 Burnet Ave Cincinnati OH 45219"
#> [3] "3333 Burnet Ave Cincinnati OH 45219" "3333 Burnet Ave Cincinnati OH 45219"
#> [5] "3333 Burnet Ave Cincinnati OH 45219"
```
