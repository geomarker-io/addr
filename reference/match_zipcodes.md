# Match ZIP codes

A single ZIP code in y is chosen for each ZIP code in x. By default, if
exact matches are not found, common variants of ZIP codes in x are
searched for in y. If multiple variants are present in y, the best one
is selected based on the lowest absolute numeric difference with the ZIP
code in x; ties are broken by OSA string distances and then preferring
the minimum number.

## Usage

``` r
match_zipcodes(x, y, zip_variants = TRUE)
```

## Arguments

- x, y:

  character vectors of ZIP codes to match

- zip_variants:

  logical; fuzzy match to common variants of x in y? (e.g., changing 4th
  or 5th digit)

## Value

a character vector, the same length as x, that is the best match in y
for each ZIP code in x

## Examples

``` r
match_zipcodes(
  c("45222", "45219", "45219", "45220", "45220", "", NA),
  c("42522", "45200", "45219", "45221", "45223", "45321", "")
)
#> [1] "45221" "45219" "45219" "45221" "45221" NA      NA     

match_zipcodes(
  c("45222", "45219", "45219", "45220", "45220", "", NA),
  c("42522", "45200", "45219", "45221", "45223", "45321", ""),
  zip_variants = FALSE
)
#> [1] NA      "45219" "45219" NA      NA      NA      NA     
```
