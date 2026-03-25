# Classify addr match stage

Classify an addr vector into the staged outcomes returned by
[`addr_match()`](https://geomarker.io/addr/reference/addr_match.md): no
match, ZIP-only match, ZIP-plus-street match, or
ZIP-plus-street-plus-number match.

## Usage

``` r
addr_match_stage(x, strict = TRUE)
```

## Arguments

- x:

  addr vector to classify

- strict:

  logical; require `x` to follow the partial-result structure produced
  by
  [`addr_match()`](https://geomarker.io/addr/reference/addr_match.md)?
  If `FALSE`, classification is based only on the deepest non-missing
  core component (`@place@zipcode`, `@street@name`, `@number@digits`).

## Value

an ordered factor with levels `none`, `zip`, `street`, `number`

## Examples

``` r
y <- as_addr(c(
  "10 MAIN ST CINCINNATI OH 45220",
  "11 MAIN ST CINCINNATI OH 45220",
  "10 MAIN ST CINCINNATI OH 45229"
))
x <- as_addr(c(
  "99 MAIN ST CINCINNATI OH 45220",
  "10 OAK ST CINCINNATI OH 45220",
  "10 MAIN ST CINCINNATI OH 45103"
))

out <- addr_match(x, y)
addr_match_stage(out)
#> [1] street zip    none  
#> Levels: none < zip < street < number
```
