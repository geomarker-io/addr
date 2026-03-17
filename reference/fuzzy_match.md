# Fuzzy match

Fuzzy match strings in x to y using optimized string alignment (ignoring
capitalization).

## Usage

``` r
fuzzy_match(x, y, osa_max_dist = 1, prefilter = c("none", "psk"))
```

## Arguments

- x:

  character vector to match

- y:

  character vector to match to

- osa_max_dist:

  maximum OSA distance to consider a match; `Inf` is a special case that
  avoids computing string distance by returning all of `y` instead of
  just the best match(es) in 'y\`

- prefilter:

  method used to prefilter y before computing osa distances for fuzzy
  matching to speed up calculations; "none" does nothing, "psk" removes
  addrs in y that do not have a
  [`phonetic_street_key()`](https://geomarker.io/addr/reference/phonetic_street_key.md)
  in x

## Value

a list of integer vectors representing the position of the best matching
string(s) in `y` for each string in `x`

## Details

If multiple strings in `y` are tied for the minimum osa distances with a
string in `x`, then all of their indices are included in the return
value.

## Examples

``` r
my_names <-
  c("Pinye", "Pine", "Oalck", "Sunset", "Riverbend", "Greenfild")
the_names <-
  c("Piney", "Pine", "Oak", "Cheshire", "Greenfield", "Maple", "Elm")
matches <- fuzzy_match(my_names, the_names, osa_max_dist = 1)
matches
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> integer(0)
#> 
#> [[4]]
#> integer(0)
#> 
#> [[5]]
#> integer(0)
#> 
#> [[6]]
#> [1] 5
#> 

lapply(matches, \(i) the_names[i])
#> [[1]]
#> [1] "Piney" "Pine" 
#> 
#> [[2]]
#> [1] "Pine"
#> 
#> [[3]]
#> character(0)
#> 
#> [[4]]
#> character(0)
#> 
#> [[5]]
#> character(0)
#> 
#> [[6]]
#> [1] "Greenfield"
#> 

x <- as_addr(voter_addresses()[1:100])@street@name
y <- unique(nad_example_data()$nad_addr@street@name)
system.time(fuzzy_match(x, y))
#>    user  system elapsed 
#>   0.602   0.005   0.206 
# larger vectors see a speedup when using
# phonetic_street_key as a prefilter
# but may miss potential matches that are within
# osa_max_dist of each other, but did not have
# identical phonetic codes (e.g., "woolper" and "woopler")
system.time(fuzzy_match(x, y, prefilter = "psk"))
#>    user  system elapsed 
#>   0.379   0.006   0.167 
```
