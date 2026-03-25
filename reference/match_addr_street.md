# Match addr_street vectors

A single addr_street in y is chosen for each addr_street in x. If exact
matches (using `as.character`) are not found, possible matches are
chosen by fuzzy matching on street name (using phonetic street key and
street name) and exact matching on street type and predirectional.
Ordinal street names use restricted phonetic candidates: an ordinal
phonetic key like `#0007` may fuzzy match only to plausible ordinal
neighbors such as digit shifts (`#0070`, `#0700`, `#7000`) or same-width
substitutions (`#0008`, `#0009`), not arbitrary OSA-distance-one ordinal
keys such as `#0017` or `#0077`. Ties are broken by .......... the first
for now.

## Usage

``` r
match_addr_street(x, y)
```

## Arguments

- x, y:

  addr_street vectors to match

## Value

an addr_street vector, the same length as x, that is the best match in y
for each addr_street code in x; if no best match is found a missing
value is returned
([`addr_street()`](https://geomarker.io/addr/reference/addr.md))

## Details

addr_street objects within missing or empty @name are not matched and
returned as missing instead.

## Examples

``` r
my_streets <- addr_street(
   predirectional = "",
   premodifier = "",
   pretype = "",
   name = c("Beechview", "Vivian", "Springfield", "Round Bottom", "Pfeiffer", "Beachview",
            "Vevan", "Srpingfield", "Square Top", "Pfeffer", "Wuhlper", ""),
  posttype = c("Cir", "Pl", "Pike", "Rd", "Rd", "Cir", "Pl", "Pike", "Rd", "Rd", "Ave", ""),
  postdirectional = ""
 )
the_streets <- nad_example_data()$nad_addr@street
match_addr_street(my_streets, the_streets)
#> <addr_street> function ()  
#>  @ predirectional : chr [1:12] "" "" "" "" "" "" "" "" NA "" "" NA
#>  @ premodifier    : chr [1:12] "" "" "" "" "" "" "" "" NA "" "" NA
#>  @ pretype        : chr [1:12] "" "" "" "" "" "" "" "" NA "" "" NA
#>  @ name           : chr [1:12] "BEECHVIEW" "VIVIAN" "SPRINGFIELD" "ROUND BOTTOM" ...
#>  @ posttype       : chr [1:12] "Cir" "Pl" "Pike" "Rd" "Rd" "Cir" "Pl" "Pike" NA "Rd" ...
#>  @ postdirectional: chr [1:12] "" "" "" "" "" "" "" "" NA "" "" NA
```
