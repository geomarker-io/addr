# Coerce to addr

`as_addr()` converts other objects into
[`addr()`](https://geomarker.io/addr/reference/addr.md) vectors. See
[`?addr`](https://geomarker.io/addr/reference/addr.md) for more details
on its structure.

## Usage

``` r
as_addr(x, ...)
```

## Arguments

- x:

  object to coerce to an addr vector

- ...:

  additional arguments passed to methods

## Methods implemented for

- `character`: will be cleaned (if `clean = TRUE`) with
  [`clean_address_text()`](https://geomarker.io/addr/reference/clean_address_text.md)
  and then tagged using
  [`usaddress_tag()`](https://geomarker.io/addr/reference/usaddress_tag.md);
  tags are normalized to abbreviations by passing all `map_*` arguments
  to [`addr_street()`](https://geomarker.io/addr/reference/addr.md) or
  [`addr_place()`](https://geomarker.io/addr/reference/addr.md); ZIP
  codes parsed with more than five characters are truncated with a
  warning; non-numeric characters in parsed address number digits will
  be removed with a warning

- `data.frame`: must have columns named according to fields in
  [`addr_number()`](https://geomarker.io/addr/reference/addr.md),
  [`addr_street()`](https://geomarker.io/addr/reference/addr.md), or
  [`addr_place()`](https://geomarker.io/addr/reference/addr.md); also
  passes the `map_*` arguments to
  [`addr_street()`](https://geomarker.io/addr/reference/addr.md) and
  [`addr_place()`](https://geomarker.io/addr/reference/addr.md)

- `addr`: returned as-is

## Examples

``` r
as_addr(voter_addresses()[1:1000])
#> Warning: street name post type parsed but not mapped: la
#> <addr>
#>  @ number: <addr_number> function ()  
#>  .. @ prefix: chr [1:1000] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ...
#>  .. @ digits: chr [1:1000] "3359" "1040" "9960" "413" "8519" "6361" "10466" ...
#>  .. @ suffix: chr [1:1000] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ...
#>  @ street: <addr_street> function ()  
#>  .. @ predirectional : chr [1:1000] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ...
#>  .. @ premodifier    : chr [1:1000] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ...
#>  .. @ pretype        : chr [1:1000] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ...
#>  .. @ name           : chr [1:1000] "QUEEN CITY" "KREIS" "DALY" "VOLKERT" "LINDERWOOD" ...
#>  .. @ posttype       : chr [1:1000] "Ave" "Ln" "Rd" "Pl" "Ln" "Ave" "Ln" "Cir" "Ave" ...
#>  .. @ postdirectional: chr [1:1000] "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ...
#>  @ place : <addr_place> function ()  
#>  .. @ name   : chr [1:1000] "CINCINNATI" "CINCINNATI" "CINCINNATI" "CINCINNATI" ...
#>  .. @ state  : chr [1:1000] "OH" "OH" "OH" "OH" "OH" "OH" "OH" "OH" "OH" "OH" ...
#>  .. @ zipcode: chr [1:1000] "45238" "45205" "45231" "45219" "45255" "45230" ...

data.frame(
  number_digits = c("290", "200"),
  street_name = c("Burnet", "Main"),
  street_posttype = c("Ave", "St"),
  place_name = c("Cincinnati", "Cincinnati"),
  place_state = c("OH", "OH"),
  place_zipcode = c("45229", "45220"),
  stringsAsFactors = FALSE
)|>
  as_addr()
#> <addr>
#>  @ number: <addr_number> function ()  
#>  .. @ prefix: chr [1:2] "" ""
#>  .. @ digits: chr [1:2] "290" "200"
#>  .. @ suffix: chr [1:2] "" ""
#>  @ street: <addr_street> function ()  
#>  .. @ predirectional : chr [1:2] "" ""
#>  .. @ premodifier    : chr [1:2] "" ""
#>  .. @ pretype        : chr [1:2] "" ""
#>  .. @ name           : chr [1:2] "Burnet" "Main"
#>  .. @ posttype       : chr [1:2] "Ave" "St"
#>  .. @ postdirectional: chr [1:2] "" ""
#>  @ place : <addr_place> function ()  
#>  .. @ name   : chr [1:2] "Cincinnati" "Cincinnati"
#>  .. @ state  : chr [1:2] "OH" "OH"
#>  .. @ zipcode: chr [1:2] "45229" "45220"
```
