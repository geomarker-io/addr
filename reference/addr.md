# addr classes

The structures for `addr()` and the `addr_` classes are derived as a
subset of the United States Thoroughfare, Landmark, and Postal Address
Data Standard that is relevant for residential, numbered thoroughfare
addresses:

      Address
       ├─ AddressNumber
       │  ├─ AddressNumberPrefix
       │  ├─ AddressNumber
       │  ├─ AddressNumberSuffix
       ├─ StreetName
       │  ├─ StreetNamePreModifier
       │  ├─ StreetNamePreDirectional
       │  ├─ StreetNamePreType
       │  ├─ StreetName
       │  ├─ StreetNamePostType
       │  └─ StreetNamePostDirectional
       └─ Place
          ├─ PlaceName
          ├─ StateName
          └─ ZipCode

`addr()` combines `addr_number()`, `addr_street()`, and `addr_place()`
into a single addr vector:

    <addr>
     @ number: <addr_number>
     .. @ prefix
     .. @ digits
     .. @ suffix
     @ street: <addr_street>
     .. @ predirectional
     .. @ premodifier
     .. @ pretype
     .. @ name
     .. @ posttype
     .. @ postdirectional
     @ place : <addr_place>
     .. @ name
     .. @ state
     .. @ zipcode

## Usage

``` r
addr_number(
  prefix = NA_character_,
  digits = NA_character_,
  suffix = NA_character_
)

addr_street(
  predirectional = NA_character_,
  premodifier = NA_character_,
  pretype = NA_character_,
  name = NA_character_,
  posttype = NA_character_,
  postdirectional = NA_character_,
  map_posttype = TRUE,
  map_directional = TRUE,
  map_pretype = TRUE,
  map_ordinal = TRUE
)

addr_place(
  name = NA_character_,
  state = NA_character_,
  zipcode = NA_character_,
  map_state = TRUE
)

addr(number = addr_number(), street = addr_street(), place = addr_place())
```

## Arguments

- prefix:

  (often fractional) appears before digits

- digits:

  primary street number for the address

- suffix:

  (often letter/part) attached after digits

- predirectional:

  direction before name

- premodifier:

  descriptive modifier before name

- pretype:

  type/classification before name

- name:

  street name or city/town/municipality name

- posttype:

  type/classification after name

- postdirectional:

  direction after name

- map_posttype:

  logical; map posttype to abbreviations?

- map_directional:

  logical; map pre- and post-directional to abbreviations?

- map_pretype:

  logical; map pretype to abbreviations?

- map_ordinal:

  logical; map ordinal street names to abbreviations?

- state:

  state or territory abbreviation

- zipcode:

  ZIP code (must be five digits not starting with "000")

- map_state:

  logical; map state to abbreviations?

- number:

  an addr_number vector

- street:

  an addr_street vector

- place:

  an addr_place vector

## Value

An addr, addr_number, addr_street, or addr_place vector

## Details

All field values must must be a character vector of at least length one
(including missing values); length one fields will be recycled to match
the length of other fields.

## Examples

``` r
# define a new addr_number vector
addr_number(digits = "290")
#> <addr_number> function ()  
#>  @ prefix: chr NA
#>  @ digits: chr "290"
#>  @ suffix: chr NA
addr_number(prefix = "N", digits = "290", suffix = "A")
#> <addr_number> function ()  
#>  @ prefix: chr "N"
#>  @ digits: chr "290"
#>  @ suffix: chr "A"

# define a new addr_street vector
addr_street(name = "Burnet", posttype = "Ave")
#> <addr_street> function ()  
#>  @ predirectional : chr NA
#>  @ premodifier    : chr NA
#>  @ pretype        : chr NA
#>  @ name           : chr "Burnet"
#>  @ posttype       : chr "Ave"
#>  @ postdirectional: chr NA

# street names are automatically mapped to abbreviations
addr_street(predirectional = "North", name = "Fifth", posttype = "Street")
#> <addr_street> function ()  
#>  @ predirectional : chr "N"
#>  @ premodifier    : chr NA
#>  @ pretype        : chr NA
#>  @ name           : chr "5TH"
#>  @ posttype       : chr "St"
#>  @ postdirectional: chr NA

# define a new addr_place vector
addr_place(name = "Cincinnati", state = "OH", zipcode = "45220")
#> <addr_place> function ()  
#>  @ name   : chr "Cincinnati"
#>  @ state  : chr "OH"
#>  @ zipcode: chr "45220"

# define a new addr vector
addr(
  addr_number(digits = "290"),
  addr_street(name = "Burnet", posttype = "Ave"),
  addr_place(name = "Cincinnati", state = "OH", zipcode = "45229")
)
#> <addr>
#>  @ number: <addr_number> function ()  
#>  .. @ prefix: chr NA
#>  .. @ digits: chr "290"
#>  .. @ suffix: chr NA
#>  @ street: <addr_street> function ()  
#>  .. @ predirectional : chr NA
#>  .. @ premodifier    : chr NA
#>  .. @ pretype        : chr NA
#>  .. @ name           : chr "Burnet"
#>  .. @ posttype       : chr "Ave"
#>  .. @ postdirectional: chr NA
#>  @ place : <addr_place> function ()  
#>  .. @ name   : chr "Cincinnati"
#>  .. @ state  : chr "OH"
#>  .. @ zipcode: chr "45229"

# define a more complicated addr vector
# and explicltly specify empty components to avoid NA
addr(
  addr_number(prefix = "", digits = "200", suffix = ""),
  addr_street(
    predirectional = "west",
    premodifier = "Old",
    pretype = "US",
    name = "50",
    posttype = "avenue",
    postdirectional = "east",
    map_directional = TRUE,
    map_pretype = TRUE,
    map_posttype = TRUE
  ),
  addr_place(name = "Cincinnati", state = "ohio", zipcode = "45220")
)
#> <addr>
#>  @ number: <addr_number> function ()  
#>  .. @ prefix: chr ""
#>  .. @ digits: chr "200"
#>  .. @ suffix: chr ""
#>  @ street: <addr_street> function ()  
#>  .. @ predirectional : chr "W"
#>  .. @ premodifier    : chr "Old"
#>  .. @ pretype        : chr "US Hwy"
#>  .. @ name           : chr "50"
#>  .. @ posttype       : chr "Ave"
#>  .. @ postdirectional: chr "E"
#>  @ place : <addr_place> function ()  
#>  .. @ name   : chr "Cincinnati"
#>  .. @ state  : chr "OH"
#>  .. @ zipcode: chr "45220"

# addr_* vectors are recycled and omitted fields are missing
addr(
  addr_number(digits = c("290", "200", "3333", "111")),
  addr_street(
    name = c("Burnet", "Main", "Ludlow", "State Route 32"),
    posttype = c("Ave", "St", "Ave", NA_character_)
  ),
  addr_place(name = "Cincinnati", state = "OH")
)
#> <addr>
#>  @ number: <addr_number> function ()  
#>  .. @ prefix: chr [1:4] NA NA NA NA
#>  .. @ digits: chr [1:4] "290" "200" "3333" "111"
#>  .. @ suffix: chr [1:4] NA NA NA NA
#>  @ street: <addr_street> function ()  
#>  .. @ predirectional : chr [1:4] NA NA NA NA
#>  .. @ premodifier    : chr [1:4] NA NA NA NA
#>  .. @ pretype        : chr [1:4] NA NA NA NA
#>  .. @ name           : chr [1:4] "Burnet" "Main" "Ludlow" "State Route 32"
#>  .. @ posttype       : chr [1:4] "Ave" "St" "Ave" NA
#>  .. @ postdirectional: chr [1:4] NA NA NA NA
#>  @ place : <addr_place> function ()  
#>  .. @ name   : chr [1:4] "Cincinnati" "Cincinnati" "Cincinnati" "Cincinnati"
#>  .. @ state  : chr [1:4] "OH" "OH" "OH" "OH"
#>  .. @ zipcode: chr [1:4] NA NA NA NA
```
