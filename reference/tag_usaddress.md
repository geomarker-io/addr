# Tag US addresses

Addresses are tagged using the usaddress conditional random field in a
[rust port](https://github.com/boydjohnson/usaddress-rs) of
[usaddress](https://github.com/datamade/usaddress). Possible address
labels include:

- `AddressNumberPrefix`

- `AddressNumberSuffix`

- `AddressNumber`

- `BuildingName`

- `CornerOf`

- `IntersectionSeparator`

- `LandmarkName`

- `NotAddress`

- `OccupancyIdentifier`

- `OccupancyType`

- `PlaceName`

- `Recipient`

- `StateName`

- `StreetNamePostDirectional`

- `StreetNamePostType`

- `StreetNamePreDirectional`

- `StreetNamePreModifier`

- `StreetNamePreType`

- `StreetName`

- `SubaddressIdentifier`

- `SubaddressType`

- `USPSBoxGroupID`

- `USPSBoxGroupType`

- `USPSBoxID`

- `USPSBoxType`

- `ZipCode`

Find more information about the definitions at
<https://www.fgdc.gov/standards/projects/address-data>

## Usage

``` r
tag_usaddress(x = NA_character_, clean = TRUE)
```

## Arguments

- x:

  character string of addresses

- clean:

  logical; clean address text with clean_address_text() before tagging?

## Value

a list of vectors of named address tags

## Examples

``` r
tag_usaddress(
  c("290 Ludlow Avenue Apt 2 Cincinnati OH 45220",
  "3333 Burnet Ave Cincinnati Ohio 45219",
  "120 North Main Street, Greenville, SC 29601",
  "200 Southwest North Street, Topeka, KS 66603",
  "215 Highway 88 Road, Jackson, CA 95642"
  )
)
#> [[1]]
#>       AddressNumber          StreetName  StreetNamePostType       OccupancyType 
#>               "290"            "Ludlow"            "Avenue"               "Apt" 
#> OccupancyIdentifier           PlaceName           StateName             ZipCode 
#>                 "2"        "Cincinnati"                "OH"             "45220" 
#> 
#> [[2]]
#>      AddressNumber         StreetName StreetNamePostType          PlaceName 
#>             "3333"           "Burnet"              "Ave"       "Cincinnati" 
#>          StateName            ZipCode 
#>             "Ohio"            "45219" 
#> 
#> [[3]]
#>            AddressNumber StreetNamePreDirectional               StreetName 
#>                    "120"                  "North"                   "Main" 
#>       StreetNamePostType                PlaceName                StateName 
#>                 "Street"             "Greenville"                     "SC" 
#>                  ZipCode 
#>                  "29601" 
#> 
#> [[4]]
#>            AddressNumber StreetNamePreDirectional               StreetName 
#>                    "200"              "Southwest"                  "North" 
#>       StreetNamePostType                PlaceName                StateName 
#>                 "Street"                 "Topeka"                     "KS" 
#>                  ZipCode 
#>                  "66603" 
#> 
#> [[5]]
#>      AddressNumber  StreetNamePreType         StreetName StreetNamePostType 
#>              "215"          "Highway"               "88"             "Road" 
#>          PlaceName          StateName            ZipCode 
#>          "Jackson"               "CA"            "95642" 
#> 

# edge cases!
tag_usaddress(
  c(
    "1600 Pennsylvania Avenue NW, Washington, DC 20500", # post-directional quadrant
    "1 Infinite Loop, Cupertino, CA 95014", # corporate campus street name
    "210 East 400 South, Salt Lake City, UT 84111", # grid addressing (Utah)
    "N6W23001 Bluemound Road, Wauwatosa, WI 53226", # address number prefix grid (Wisconsin)
    "350 Fifth Avenue, New York, NY 10118", # ordinal street name
    "4059 Mt Lee Drive, Hollywood, CA 90068", # abbreviated street element
    "233 South Wacker Drive, Chicago, IL 60606", # pre-directional
    "700 Exposition Park Drive, Los Angeles, CA 90037", # multi-word street name
    "2 South Biscayne Boulevard, Miami, FL 33131" # directional + boulevard
  )
)
#> [[1]]
#>             AddressNumber                StreetName        StreetNamePostType 
#>                    "1600"            "Pennsylvania"                  "Avenue" 
#> StreetNamePostDirectional                 PlaceName                 StateName 
#>                      "NW"              "Washington"                      "DC" 
#>                   ZipCode 
#>                   "20500" 
#> 
#> [[2]]
#>      AddressNumber         StreetName StreetNamePostType          PlaceName 
#>                "1"         "Infinite"             "Loop"        "Cupertino" 
#>          StateName            ZipCode 
#>               "CA"            "95014" 
#> 
#> [[3]]
#>             AddressNumber  StreetNamePreDirectional                StreetName 
#>                     "210"                    "East"                     "400" 
#> StreetNamePostDirectional                 PlaceName                 PlaceName 
#>                   "South"                    "Salt"                    "Lake" 
#>                 PlaceName                 StateName                   ZipCode 
#>                    "City"                      "UT"                   "84111" 
#> 
#> [[4]]
#>      AddressNumber         StreetName StreetNamePostType          PlaceName 
#>         "N6W23001"        "Bluemound"             "Road"        "Wauwatosa" 
#>          StateName            ZipCode 
#>               "WI"            "53226" 
#> 
#> [[5]]
#>      AddressNumber         StreetName StreetNamePostType          PlaceName 
#>              "350"            "Fifth"           "Avenue"              "New" 
#>          PlaceName          StateName            ZipCode 
#>             "York"               "NY"            "10118" 
#> 
#> [[6]]
#>      AddressNumber         StreetName         StreetName StreetNamePostType 
#>             "4059"               "Mt"              "Lee"            "Drive" 
#>          PlaceName          StateName            ZipCode 
#>        "Hollywood"               "CA"            "90068" 
#> 
#> [[7]]
#>            AddressNumber StreetNamePreDirectional               StreetName 
#>                    "233"                  "South"                 "Wacker" 
#>       StreetNamePostType                PlaceName                StateName 
#>                  "Drive"                "Chicago"                     "IL" 
#>                  ZipCode 
#>                  "60606" 
#> 
#> [[8]]
#>      AddressNumber         StreetName         StreetName StreetNamePostType 
#>              "700"       "Exposition"             "Park"            "Drive" 
#>          PlaceName          PlaceName          StateName            ZipCode 
#>              "Los"          "Angeles"               "CA"            "90037" 
#> 
#> [[9]]
#>            AddressNumber StreetNamePreDirectional               StreetName 
#>                      "2"                  "South"               "Biscayne" 
#>       StreetNamePostType                PlaceName                StateName 
#>              "Boulevard"                  "Miami"                     "FL" 
#>                  ZipCode 
#>                  "33131" 
#> 
```
