# National Address Database (NAD)

Download the NAD to the R user's data directory for the addr package and
read data into R by county and state.

## Usage

``` r
nad_read(county, state)

nad_download()
```

## Arguments

- county:

  character, length one; name of county

- state:

  character, length one; name of state

## Details

The U.S. Department of Transportation partners with address programs
from state, local, and tribal governments to compile their authoritative
data into a database. Find more information here:
<https://www.transportation.gov/gis/national-address-database> The NAD
is downloaded as the latest release from the transportation.gov data
portal:
<https://data.transportation.gov/dataset/National-Address-Database-NAD-File-Geodatabase/yw36-suxr/about_data>
For the original schema, see
<https://www.transportation.gov/sites/dot.gov/files/2023-07/NAD_Schema_202304.pdf>

The NAD does not distinguish between empty and missing address
components. When reading into R, all missing address components are
replaced with an empty string (`""`) *except* for address number
(digits), street name, and ZIP code.

## Examples

``` r
if (FALSE) { # \dontrun{
  nad_read("Hamilton", "OH")
} # }

# example data preloaded for Hamilton County, OH
nad_example_data()
#> # A tibble: 349,407 × 7
#>    nad_addr            subaddress uuid  date_update s2    address_type parcel_id
#>    <addr>              <chr>      <chr> <date>      <s2c> <chr>        <chr>    
#>  1 7610 THOMPSON Rd C… NA         {68A… 2025-03-30  8840… Unknown      NA       
#>  2 9483 ZOLA Ct HARRI… NA         {CBF… 2025-03-30  8840… Unknown      NA       
#>  3 208 MOHAWK St CINC… NA         {B18… 2025-03-30  8841… Unknown      NA       
#>  4 11800 READING Rd C… NA         {393… 2025-03-30  8840… Unknown      NA       
#>  5 12050 PRINCETON Pi… NA         {478… 2025-03-30  8840… Unknown      NA       
#>  6 6610 PFEIFFER Rd C… NA         {947… 2025-03-30  8840… Unknown      NA       
#>  7 6501 PFEIFFER Rd C… NA         {998… 2025-03-30  8840… Unknown      NA       
#>  8 4244 ROUND BOTTOM … NA         {5F5… 2025-03-30  8841… Unknown      NA       
#>  9 8210 MARKET PLACE … NA         {C01… 2025-03-30  8840… Unknown      NA       
#> 10 8212 MARKET PLACE … NA         {2A8… 2025-03-30  8840… Unknown      NA       
#> # ℹ 349,397 more rows
```
