# Example National Address Database addresses

An example of the data returned using
[`nad_read()`](https://geomarker.io/addr/reference/nad_read.md) for
Hamilton County, Ohio. See
[`?nad_read`](https://geomarker.io/addr/reference/nad_read.md) for more
information about the National Address Database.

## Usage

``` r
nad_example_data()
```

## Value

a tibble with 349,407 rows and 7 columns

## Examples

``` r
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
