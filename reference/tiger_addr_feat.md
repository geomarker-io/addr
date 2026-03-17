# Get s2_geography for tiger street ranges

TIGER address features (i.e. street address ranges) are downloaded from
the census.gov FTP site and converted to a "long" format by street side
L/R, which is used with `tiger_geocode()`.

## Usage

``` r
tiger_addr_feat(county, year)
```

## Arguments

- county:

  character string of county identifier

- year:

  character year of tigris product

## Value

a list of tibbles, one for each street name, with `TLID`,
`s2_geography`, `from`, and `to` columns

## Examples

``` r
tiger_addr_feat("39061", "2024")
#> Simple feature collection with 55922 features and 8 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -84.8203 ymin: 39.02539 xmax: -84.25951 ymax: 39.31196
#> Geodetic CRS:  NAD83
#> # A tibble: 55,922 × 9
#>    LINEARID   FULLNAME                  geometry side    ZIP FROMHN  TOHN PARITY
#>  * <chr>      <chr>             <LINESTRING [°]> <chr> <int>  <int> <int> <chr> 
#>  1 110386722… S State… (-84.81996 39.25987, -84… L     45030    300   318 E     
#>  2 110386722… S State… (-84.8199 39.26203, -84.… L     45030    100   108 E     
#>  3 110386722… S State… (-84.81994 39.26095, -84… L     45030    200   298 E     
#>  4 110386722… S State… (-84.81995 39.2593, -84.… L     45030    320   398 E     
#>  5 110386722… S State… (-84.81999 39.25823, -84… L     45030    400   446 E     
#>  6 110386722… S State… (-84.82 39.25739, -84.81… L     45030    448   498 E     
#>  7 110386722… S State… (-84.81998 39.25685, -84… L     45030    500   998 E     
#>  8 110386722… S State… (-84.82 39.25464, -84.82… L     45030   1000  1098 E     
#>  9 110386722… S State… (-84.8201 39.2516, -84.8… L     45030  26348 26398 E     
#> 10 110386722… S State… (-84.81992 39.26151, -84… L     45030    110   198 E     
#> # ℹ 55,912 more rows
#> # ℹ 1 more variable: OFFSET <chr>
```
