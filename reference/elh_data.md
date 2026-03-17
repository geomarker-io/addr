# Example line-one addresses

The Cincinnati Evicition Hotspots data was downloaded from [Eviction
Labs](https://evictionlab.org/uploads/cincinnati_hotspots_media_report.csv)
and contains characteristics of the top 100 buildings that are
responsible for about 25% of all eviction filings in Cincinnati (from
their "current through 8-31-2024" release).

## Usage

``` r
elh_data()
```

## Value

a tibble with 100 rows and 9 columns

## Details

https://evictionlab.org/eviction-tracking/cincinnati-oh/

## Examples

``` r
elh_data()
#> # A tibble: 100 × 9
#>    position time_period end_date   xplaintiff xstreet_clean filings top100   lat
#>       <dbl> <date>      <date>     <chr>      <chr>           <dbl>  <dbl> <dbl>
#>  1        1 2023-09-01  2024-08-31 CINCINNAT… 2958 HIGHFOR…     265  0.247  39.2
#>  2        2 2023-09-01  2024-08-31 FOUR TOWE… 2705 E TOWER…     133  0.247  39.1
#>  3        3 2023-09-01  2024-08-31 NPRC PARK… 2310 WALDEN …      77  0.247  39.3
#>  4        4 2023-09-01  2024-08-31 LAFEUILLE… 2680 LAFEUIL…      67  0.247  39.1
#>  5        5 2023-09-01  2024-08-31 LUMA PROP… 1903 DREXEL …      57  0.247  39.3
#>  6        6 2023-09-01  2024-08-31 HOMES, PA… 1528 DUDLEY …      56  0.247  39.1
#>  7        7 2023-09-01  2024-08-31 UPTOWN CO… 400 W 9TH ST       55  0.247  39.1
#>  8        8 2023-09-01  2024-08-31 NPRC PARK… 2325 WALDEN …      54  0.247  39.3
#>  9        9 2023-09-01  2024-08-31 MALLARD L… 11923 CROSSI…      53  0.247  39.3
#> 10       10 2023-09-01  2024-08-31 NPRC APEX… 5317 EASTKNO…      52  0.247  39.2
#> # ℹ 90 more rows
#> # ℹ 1 more variable: lon <dbl>
```
