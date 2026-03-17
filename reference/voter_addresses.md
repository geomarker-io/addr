# Example addresses

The voter_addresses data was generated as an example character vector of
real-world addresses. These addresses were downloaded from the Hamilton
County, Ohio voter registration database on 2024-09-12. See
`inst/make_example_addresses.R` for more details.
`AddressPreDirectional`, `AddressNumber`, `AddressStreet`,
`AddressSuffix`, `CityName`, "OH", and `AddressZip` are pasted together
to create 242,133 unique addresses of registered voters in Hamilton
County, OH.

## Usage

``` r
voter_addresses()
```

## Value

a character vector

## Examples

``` r
voter_addresses() |>
  head()
#> [1] "3359 QUEEN CITY AVE CINCINNATI OH 45238"
#> [2] "1040 KREIS LN CINCINNATI OH 45205"      
#> [3] "9960 DALY RD CINCINNATI OH 45231"       
#> [4] "413 VOLKERT PL CINCINNATI OH 45219"     
#> [5] "8519 LINDERWOOD LN CINCINNATI OH 45255" 
#> [6] "6361 BEECHMONT AVE CINCINNATI OH 45230" 
```
