# Convert street names into phonetic matching keys

Ordinal street names (e.g., "11TH", "5TH") are encoded as zero-padded
numeric identifiers with a special prefix, while non-ordinal street
names are encoded using a Soundex phonetic code (see
[`?stringdist::phonetic`](https://rdrr.io/pkg/stringdist/man/phonetic.html)).
Ordinal words (e.g., "Eleventh", "Fifth") are detected and converted
automatically. Each phonetic key is exactly four characters long.

## Usage

``` r
phonetic_street_key(x)
```

## Arguments

- x:

  character vector

## Value

character vector

## Examples

``` r
phonetic_street_key(
  c("MEADOWLARK", "TOWNSEND", "IMMACULATE", "7TH", "WERK",
    "PAXTON", "5th", "BURNET", "FIFTH", "CLIFTON")
)
#>  [1] "M346"  "T525"  "I524"  "#0007" "W620"  "P235"  "#0005" "B653"  "#0005"
#> [10] "C413" 
```
