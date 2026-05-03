# addr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/addr)](https://CRAN.R-project.org/package=addr)
[![R-CMD-check](https://github.com/cole-brokamp/addr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/addr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![r-universe](https://geomarker-io.r-universe.dev/badges/addr)](https://geomarker-io.r-universe.dev/addr)

<!-- badges: end -->

Addresses that were not validated at collection are often inconsistently formatted and contain typographical or phonetic noise, making them difficult to compare or link to other address data.
The goal of addr is to clean, parse, standardize, match, and geocode messy, real-world US addresses in R.
addr uses the included `usaddress` library to tag address components and build vctrs-based address vectors, including `addr()` vectors and the `addr_number()`, `addr_street()`, and `addr_place()` component vectors.
These vectors can be standardized, matched, joined, and used as data-frame columns, allowing standard R tools to work with nested address structures.

## Installation

Install the latest stable release of addr from [R-universe](https://r-universe.dev) with:

```r
install.packages("addr", repos = c("https://geomarker-io.r-universe.dev", "https://cloud.r-project.org"))
```

Or, install the development version of addr from [GitHub](https://github.com/) with:

```r
# install.packages("pak")
pak::pak("cole-brokamp/addr")
```

Installing addr from GitHub requires a working [Rust](https://www.rust-lang.org/learn/get-started) toolchain; install one using [rustup](https://www.rust-lang.org/tools/install).

## Getting Started

addr vectors behave like standard R vectors: they recycle, subset, and combine with vctrs tooling. You can parse text into an addr vector with `as_addr()` or build one from component vectors with `addr()`.

```r
addr_vec <- as_addr(c("3333 Burnet Ave Cincinnati OH 45229",
                      "5130 Rapid Run Rd Cincinnati OH 45238"))
```

## Address Matching

`addr_match()` compares one `addr()` vector to another and returns one selected reference address for each input address. Matching is staged: ZIP codes are matched first, then streets are matched within each matched ZIP code, then address numbers are matched within each matched ZIP/street group. This keeps matching fast while still allowing common street-name, phonetic, ZIP-code, and address-number variation.

Use `addr_left_join()` when the goal is to join data frames with addr columns. It uses the same staged matching as `addr_match()` and then expands exact duplicate reference rows when more than one row in `y` has the selected address. Use `addr_fuzzy_left_join()` when you need all fuzzy candidate matches rather than one selected match.

For repeated matching against the same reference addresses, prepare the reference once with `addr_match_prepare()` and reuse the returned index in later `addr_match()` or `addr_left_join()` calls.

## National Address Database

`nad()` reads county-level address points from the U.S. Department of Transportation National Address Database. Counties can be requested by county name plus state, such as `"Hamilton", "OH"`, or by 5-digit county FIPS code, such as `"39061"`.

The nationwide NAD geodatabase is large, so addr caches derived county data in the R user cache directory. The package also includes `nad_example_data()` for Hamilton County, Ohio, which is useful for examples, tests, and matching workflows that should run without downloading the full NAD source first.

## Geocoding

`geocode()` converts `addr()` vectors to point locations using Census TIGER address ranges. It matches the input street to installed TIGER address features, chooses the best address range and street side from the address number, interpolates a point along the range, and offsets that point from the street line.

Geocoding returns the input address, matched ZIP code, matched street, point geography, and s2 cell. Inputs with missing or unmatched ZIP codes, streets, or address ranges return missing geographies rather than guessed coordinates.

## TIGER Address Features

TIGER address features are Census street-segment address ranges. addr stores them as a hive-partitioned, multi-file parquet dataset, grouped by ZIP-code partitions and county files, so geocoding can read only the local files needed for the input ZIP codes.

`taf_install()` installs TIGER address features for one county. `taf_needed_counties()` identifies which county files may contain the ZIP codes in an input address vector, including selected ZIP-code variants. `taf_ensure()` installs any missing county files, and `geocode()` calls it by default before geocoding. Use `taf()` to open the installed multi-file dataset with arrow and `taf_zip()` to read the address ranges for specific ZIP codes.
