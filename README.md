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

## Using

addr vectors behave like standard R vectors: they recycle, subset, and combine with vctrs tooling. You can parse text into an addr vector with `as_addr()` or build one from component vectors with `addr()`.

```r
addr_vec <- as_addr(c("3333 Burnet Ave Cincinnati OH 45229",
                      "5130 Rapid Run Rd Cincinnati OH 45238"))
```
