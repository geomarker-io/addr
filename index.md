# addr

Addresses that were not validated at the time of collection are often
heterogenously formatted, making them difficult to directly compare. The
goal of addr is to clean, parse, standardize, and match messy,
real-world addresses in R to use for data linkages. addr is built around
vctrs-based addr vectors, including the
[`addr()`](https://geomarker.io/addr/reference/addr.md) vector and the
[`addr_number()`](https://geomarker.io/addr/reference/addr.md),
[`addr_street()`](https://geomarker.io/addr/reference/addr.md), and
[`addr_place()`](https://geomarker.io/addr/reference/addr.md) subclass
vectors.

## Installation

Install the latest stable release of addr from
[R-universe](https://r-universe.dev) with:

``` r
install.packages("addr", repos = c("https://geomarker-io.r-universe.dev", "https://cloud.r-project.org"))
```

Or, install the development version of addr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("cole-brokamp/addr")
```

Installing addr from GitHub requires a working
[Rust](https://www.rust-lang.org/learn/get-started) toolchain; install
one using [rustup](https://www.rust-lang.org/tools/install).

## Using

addr vectors behave like standard R vectors: they recycle, subset, and
combine with vctrs tooling. You can parse text into an addr vector with
[`as_addr()`](https://geomarker.io/addr/reference/as_addr.md) or build
one from its subclass vectors with
[`addr()`](https://geomarker.io/addr/reference/addr.md).

``` r
addr_vec <- as_addr(c("3333 Burnet Ave Cincinnati OH 45229",
                      "5130 Rapid Run Rd Cincinnati OH 45238"))
```
