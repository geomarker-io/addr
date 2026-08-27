# addr (development version)

- Breaking: `addr()`, `addr_number()`, `addr_street()`, `addr_place()`, and `as_addr()` now store every address component in uppercase after applying existing mappings. Mapping flags control abbreviation mapping but no longer preserve input case. `as_addr()` upgrades legacy mixed-case addr objects. Prepared `addr_match_index` objects created by an older version must be rebuilt with `addr_match_prepare()`.
- `geocode()` and `geocode_zip()` now consider Census place- and county-subdivision-derived ZCTAs by default after the exact input ZIP and before typographical ZIP variants. The new `place_zip_variants` and `place_zip_variant` arguments independently control this search. Valid address ranges are preferred across all candidate tiers, and TIGER county files for every enabled candidate are prepared before geocoding.
- Breaking: `taf_zip()` is now `taf()`, making `taf()` the materialized TAF reader for one or more ZIP codes alongside the county-oriented `nad()`. The former lazy Arrow `taf()` interface is now `taf_dataset()`, parallel to `nad_dataset()`. No compatibility aliases are retained.
- Breaking: `taf_needed_counties()` and `taf_ensure()` now consider place- and county-subdivision-derived ZCTAs as well as typographical ZIP variants for `addr` inputs. The new public `taf_manifest()` inventories one installed county-ZIP Parquet file per row and can validate storage schemas, row counts, sizes, and SHA-256 digests. TAF data, manifests, catalogs, and schema-v2 fuel bundles use the `v2` layout and validate and refresh the manifest across distribution and installed Parquet compression. The former six-column schema-v1 TAF manifest is not migrated; remove both old year-specific TAF directories before rebuilding counties with `taf_install()` or installing a matching schema-v2 fuel bundle.
- Breaking: `nad()`, `nad_install()`, and `nad_download()` now support only revision 23. stow installs the pinned USDOT compressed flat-file archive as a durable managed local copy under `stow/nad`; a native streaming reader filters one requested county without unpacking the roughly 41 GB text member. `nad_install()` accepts exactly one county and writes it atomically into the Hive-partitioned Parquet dataset under `v2/nad/23/state=<STATE>/county_fips=<FIPS>/part-0.parquet`. `nad()` uses that file while `nad_dataset()` opens all installed counties as one lazy Arrow dataset. Each county install is inventoried with row count, size, SHA-256 digest, revision, and timestamp in `v2/nad_manifest/23/counties.parquet`; `nad_manifest()` reads or fully validates that inventory. Former NAD layouts are not searched or migrated. The packaged `nad_example_data()` and prepared matching index have been rebuilt from Hamilton County revision 23 data.
- The new packaged `nad_catalog()` inventories the 2,259 revision 23 source county labels and maps them to Census county FIPS identifiers, including their source row counts. `nad_install()` checks the catalog before scanning the national source and uses its exact source label. The mapping retains distinct unsuffixed county and explicit independent-city labels. A zero-row extraction is rejected before writing a Parquet file or manifest entry because it indicates source/catalog drift rather than a valid empty installation.
- addr now uses stow 0.3.0 to retain the Census TIGER `FEATNAMES` and `ADDRFEAT` source ZIP files as durable managed local copies. These source files are stored under addr's `stow/tiger_feat_names` and `stow/tiger_addr_feat` directories.
  - TIGER HTTPS downloads are now paced at least one second apart and transient failures are retried three times with exponential backoff and jitter. Invalid Census `Request Rejected` HTML responses are not cached and now produce an explanatory error, including a support ID when present. An explicit `options(addr.tiger_download_protocol = "ftp")` override supports Census's documented bulk-download route without silently downgrading normal encrypted HTTPS requests.
  - The addr container image now installs stow 0.3.0 so TIGER source ZIP files can also be retained as durable managed local copies when using the image.
  - The processed TAF dataset used for geocoding is not managed by `stow()` and remains under `v2/tiger_addr_feat/<year>` in addr's user data directory. By default, when `geocode()` needs a missing county, `taf_install()` uses the durable managed local copies of the source TIGER ZIP files to build that county's processed TAF Parquet files locally. The optional preprocessed national TAF release bundle, its manifest, and its installation workflow remain separate from stow.

# addr 1.3.0

## What's Changed

* TIGER/Line address features (TAF) files can be installed using a fixed asset from the addr github releases instead of downloading and installing from source by county as needed (see install instructions in README)
* guard live tiger download tests by @cole-brokamp in https://github.com/geomarker-io/addr/pull/93
* add batch geocoder cli by @cole-brokamp in https://github.com/geomarker-io/addr/pull/92

**Full Changelog**: https://github.com/geomarker-io/addr/compare/v1.2.0...v1.3.0

# addr 1.2.0

## What's Changed

* suppress extra offset for tiger offset ranges by @cole-brokamp in https://github.com/geomarker-io/addr/pull/84
* improve geocode error context by @cole-brokamp in https://github.com/geomarker-io/addr/pull/85
* reduce geocode memory use by @cole-brokamp in https://github.com/geomarker-io/addr/pull/87
* experimental addr container image by @cole-brokamp in https://github.com/geomarker-io/addr/pull/88
* TIGER download improvements by @cole-brokamp in https://github.com/geomarker-io/addr/pull/89
* reduce example data sizes and tighten packaged fixtures by @cole-brokamp in https://github.com/geomarker-io/addr/pull/90
* vendor rust crates for cran builds by @cole-brokamp in https://github.com/geomarker-io/addr/pull/91

**Full Changelog**: https://github.com/geomarker-io/addr/compare/v1.1.0...v1.2.0

# addr 1.1.0

## What's Changed

- TIGER/Line street range address geocoding, including a suite of functions to manage and install local copies of prepared tiger address features and feature names
- Added "swap" and "compatible" for street type matching modes
- Improvements to as_addr() when parsing invalid address numbers or ZIP Codes
- Unmapped types/directions preserve original instead of returning missing values

**Full Changelog**: https://github.com/geomarker-io/addr/compare/v1.0.0...v1.1.0

# addr 1.0.0

## What's Changed

* deduplicate parsed address tags in as_addr; closes #56 by @cole-brokamp in https://github.com/geomarker-io/addr/pull/60
* update street post type mappings; closes #58 by @cole-brokamp in https://github.com/geomarker-io/addr/pull/61
* Addr street matching by @cole-brokamp in https://github.com/geomarker-io/addr/pull/62
* addr_match using addr_part matching functions by @cole-brokamp in https://github.com/geomarker-io/addr/pull/63
* addr_left_join uses addr_match to left_join two data frames with addr columns by @cole-brokamp in https://github.com/geomarker-io/addr/pull/64
* Add prepared NAD example data for faster addr_match examples and tests by @cole-brokamp in https://github.com/geomarker-io/addr/pull/65
* Reduced dependencies on external packages by @cole-brokamp in https://github.com/geomarker-io/addr/pull/66
* cache NAD extracts (including pre-warmed cache for hamilton, ohio) by @cole-brokamp in https://github.com/geomarker-io/addr/pull/70
* NAD downloaded by release; more NAD data binary functionality by @cole-brokamp in https://github.com/geomarker-io/addr/pull/71

**Full Changelog**: https://github.com/geomarker-io/addr/compare/v0.9.11...v1.0.0

# addr 0.9.11

v0.9 of the addr package is a major rewrite and is being released for testing and final development in preparation for a v1.0 release:

- internally, moved to S7 object oriented system to represent addr objects; reduced the dependence on external packages
- addr() objects are now composed of addr_number(), addr_street(), and addr_place() objects; each have tags corresponding to the US Postal Address Standard
- addr vectors can be left joined if columns in a data.frame or tibble with `addr_left_fuzzy_join()`
- fuzzy join functions available for all fields in an addr vector, or only one
- tagged address components, like state or street name pre/post types and directionals, can be mapped to known variants and abbreviations based on USPS Publications and manually crafted edge cases and common misspellings
  - more parsing is done internally (e.g., digits of address number must be numeric, ZIP codes are truncated to five digits)
- introduction of TIGER/Line address feature files
- moved census geography intersection and date-related functions to {geomarker}
- deprecate addr_hash
- new functions to download and extract addr objects from the National Address Database
- new experimental approach to address range geocoding with local TIGER/Line address feature files
- introduce the phonetic street key (psk), a code used to block street names according to ordinal names and non-ordinal phonetics
- add experimental functions to directly match addr_number() objects and zip_codes

# addr 0.7.0

## What's Changed

* `fuzzy_match()` and `fuzzy_match_addr_field()` functions for more flexible matching within `addr_match()` by @cole-brokamp in https://github.com/geomarker-io/addr/pull/40
* addr_hash function to create md5 hash of addr vector by @cole-brokamp in https://github.com/geomarker-io/addr/pull/41
* impute_date_ranges() from a chronological sequence of dates by @cole-brokamp in https://github.com/geomarker-io/addr/pull/42
* national address database example data for Hamilton County, OH by @cole-brokamp in https://github.com/geomarker-io/addr/pull/45
* use codec::cincy_addr_geo() for CAGIS addresses and don't remove # from address text by @cole-brokamp in https://github.com/geomarker-io/addr/pull/46

**Full Changelog**: https://github.com/geomarker-io/addr/compare/v0.6.0...v0.7.0

# addr 0.6.0

## What's Changed

* update to q4 2024 CAGIS addresses by @cole-brokamp in https://github.com/geomarker-io/addr/pull/36

**Full Changelog**: https://github.com/geomarker-io/addr/compare/v0.5.0...v0.6.0

# addr 0.5.0

## What's Changed

* `addr_match_geocode()` reports more specific `match_method` for tiger matching ("street" or "range") by @cole-brokamp in https://github.com/geomarker-io/addr/pull/25
* addr matched to tiger street but no range returns closest range by @cole-brokamp in https://github.com/geomarker-io/addr/pull/30
* print addr objects in color and with style to visually represent tags by @cole-brokamp in https://github.com/geomarker-io/addr/pull/34
* example code for working with s2 parent cells and mapping with  rdeck by @cole-brokamp in https://github.com/geomarker-io/addr/pull/33
* use dummy address components for better addr parsing of tiger street range names by @erikarasnick in https://github.com/geomarker-io/addr/pull/35

## New Contributors

* @erikarasnick made their first contribution in https://github.com/geomarker-io/addr/pull/35

**Full Changelog**: https://github.com/geomarker-io/addr/compare/v0.4.0...v0.5.0

# addr 0.4.0

## What's Changed

* tiger_block_group() can handle s2 vectors with NA values by @cole-brokamp in https://github.com/cole-brokamp/addr/pull/18
* tiger matching returns all street ranges when number is out of range by @cole-brokamp in https://github.com/cole-brokamp/addr/pull/19
* geocoding with matching and tiger range matching by @cole-brokamp in https://github.com/cole-brokamp/addr/pull/20

**Full Changelog**: https://github.com/cole-brokamp/addr/compare/v0.3.1...v0.4.0

# addr 0.3.1

quick fix to export other addr_match functions

# addr 0.3.0

## What's Changed

- added example voter addresses data
- changed how addr_match works under the hood; is now faster
- added ability to simplify matched addr list into addr vector (in both address and street name/number matching)
- added example eviction lab addresses
- update example geocoding based on changes
- check in voter geocode addr
- use degauss for comparison geocoding

**Full Changelog**: https://github.com/cole-brokamp/addr/compare/v0.2.0...v0.3.0

# addr 0.2.0

## What's Changed

* add matching to tigris street range by @cole-brokamp in https://github.com/cole-brokamp/addr/pull/15
  * update cargo and roxygen
  * keep cagis addr just in package folder and as function `cagis_addr()`
  * cache downloads of tigris street ranges in R_USER_CACHE_DIR
  * addr_match_street for just matching on street names
  * use street match function for matching on tiger street ranges
  * add summarizing street range matches
  * change names; closes https://github.com/cole-brokamp/addr/issues/16
  * update readme with new examples

**Full Changelog**: https://github.com/cole-brokamp/addr/compare/v0.1.0...v0.2.0

# addr 0.1.0

initial release
