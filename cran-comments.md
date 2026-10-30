This is the first CRAN submission of addr, version 2.1.0.

Release source

- addr_2.1.0.tar.gz, built with R 4.6.2 on Linux from commit 4a3587c.
- Size: 7,075,216 bytes.
- SHA-256: 15ba84e0225aad0044e3ba201631583d2b497706cf542703b62e1512b00c636a
- Source artifact and exact-tarball R-devel check:
  https://github.com/geomarker-io/addr/actions/runs/41069125159
- The same archive was checked locally and uploaded to Win-builder R-devel.
  Updates to this file do not affect it; cran-comments.md is excluded from
  the source package.

Checks performed on 2026-10-30

- macOS arm64, R 4.6.1: R CMD check --as-cran completed with 0 errors,
  0 warnings, and 1 NOTE: "New submission". Both local and remote incoming
  checks were enabled, using the official stat.ethz.ch CRAN mirror for metadata.
  There is no development-version NOTE. Tests: 856 passes and 3 opt-in live
  TIGER download skips. PDF and HTML manuals passed, with checkbashisms and
  HTML Tidy 5.8.0 available.
- Ubuntu 24.04.5, R-devel (2026-10-26 r90766): the identical source archive
  passed R CMD check --as-cran, including PDF and HTML manuals, with 0 errors,
  0 warnings, and 1 NOTE: "New submission". Remote incoming checks were enabled.
  This job deliberately omits arrow, dplyr, leaflet, mirai, and sf to exercise
  required-dependency-only use; their absence is reported as INFO. Full suggested
  dependencies are exercised by the local and other platform checks.
- urlchecker: no invalid URLs found.
- Release platform matrix:
  https://github.com/geomarker-io/addr/actions/runs/41069125122
  Release R on macOS, Windows, and Linux; Windows/Linux R-devel; Linux R-oldrel;
  R 4.2.0 with Rust 1.71; and installed CLI validation all passed.
  Windows R-devel (2026-10-29 r90794 ucrt) reports Status: OK, with 844 test
  passes and 5 skips (3 opt-in downloads and 2 Unix-specific tests).
- Win-builder R-devel: the archive was accepted through the HTTPS upload form
  on 2026-10-30 at 14:12 UTC. At 14:44 UTC, the archive was still listed in
  the public R-devel queue. No result email has arrived yet; its outcome remains
  pending and must be reviewed before CRAN submission.

Minimum-version notes

The R 4.2.0 / Rust 1.71 job has 0 errors, 0 warnings, and 3 NOTEs, with 843 test
passes and 5 skips (3 opt-in downloads and 2 absent optional interfaces):

- The suggested packages listed above are deliberately absent.
- Installed size is 16.4 MB, dominated by an 11 MB Rust-linked library from
  the minimum supported compiler. Bundled data and documentation are each
  below 5 MB, and the source archive is below 10 MB.
- R 4.2 codetools treats property names passed to S7's compatibility `@`
  accessor as undefined globals. These are address properties, not free-variable
  lookups. The package follows S7's conditional import guidance for R < 4.3;
  current R and R-devel checks do not report this note.

Installation and external data

Rust sources and dependencies are bundled in src/rust/vendor.tar.xz. Builds use
cargo --locked --offline and at most two compilation jobs; neither Rust tools
nor dependency code are downloaded at installation. Cargo/rustc >= 1.71 are
required. DESCRIPTION credits upstream contributors, and COPYRIGHTS and
RUST-LICENSES preserve detailed notices. Post-link cleanup removes Rust build
intermediates while retaining Makevars and C objects for R's symbol checks.

Loading addr does not access the network. Large downloads are in non-running
examples; live-download tests are opt-in and skipped on CRAN. Local fixtures
exercise geocoding and data management. The optional mirai integration test
skips narrowly recognized prohibited or unsupported localhost sockets.

On-demand NAD/TIGER data are stored beneath tools::R_user_dir("addr", "data").
stow manages source inventory and cleanup; addr's removal helpers manage
processed counties and their manifests, with previews by default. A missing-
county preflight and retained-data notice make automatic geocoding installs
visible. Packaged fixtures and references retain their source attribution and
reuse terms in DATA-SOURCES. The Eviction Lab hotspot database is separately
licensed under ODC-BY 1.0; LICENSE.note and ODC-BY-1.0 preserve its terms, also
embedded with attribution in the fixture. Addr's own software is MIT-licensed;
vendored Rust license terms are preserved separately.
