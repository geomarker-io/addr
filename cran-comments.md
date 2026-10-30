Submission preparation notes for addr 2.0.0.9001

This is a development checklist, not a completed CRAN submission record.
Keep the development version until the release is prepared, then rebuild and
check the exact source tarball to be uploaded.

- The Rust dependency sources are bundled in src/rust/vendor.tar.xz. Installation
  uses cargo --locked --offline with at most two compilation jobs. Cargo and
  rustc 1.71 or newer are required; the package does not install either tool.
  DESCRIPTION credits upstream contributors; COPYRIGHTS and RUST-LICENSES retain
  the detailed attribution and license notices in installed packages.
- Loading the package does not download data. Live TIGER download tests are
  explicitly opt-in and skipped during CRAN checks. Local fixture tests exercise
  the data readers and geocoding. Optional mirai integration skips only recognized
  dispatcher socket-permission or unsupported-address-family errors.
- Large-data examples are not run automatically. NAD county preparation can
  require a roughly 7.6 GB archive. TIGER source and processed-data copies use the
  package-specific user data directory. Download controls and retention behavior
  will be reviewed separately before submission.
- Packaged data provenance and redistribution status are recorded in DATA-SOURCES.
  Resolve the voter-address and Eviction Lab fixture redistribution status before
  uploading a release; these are not represented as covered by the MIT license.

Validation record

- Local source tarball, macOS arm64 / R 4.6.1, 2026-10-30: R CMD check
  --as-cran completed with 0 errors, 0 warnings, and 1 incoming-feasibility note
  for a new submission with the intentionally retained development version.
  Tests: 795 passes, 3 opt-in live-download skips. Both PDF and HTML manual
  checks passed with checkbashisms and HTML Tidy 5.8.0 available. A separate
  urlchecker run found no invalid URLs.
- CI covers release R on macOS, Windows, and Linux; R-devel on Windows and Linux;
  Linux R-oldrel; and R 4.2.0 / Rust 1.71 with selected optional packages absent.
  Record the completed run URLs and results for the final release here.
- The Linux release job includes PDF manual validation. Shell/HTML checking tools
  are installed on Unix runners, and warnings fail the CI checks.
- Before submission, run current Win-builder R-devel and macOS builder checks on
  the exact release tarball and record their results here. Those service checks
  have not yet been submitted.
- A first CRAN submission may report the normal "New submission" note. The
  development-version note must be eliminated by choosing the release version.
- Confirm the maintainer address and credited authors, review current CRAN
  policies, and resolve all unexplained warnings or significant notes before
  submitting through the CRAN form.
