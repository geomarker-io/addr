#!/usr/bin/env bash
set -euo pipefail

ARCHIVE="${1:?usage: install-addr-taf-fuel.sh ARCHIVE.tar.zst [SHA256_FILE]}"
SHA256_FILE="${2:-${ARCHIVE}.sha256}"

ADDR_DATA_DIR="$(Rscript -e 'cat(tools::R_user_dir("addr", "data"))')"

test -f "$ARCHIVE"
test -f "$SHA256_FILE"

shasum -a 256 -c "$SHA256_FILE"

mkdir -p "$ADDR_DATA_DIR"

zstd -dc "$ARCHIVE" | tar -C "$ADDR_DATA_DIR" -xf -

echo "installed addr TAF fuel under: $ADDR_DATA_DIR"

Rscript -e 'stopifnot(nrow(addr::taf_zip("45220", year = "2025")) > 0); message("taf_zip verification passed")'
Rscript -e 'addr::geocode(addr::as_addr("3333 Burnet Ave Cincinnati OH 45229"), year = "2025", taf_install = FALSE); message("geocode verification passed")'
