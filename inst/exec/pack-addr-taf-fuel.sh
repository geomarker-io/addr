#!/usr/bin/env bash
set -euo pipefail

YEAR="${1:-2025}"
VERSION="${2:-v1}"
OUT_DIR="${3:-$PWD}"
ARCHIVE="addr-taf-${VERSION}-${YEAR}.tar.zst"

ADDR_DATA_DIR="$(Rscript -e 'cat(tools::R_user_dir("addr", "data"))')"

cd "$ADDR_DATA_DIR"

test -d "${VERSION}/tiger_addr_feat/${YEAR}"
test -f "${VERSION}/tiger_addr_feat_manifest/${YEAR}/county_zip.parquet"

mkdir -p "$OUT_DIR"

# TODO: If final archives are produced on macOS, suppress Apple extended
# metadata such as LIBARCHIVE.xattr.com.apple.provenance so GNU tar users do
# not see unknown extended header warnings during extraction.
tar \
  --exclude='.DS_Store' \
  --exclude='*/tiger_addr_feat_locks/*' \
  -cf - \
  "${VERSION}/tiger_addr_feat/${YEAR}" \
  "${VERSION}/tiger_addr_feat_manifest/${YEAR}" \
  | zstd -T0 -19 -o "${OUT_DIR}/${ARCHIVE}"

cd "$OUT_DIR"
shasum -a 256 "$ARCHIVE" > "${ARCHIVE}.sha256"

echo "wrote: ${OUT_DIR}/${ARCHIVE}"
echo "wrote: ${OUT_DIR}/${ARCHIVE}.sha256"
