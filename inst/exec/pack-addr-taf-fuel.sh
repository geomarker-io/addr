#!/usr/bin/env bash
set -euo pipefail

YEAR="${1:-2025}"
VERSION="${2:-v1}"
OUT_DIR="${3:-$PWD}"
BASE="addr-taf-${VERSION}-${YEAR}"
ARCHIVE="${BASE}.tar.zst"
SHA256_FILE="${ARCHIVE}.sha256"
JSON_FILE="${BASE}.json"
README_FILE="${BASE}.README.md"

die() {
  echo "pack-addr-taf-fuel: $*" >&2
  exit 1
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

json_escape() {
  local value="$1"
  value="${value//\\/\\\\}"
  value="${value//\"/\\\"}"
  value="${value//$'\n'/\\n}"
  value="${value//$'\r'/\\r}"
  value="${value//$'\t'/\\t}"
  printf '%s' "$value"
}

json_string_field() {
  local key="$1"
  local value="$2"
  local comma=","
  if [ "$#" -ge 3 ]; then
    comma="$3"
  fi
  printf '  "%s": "%s"%s\n' "$key" "$(json_escape "$value")" "$comma"
}

json_number_field() {
  local key="$1"
  local value="$2"
  local comma=","
  if [ "$#" -ge 3 ]; then
    comma="$3"
  fi
  printf '  "%s": %s%s\n' "$key" "$value" "$comma"
}

count_files() {
  find "$1" -type f ! -name '.DS_Store' | wc -l | tr -d '[:space:]'
}

require_command Rscript
require_command tar
require_command zstd
require_command shasum
require_command find
require_command wc
require_command awk
require_command date

mkdir -p "$OUT_DIR"
OUT_DIR="$(cd "$OUT_DIR" && pwd)"

for output in "$ARCHIVE" "$SHA256_FILE" "$JSON_FILE" "$README_FILE"; do
  if [ -e "${OUT_DIR}/${output}" ] || [ -L "${OUT_DIR}/${output}" ]; then
    die "output already exists: ${OUT_DIR}/${output}"
  fi
done

ADDR_DATA_DIR="$(Rscript -e 'cat(tools::R_user_dir("addr", "data"))')"
ADDR_PACKAGE_VERSION="$(
  Rscript -e '
version <- NA_character_
if (file.exists("DESCRIPTION")) {
  desc <- read.dcf("DESCRIPTION")
  if (identical(desc[1, "Package"], "addr")) {
    version <- desc[1, "Version"]
  }
}
if (is.na(version)) {
  version <- tryCatch(
    as.character(utils::packageVersion("addr")),
    error = function(e) NA_character_
  )
}
if (is.na(version)) {
  stop("could not determine addr package version", call. = FALSE)
}
cat(version)
'
)"

DATA_PATH="${VERSION}/tiger_addr_feat/${YEAR}"
MANIFEST_PATH="${VERSION}/tiger_addr_feat_manifest/${YEAR}"
REQUIRED_MANIFEST_FILE="${MANIFEST_PATH}/county_zip.parquet"

cd "$ADDR_DATA_DIR"

[ -d "$DATA_PATH" ] || die "missing TAF data directory: ${ADDR_DATA_DIR}/${DATA_PATH}"
[ -d "$MANIFEST_PATH" ] || die "missing TAF manifest directory: ${ADDR_DATA_DIR}/${MANIFEST_PATH}"
[ -f "$REQUIRED_MANIFEST_FILE" ] || die "missing required manifest file: ${ADDR_DATA_DIR}/${REQUIRED_MANIFEST_FILE}"

DATA_FILE_COUNT="$(count_files "$DATA_PATH")"
MANIFEST_FILE_COUNT="$(count_files "$MANIFEST_PATH")"

TMP_DIR="$(mktemp -d "${OUT_DIR}/.addr-taf-pack.XXXXXX")"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

COPYFILE_DISABLE=1 tar \
  --no-xattrs \
  --exclude='.DS_Store' \
  --exclude='*/tiger_addr_feat_locks/*' \
  -cf - \
  "$DATA_PATH" \
  "$MANIFEST_PATH" \
  | zstd -T0 -19 -o "${TMP_DIR}/${ARCHIVE}"

(
  cd "$TMP_DIR"
  shasum -a 256 "$ARCHIVE" > "$SHA256_FILE"
)

ARCHIVE_SHA256="$(awk '{print $1}' "${TMP_DIR}/${SHA256_FILE}")"
ARCHIVE_SIZE_BYTES="$(wc -c < "${TMP_DIR}/${ARCHIVE}" | tr -d '[:space:]')"
CREATED_UTC="$(date -u '+%Y-%m-%dT%H:%M:%SZ')"

{
  printf '{\n'
  json_string_field "artifact_type" "addr-taf-fuel"
  json_number_field "schema_version" "1"
  json_string_field "taf_version" "$VERSION"
  json_string_field "taf_year" "$YEAR"
  json_string_field "addr_package_version" "$ADDR_PACKAGE_VERSION"
  json_string_field "addr_package_version_required" "$ADDR_PACKAGE_VERSION"
  json_string_field "archive_file" "$ARCHIVE"
  json_string_field "archive_sha256" "$ARCHIVE_SHA256"
  json_number_field "archive_size_bytes" "$ARCHIVE_SIZE_BYTES"
  json_string_field "created_utc" "$CREATED_UTC"
  json_string_field "data_path" "$DATA_PATH"
  json_string_field "manifest_path" "$MANIFEST_PATH"
  json_string_field "required_manifest_file" "$REQUIRED_MANIFEST_FILE"
  json_number_field "data_file_count" "$DATA_FILE_COUNT"
  json_number_field "manifest_file_count" "$MANIFEST_FILE_COUNT" ""
  printf '}\n'
} > "${TMP_DIR}/${JSON_FILE}"

cat > "${TMP_DIR}/${README_FILE}" <<EOF
# addr TAF fuel artifact

This artifact contains installed addr TIGER address feature data for:

- TAF version: ${VERSION}
- TAF year: ${YEAR}
- addr package version used to package it: ${ADDR_PACKAGE_VERSION}
- required addr package version for install: ${ADDR_PACKAGE_VERSION}
- archive: ${ARCHIVE}
- sha256: ${ARCHIVE_SHA256}

## Install

Run the installer from a shell, not from inside R.

From a source checkout:

\`\`\`sh
inst/exec/install-addr-taf-fuel.sh ${ARCHIVE}
\`\`\`

From an installed addr package:

\`\`\`sh
bash "\$(Rscript -e 'cat(system.file("exec", "install-addr-taf-fuel.sh", package = "addr"))')" \\
  ${ARCHIVE}
\`\`\`

By default, the installer writes to the directory returned by:

\`\`\`sh
Rscript -e 'cat(tools::R_user_dir("addr", "data"))'
\`\`\`

To install somewhere else, set R_USER_DATA_DIR before running the installer:

\`\`\`sh
export R_USER_DATA_DIR=/scratch/<user>/addr-data
inst/exec/install-addr-taf-fuel.sh ${ARCHIVE}
\`\`\`

With standard R user-directory behavior, that installs under:

\`\`\`text
\${R_USER_DATA_DIR}/R/addr/${VERSION}/tiger_addr_feat/${YEAR}
\${R_USER_DATA_DIR}/R/addr/${VERSION}/tiger_addr_feat_manifest/${YEAR}
\`\`\`

## Replacement behavior

The installer refuses to overwrite existing TAF fuel. If either target year
directory already exists, delete the existing directories first, then rerun the
installer.

For this artifact, the target directories are:

\`\`\`text
${VERSION}/tiger_addr_feat/${YEAR}
${VERSION}/tiger_addr_feat_manifest/${YEAR}
\`\`\`

The installer validates the archive checksum and metadata before installing.
It also requires addr package version ${ADDR_PACKAGE_VERSION}.
Keep the .tar.zst, .tar.zst.sha256, .json, and this README file together.
EOF

mv "${TMP_DIR}/${ARCHIVE}" "${OUT_DIR}/${ARCHIVE}"
mv "${TMP_DIR}/${SHA256_FILE}" "${OUT_DIR}/${SHA256_FILE}"
mv "${TMP_DIR}/${JSON_FILE}" "${OUT_DIR}/${JSON_FILE}"
mv "${TMP_DIR}/${README_FILE}" "${OUT_DIR}/${README_FILE}"

trap - EXIT
rmdir "$TMP_DIR"

echo "wrote: ${OUT_DIR}/${ARCHIVE}"
echo "wrote: ${OUT_DIR}/${SHA256_FILE}"
echo "wrote: ${OUT_DIR}/${JSON_FILE}"
echo "wrote: ${OUT_DIR}/${README_FILE}"
