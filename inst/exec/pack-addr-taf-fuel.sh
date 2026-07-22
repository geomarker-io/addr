#!/usr/bin/env bash
set -euo pipefail

YEAR="${1:-2025}"
VERSION="${2:-v1}"
OUT_DIR="${3:-$PWD}"
BASE="addr-taf-${VERSION}-${YEAR}"
ARCHIVE="${BASE}.tar.zst"
JSON_FILE="${BASE}.json"
ARCHIVE_PARQUET_COMPRESSION="zstd"
ARCHIVE_PARQUET_COMPRESSION_LEVEL="9"
INSTALLED_PARQUET_COMPRESSION="snappy"

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

for output in "$ARCHIVE" "$JSON_FILE"; do
  if [ -e "${OUT_DIR}/${output}" ] || [ -L "${OUT_DIR}/${output}" ]; then
    die "output already exists: ${OUT_DIR}/${output}"
  fi
done

ADDR_DATA_DIR="$(Rscript -e 'cat(tools::R_user_dir("addr", "data"))')"
ADDR_PACKAGE_VERSION="$(
  Rscript -e 'cat(as.character(utils::packageVersion("addr")))'
)"
echo "packaging TAF fuel for addr package version ${ADDR_PACKAGE_VERSION}"

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

STAGING_ROOT="${TMP_DIR}/staging"
mkdir -p "$STAGING_ROOT"

echo "transcoding TAF parquet for distribution (${ARCHIVE_PARQUET_COMPRESSION} level ${ARCHIVE_PARQUET_COMPRESSION_LEVEL})"
ADDR_TAF_PACK_SOURCE_ROOT="$ADDR_DATA_DIR" \
  ADDR_TAF_PACK_STAGING_ROOT="$STAGING_ROOT" \
  ADDR_TAF_PACK_DATA_PATH="$DATA_PATH" \
  ADDR_TAF_PACK_MANIFEST_PATH="$MANIFEST_PATH" \
  ADDR_TAF_PACK_COMPRESSION="$ARCHIVE_PARQUET_COMPRESSION" \
  ADDR_TAF_PACK_COMPRESSION_LEVEL="$ARCHIVE_PARQUET_COMPRESSION_LEVEL" \
  Rscript - <<'RSCRIPT'
source_root <- Sys.getenv("ADDR_TAF_PACK_SOURCE_ROOT")
staging_root <- Sys.getenv("ADDR_TAF_PACK_STAGING_ROOT")
relative_roots <- c(
  Sys.getenv("ADDR_TAF_PACK_DATA_PATH"),
  Sys.getenv("ADDR_TAF_PACK_MANIFEST_PATH")
)
compression <- Sys.getenv("ADDR_TAF_PACK_COMPRESSION")
compression_level <- as.integer(Sys.getenv("ADDR_TAF_PACK_COMPRESSION_LEVEL"))

if (!requireNamespace("nanoparquet", quietly = TRUE)) {
  stop("nanoparquet is required to package TAF fuel", call. = FALSE)
}
if (is.na(compression_level)) {
  stop("invalid parquet compression level", call. = FALSE)
}

files_total <- sum(vapply(relative_roots, function(relative_root) {
  relative_files <- list.files(
    file.path(source_root, relative_root),
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE
  )
  sum(basename(relative_files) != ".DS_Store")
}, integer(1)))
files_done <- 0L
parquet_options <- nanoparquet::parquet_options(
  compression_level = compression_level
)

for (relative_root in relative_roots) {
  root <- file.path(source_root, relative_root)
  relative_files <- list.files(
    root,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = FALSE,
    include.dirs = FALSE
  )
  relative_files <- relative_files[basename(relative_files) != ".DS_Store"]

  for (relative_file in relative_files) {
    source_file <- file.path(root, relative_file)
    staging_file <- file.path(staging_root, relative_root, relative_file)
    dir.create(dirname(staging_file), recursive = TRUE, showWarnings = FALSE)

    if (grepl("[.]parquet$", source_file, ignore.case = TRUE)) {
      value <- nanoparquet::read_parquet(source_file)
      nanoparquet::write_parquet(
        value,
        staging_file,
        compression = compression,
        options = parquet_options
      )
    } else if (!file.copy(source_file, staging_file, overwrite = FALSE)) {
      stop("could not stage file: ", source_file, call. = FALSE)
    }

    files_done <- files_done + 1L
    if (files_done %% 1000L == 0L || files_done == files_total) {
      message("staged ", files_done, " of ", files_total, " files")
    }
  }
}
RSCRIPT

STAGED_DATA_FILE_COUNT="$(count_files "${STAGING_ROOT}/${DATA_PATH}")"
STAGED_MANIFEST_FILE_COUNT="$(count_files "${STAGING_ROOT}/${MANIFEST_PATH}")"
[ "$STAGED_DATA_FILE_COUNT" = "$DATA_FILE_COUNT" ] || die "staged TAF data file count does not match source"
[ "$STAGED_MANIFEST_FILE_COUNT" = "$MANIFEST_FILE_COUNT" ] || die "staged TAF manifest file count does not match source"

cd "$STAGING_ROOT"
COPYFILE_DISABLE=1 tar \
  --no-xattrs \
  --exclude='.DS_Store' \
  --exclude='*/tiger_addr_feat_locks/*' \
  -cf - \
  "$DATA_PATH" \
  "$MANIFEST_PATH" \
  | zstd -T0 -19 -o "${TMP_DIR}/${ARCHIVE}"

ARCHIVE_SHA256="$(shasum -a 256 "${TMP_DIR}/${ARCHIVE}" | awk '{print $1}')"
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
  json_string_field "archive_parquet_compression" "$ARCHIVE_PARQUET_COMPRESSION"
  json_number_field "archive_parquet_compression_level" "$ARCHIVE_PARQUET_COMPRESSION_LEVEL"
  json_string_field "installed_parquet_compression" "$INSTALLED_PARQUET_COMPRESSION"
  json_string_field "created_utc" "$CREATED_UTC"
  json_string_field "data_path" "$DATA_PATH"
  json_string_field "manifest_path" "$MANIFEST_PATH"
  json_string_field "required_manifest_file" "$REQUIRED_MANIFEST_FILE"
  json_number_field "data_file_count" "$DATA_FILE_COUNT"
  json_number_field "manifest_file_count" "$MANIFEST_FILE_COUNT" ""
  printf '}\n'
} > "${TMP_DIR}/${JSON_FILE}"

mv "${TMP_DIR}/${ARCHIVE}" "${OUT_DIR}/${ARCHIVE}"
mv "${TMP_DIR}/${JSON_FILE}" "${OUT_DIR}/${JSON_FILE}"

cleanup
trap - EXIT

echo "wrote: ${OUT_DIR}/${ARCHIVE}"
echo "wrote: ${OUT_DIR}/${JSON_FILE}"
