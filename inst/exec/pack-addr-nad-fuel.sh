#!/usr/bin/env bash
set -euo pipefail

REVISION="${1:-23}"
OUT_DIR="${2:-$PWD}"
BASE="addr-nad-r${REVISION}"
ARCHIVE="${BASE}.tar.zst"
JSON_FILE="${BASE}.json"

die() {
  echo "pack-addr-nad-fuel: $*" >&2
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

validate_unsigned_integer() {
  local value="$1"
  local label="$2"
  case "$value" in
    ''|*[!0-9]*) die "${label} must be an unsigned integer: ${value}" ;;
  esac
}

count_files() {
  find "$1" -type f ! -name '.DS_Store' | wc -l | tr -d '[:space:]'
}

validate_nad_tree() {
  local data_dir="$1"
  local manifest_file="$2"
  ADDR_NAD_FUEL_DATA_DIR="$data_dir" \
    ADDR_NAD_FUEL_MANIFEST_FILE="$manifest_file" \
    ADDR_NAD_FUEL_REVISION="$REVISION" \
    Rscript - <<'RSCRIPT'
data_dir <- Sys.getenv("ADDR_NAD_FUEL_DATA_DIR")
manifest_file <- Sys.getenv("ADDR_NAD_FUEL_MANIFEST_FILE")
revision <- as.integer(Sys.getenv("ADDR_NAD_FUEL_REVISION"))

manifest <- nanoparquet::read_parquet(manifest_file) |>
  tibble::as_tibble()
validator <- getFromNamespace("nad_validate_manifest", "addr")
validator(
  manifest,
  data_root = data_dir,
  version = revision,
  verify_files = TRUE
)
cat(nrow(manifest))
RSCRIPT
}

require_command Rscript
require_command tar
require_command zstd
require_command shasum
require_command find
require_command wc
require_command awk
require_command date
require_command cp
require_command mktemp

validate_unsigned_integer "$REVISION" "NAD revision"
[ "$REVISION" = "23" ] || die "unsupported NAD revision: ${REVISION}"

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
echo "packaging NAD fuel for addr package version ${ADDR_PACKAGE_VERSION}"

DATA_PATH="v2/nad/${REVISION}"
MANIFEST_PATH="v2/nad_manifest/${REVISION}"
REQUIRED_MANIFEST_FILE="${MANIFEST_PATH}/counties.parquet"
DATA_DIR="${ADDR_DATA_DIR}/${DATA_PATH}"
MANIFEST_DIR="${ADDR_DATA_DIR}/${MANIFEST_PATH}"
MANIFEST_FILE="${ADDR_DATA_DIR}/${REQUIRED_MANIFEST_FILE}"

[ -d "$DATA_DIR" ] || die "missing NAD data directory: ${DATA_DIR}"
[ -d "$MANIFEST_DIR" ] || die "missing NAD manifest directory: ${MANIFEST_DIR}"
[ -f "$MANIFEST_FILE" ] || die "missing required manifest file: ${MANIFEST_FILE}"

echo "validating installed NAD counties against the local manifest"
COUNTY_COUNT="$(validate_nad_tree "$DATA_DIR" "$MANIFEST_FILE")"
validate_unsigned_integer "$COUNTY_COUNT" "NAD county count"
[ "$COUNTY_COUNT" -gt 0 ] || die "NAD manifest contains no installed counties"

DATA_FILE_COUNT="$(count_files "$DATA_DIR")"
MANIFEST_FILE_COUNT="$(count_files "$MANIFEST_DIR")"
SOURCE_SYMLINK_COUNT="$(find "$DATA_DIR" "$MANIFEST_DIR" -type l | wc -l | tr -d '[:space:]')"
[ "$DATA_FILE_COUNT" = "$COUNTY_COUNT" ] || die "NAD data file count does not match manifest rows"
[ "$MANIFEST_FILE_COUNT" = "1" ] || die "NAD manifest directory must contain only counties.parquet"
[ "$SOURCE_SYMLINK_COUNT" = "0" ] || die "NAD fuel source directories must not contain symbolic links"

TMP_DIR="$(mktemp -d "${OUT_DIR}/.addr-nad-pack.XXXXXX")"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

STAGING_ROOT="${TMP_DIR}/staging"
mkdir -p "${STAGING_ROOT}/$(dirname "$DATA_PATH")"
mkdir -p "${STAGING_ROOT}/$(dirname "$MANIFEST_PATH")"
cp -R "$DATA_DIR" "${STAGING_ROOT}/${DATA_PATH}"
cp -R "$MANIFEST_DIR" "${STAGING_ROOT}/${MANIFEST_PATH}"

STAGED_DATA_DIR="${STAGING_ROOT}/${DATA_PATH}"
STAGED_MANIFEST_DIR="${STAGING_ROOT}/${MANIFEST_PATH}"
STAGED_MANIFEST_FILE="${STAGING_ROOT}/${REQUIRED_MANIFEST_FILE}"
STAGED_DATA_FILE_COUNT="$(count_files "$STAGED_DATA_DIR")"
STAGED_MANIFEST_FILE_COUNT="$(count_files "$STAGED_MANIFEST_DIR")"
STAGED_SYMLINK_COUNT="$(find "$STAGED_DATA_DIR" "$STAGED_MANIFEST_DIR" -type l | wc -l | tr -d '[:space:]')"
[ "$STAGED_DATA_FILE_COUNT" = "$DATA_FILE_COUNT" ] || die "staged NAD data file count does not match source"
[ "$STAGED_MANIFEST_FILE_COUNT" = "$MANIFEST_FILE_COUNT" ] || die "staged NAD manifest file count does not match source"
[ "$STAGED_SYMLINK_COUNT" = "0" ] || die "staged NAD fuel must not contain symbolic links"

echo "validating staged NAD counties"
STAGED_COUNTY_COUNT="$(validate_nad_tree "$STAGED_DATA_DIR" "$STAGED_MANIFEST_FILE")"
[ "$STAGED_COUNTY_COUNT" = "$COUNTY_COUNT" ] || die "staged NAD county count does not match source"

cd "$STAGING_ROOT"
COPYFILE_DISABLE=1 tar \
  --no-xattrs \
  --exclude='.DS_Store' \
  -cf - \
  "$DATA_PATH" \
  "$MANIFEST_PATH" \
  | zstd -T0 -19 -o "${TMP_DIR}/${ARCHIVE}"

ARCHIVE_SHA256="$(shasum -a 256 "${TMP_DIR}/${ARCHIVE}" | awk '{print $1}')"
ARCHIVE_SIZE_BYTES="$(wc -c < "${TMP_DIR}/${ARCHIVE}" | tr -d '[:space:]')"
CREATED_UTC="$(date -u '+%Y-%m-%dT%H:%M:%SZ')"

{
  printf '{\n'
  json_string_field "artifact_type" "addr-nad-fuel"
  json_number_field "schema_version" "2"
  json_number_field "nad_revision" "$REVISION"
  json_string_field "addr_package_version" "$ADDR_PACKAGE_VERSION"
  json_string_field "addr_package_version_required" "$ADDR_PACKAGE_VERSION"
  json_string_field "county_file_format" "parquet"
  json_string_field "dataset_partitioning" "hive"
  json_string_field "state_partition_field" "state"
  json_string_field "county_partition_field" "county_fips"
  json_string_field "archive_file" "$ARCHIVE"
  json_string_field "archive_sha256" "$ARCHIVE_SHA256"
  json_number_field "archive_size_bytes" "$ARCHIVE_SIZE_BYTES"
  json_string_field "created_utc" "$CREATED_UTC"
  json_string_field "data_path" "$DATA_PATH"
  json_string_field "manifest_path" "$MANIFEST_PATH"
  json_string_field "required_manifest_file" "$REQUIRED_MANIFEST_FILE"
  json_number_field "county_count" "$COUNTY_COUNT"
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
