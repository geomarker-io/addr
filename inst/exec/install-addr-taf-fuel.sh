#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat >&2 <<'EOF'
usage: install-addr-taf-fuel.sh ARCHIVE.tar.zst [METADATA.json [ARCHIVE.tar.zst.sha256]]

Installs packaged addr TAF fuel into tools::R_user_dir("addr", "data").
Set R_USER_DATA_DIR before running this script to install somewhere else.
The installer refuses to overwrite existing TAF fuel; delete existing year
directories first if replacing an installed artifact.
EOF
}

die() {
  echo "install-addr-taf-fuel: $*" >&2
  if [ -n "${README_FILE:-}" ]; then
    echo "See the generated README sidecar for install and replacement details: ${README_FILE}" >&2
  else
    echo "See the generated .README.md sidecar for install and replacement details." >&2
  fi
  exit 1
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

abs_path() {
  local path="$1"
  local dir
  local base
  dir="$(cd "$(dirname "$path")" && pwd)"
  base="$(basename "$path")"
  printf '%s/%s' "$dir" "$base"
}

json_get() {
  local key="$1"
  awk -v key="$key" '
    $0 ~ "^[[:space:]]*\"" key "\"[[:space:]]*:" {
      line = $0
      sub("^[^\"]*\"" key "\"[[:space:]]*:[[:space:]]*", "", line)
      if (line ~ /^"/) {
        sub(/^"/, "", line)
        sub(/"[[:space:]]*,?[[:space:]]*$/, "", line)
      } else {
        sub(/[[:space:]]*,?[[:space:]]*$/, "", line)
      }
      print line
      found = 1
      exit
    }
    END {
      if (!found) {
        exit 1
      }
    }
  ' "$JSON_FILE"
}

json_required() {
  local key="$1"
  local value
  if ! value="$(json_get "$key")" || [ -z "$value" ]; then
    die "metadata missing required field: ${key}"
  fi
  printf '%s' "$value"
}

validate_relative_path() {
  local path="$1"
  local label="$2"
  local part
  local trimmed="${path%/}"

  [ -n "$trimmed" ] || die "${label} is empty"
  case "$trimmed" in
    /*) die "${label} must be relative: ${path}" ;;
  esac

  IFS='/' read -r -a parts <<< "$trimmed"
  for part in "${parts[@]}"; do
    if [ -z "$part" ] || [ "$part" = ".." ]; then
      die "${label} contains an unsafe path segment: ${path}"
    fi
  done
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

if [ "$#" -lt 1 ] || [ "$#" -gt 3 ]; then
  usage
  exit 1
fi

ARCHIVE="$1"
DEFAULT_JSON="${ARCHIVE%.tar.zst}.json"
DEFAULT_SHA256="${ARCHIVE}.sha256"

if [ "$DEFAULT_JSON" = "$ARCHIVE" ]; then
  die "archive must end in .tar.zst: ${ARCHIVE}"
fi

if [ "$#" -eq 2 ] && [ "${2##*.}" = "sha256" ]; then
  JSON_FILE="$DEFAULT_JSON"
  SHA256_FILE="$2"
else
  JSON_FILE="${2:-$DEFAULT_JSON}"
  SHA256_FILE="${3:-$DEFAULT_SHA256}"
fi

README_FILE="${JSON_FILE%.json}.README.md"

require_command Rscript
require_command tar
require_command zstd
require_command shasum
require_command find
require_command wc
require_command awk
require_command mktemp

[ -f "$ARCHIVE" ] || die "archive not found: ${ARCHIVE}"
[ -f "$JSON_FILE" ] || die "metadata JSON not found: ${JSON_FILE}"
[ -f "$SHA256_FILE" ] || die "sha256 file not found: ${SHA256_FILE}"

ARCHIVE_ABS="$(abs_path "$ARCHIVE")"
ARCHIVE_DIR="$(dirname "$ARCHIVE_ABS")"
ARCHIVE_BASENAME="$(basename "$ARCHIVE_ABS")"
JSON_FILE="$(abs_path "$JSON_FILE")"
SHA256_FILE="$(abs_path "$SHA256_FILE")"
README_FILE="$(abs_path "$README_FILE")"

ARTIFACT_TYPE="$(json_required artifact_type)"
SCHEMA_VERSION="$(json_required schema_version)"
TAF_VERSION="$(json_required taf_version)"
TAF_YEAR="$(json_required taf_year)"
META_ARCHIVE_FILE="$(json_required archive_file)"
META_ARCHIVE_SHA256="$(json_required archive_sha256)"
META_ARCHIVE_SIZE_BYTES="$(json_required archive_size_bytes)"
META_ADDR_PACKAGE_VERSION_REQUIRED="$(json_required addr_package_version_required)"
META_DATA_PATH="$(json_required data_path)"
META_MANIFEST_PATH="$(json_required manifest_path)"
META_REQUIRED_MANIFEST_FILE="$(json_required required_manifest_file)"
META_DATA_FILE_COUNT="$(json_required data_file_count)"
META_MANIFEST_FILE_COUNT="$(json_required manifest_file_count)"
META_ARCHIVE_PARQUET_COMPRESSION="$(json_required archive_parquet_compression)"
META_ARCHIVE_PARQUET_COMPRESSION_LEVEL="$(json_required archive_parquet_compression_level)"
META_INSTALLED_PARQUET_COMPRESSION="$(json_required installed_parquet_compression)"

validate_unsigned_integer "$SCHEMA_VERSION" "metadata schema_version"
validate_unsigned_integer "$META_ARCHIVE_SIZE_BYTES" "metadata archive_size_bytes"
validate_unsigned_integer "$META_DATA_FILE_COUNT" "metadata data_file_count"
validate_unsigned_integer "$META_MANIFEST_FILE_COUNT" "metadata manifest_file_count"

[ "$ARTIFACT_TYPE" = "addr-taf-fuel" ] || die "unexpected artifact_type: ${ARTIFACT_TYPE}"
[ "$SCHEMA_VERSION" = "1" ] || die "unsupported schema_version: ${SCHEMA_VERSION}"
validate_unsigned_integer "$META_ARCHIVE_PARQUET_COMPRESSION_LEVEL" "metadata archive_parquet_compression_level"
[ "$META_ARCHIVE_PARQUET_COMPRESSION" = "zstd" ] || die "unsupported archive parquet compression: ${META_ARCHIVE_PARQUET_COMPRESSION}"
[ "$META_INSTALLED_PARQUET_COMPRESSION" = "snappy" ] || die "unsupported installed parquet compression: ${META_INSTALLED_PARQUET_COMPRESSION}"
[ "$META_ARCHIVE_FILE" = "$ARCHIVE_BASENAME" ] || die "metadata archive_file does not match archive: ${META_ARCHIVE_FILE}"

INSTALLED_ADDR_PACKAGE_VERSION="$(
  Rscript -e 'cat(as.character(utils::packageVersion("addr")))'
)"
[ "$INSTALLED_ADDR_PACKAGE_VERSION" = "$META_ADDR_PACKAGE_VERSION_REQUIRED" ] || die "addr package version ${INSTALLED_ADDR_PACKAGE_VERSION} is not compatible with this TAF fuel artifact; expected ${META_ADDR_PACKAGE_VERSION_REQUIRED}"

EXPECTED_DATA_PATH="${TAF_VERSION}/tiger_addr_feat/${TAF_YEAR}"
EXPECTED_MANIFEST_PATH="${TAF_VERSION}/tiger_addr_feat_manifest/${TAF_YEAR}"
EXPECTED_REQUIRED_MANIFEST_FILE="${EXPECTED_MANIFEST_PATH}/county_zip.parquet"

[ "$META_DATA_PATH" = "$EXPECTED_DATA_PATH" ] || die "metadata data_path does not match taf version/year"
[ "$META_MANIFEST_PATH" = "$EXPECTED_MANIFEST_PATH" ] || die "metadata manifest_path does not match taf version/year"
[ "$META_REQUIRED_MANIFEST_FILE" = "$EXPECTED_REQUIRED_MANIFEST_FILE" ] || die "metadata required_manifest_file is not expected county_zip.parquet path"

validate_relative_path "$META_DATA_PATH" "metadata data_path"
validate_relative_path "$META_MANIFEST_PATH" "metadata manifest_path"
validate_relative_path "$META_REQUIRED_MANIFEST_FILE" "metadata required_manifest_file"

ACTUAL_ARCHIVE_SHA256="$(shasum -a 256 "$ARCHIVE_ABS" | awk '{print $1}')"
[ "$ACTUAL_ARCHIVE_SHA256" = "$META_ARCHIVE_SHA256" ] || die "archive sha256 does not match metadata"

ACTUAL_ARCHIVE_SIZE_BYTES="$(wc -c < "$ARCHIVE_ABS" | tr -d '[:space:]')"
[ "$ACTUAL_ARCHIVE_SIZE_BYTES" = "$META_ARCHIVE_SIZE_BYTES" ] || die "archive size does not match metadata"

(
  cd "$ARCHIVE_DIR"
  shasum -a 256 -c "$SHA256_FILE"
) || die "sha256 validation failed"

ADDR_DATA_DIR="$(Rscript -e 'cat(tools::R_user_dir("addr", "data"))')"
TAF_DATA_DIR="${ADDR_DATA_DIR}/${META_DATA_PATH}"
TAF_MANIFEST_DIR="${ADDR_DATA_DIR}/${META_MANIFEST_PATH}"

existing_paths=()
for path in "$TAF_DATA_DIR" "$TAF_MANIFEST_DIR"; do
  if [ -e "$path" ] || [ -L "$path" ]; then
    existing_paths+=("$path")
  fi
done

if [ "${#existing_paths[@]}" -gt 0 ]; then
  {
    echo "install-addr-taf-fuel: addr TAF fuel already exists at:"
    printf '  %s\n' "${existing_paths[@]}"
    echo
    echo "The installer refuses to overwrite existing TAF fuel."
    echo "Delete the existing path(s) first, then rerun this script."
    echo "To install somewhere else, set R_USER_DATA_DIR before running this script."
    echo "See the generated README sidecar for details: ${README_FILE}"
  } >&2
  exit 1
fi

STAGING_DIR="$(mktemp -d "${TMPDIR:-/tmp}/addr-taf-install.XXXXXX")"
cleanup() {
  rm -rf "$STAGING_DIR"
}
trap cleanup EXIT

MEMBERS_FILE="${STAGING_DIR}/archive-members.txt"
zstd -dc "$ARCHIVE_ABS" | tar -tf - > "$MEMBERS_FILE"

while IFS= read -r member; do
  member="${member%/}"
  validate_relative_path "$member" "archive member"
  case "$member" in
    "$META_DATA_PATH"|"$META_DATA_PATH"/*|"$META_MANIFEST_PATH"|"$META_MANIFEST_PATH"/*)
      ;;
    *)
      die "archive contains unexpected member: ${member}"
      ;;
  esac
done < "$MEMBERS_FILE"

zstd -dc "$ARCHIVE_ABS" | tar -C "$STAGING_DIR" -xf -

STAGED_DATA_DIR="${STAGING_DIR}/${META_DATA_PATH}"
STAGED_MANIFEST_DIR="${STAGING_DIR}/${META_MANIFEST_PATH}"
STAGED_REQUIRED_MANIFEST_FILE="${STAGING_DIR}/${META_REQUIRED_MANIFEST_FILE}"

[ -d "$STAGED_DATA_DIR" ] || die "staged data directory missing: ${META_DATA_PATH}"
[ -d "$STAGED_MANIFEST_DIR" ] || die "staged manifest directory missing: ${META_MANIFEST_PATH}"
[ -f "$STAGED_REQUIRED_MANIFEST_FILE" ] || die "staged required manifest file missing: ${META_REQUIRED_MANIFEST_FILE}"

STAGED_DATA_FILE_COUNT="$(count_files "$STAGED_DATA_DIR")"
STAGED_MANIFEST_FILE_COUNT="$(count_files "$STAGED_MANIFEST_DIR")"

[ "$STAGED_DATA_FILE_COUNT" = "$META_DATA_FILE_COUNT" ] || die "staged data file count does not match metadata"
[ "$STAGED_MANIFEST_FILE_COUNT" = "$META_MANIFEST_FILE_COUNT" ] || die "staged manifest file count does not match metadata"

echo "transcoding installed parquet to ${META_INSTALLED_PARQUET_COMPRESSION}"
ADDR_TAF_INSTALL_DATA_DIR="$STAGED_DATA_DIR" \
  ADDR_TAF_INSTALL_MANIFEST_DIR="$STAGED_MANIFEST_DIR" \
  ADDR_TAF_INSTALL_COMPRESSION="$META_INSTALLED_PARQUET_COMPRESSION" \
  Rscript - <<'RSCRIPT'
roots <- c(
  Sys.getenv("ADDR_TAF_INSTALL_DATA_DIR"),
  Sys.getenv("ADDR_TAF_INSTALL_MANIFEST_DIR")
)
compression <- Sys.getenv("ADDR_TAF_INSTALL_COMPRESSION")

if (!requireNamespace("nanoparquet", quietly = TRUE)) {
  stop("nanoparquet is required to install TAF fuel", call. = FALSE)
}

files <- unlist(lapply(roots, function(root) {
  list.files(
    root,
    pattern = "[.]parquet$",
    all.files = TRUE,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = FALSE,
    ignore.case = TRUE
  )
}), use.names = FALSE)

for (i in seq_along(files)) {
  source_file <- files[[i]]
  temporary_file <- paste0(source_file, ".installing")

  value <- nanoparquet::read_parquet(source_file)
  nanoparquet::write_parquet(
    value,
    temporary_file,
    compression = compression
  )
  unlink(source_file, force = TRUE)
  if (!file.rename(temporary_file, source_file)) {
    stop("could not replace staged parquet file: ", source_file, call. = FALSE)
  }

  if (i %% 1000L == 0L || i == length(files)) {
    message("transcoded ", i, " of ", length(files), " files")
  }
}
RSCRIPT

mkdir -p "$(dirname "$TAF_DATA_DIR")"
mkdir -p "$(dirname "$TAF_MANIFEST_DIR")"

mv "$STAGED_DATA_DIR" "$TAF_DATA_DIR"
mv "$STAGED_MANIFEST_DIR" "$TAF_MANIFEST_DIR"

echo "installed addr TAF fuel under: $ADDR_DATA_DIR"
echo "installed data: $TAF_DATA_DIR"
echo "installed manifest: $TAF_MANIFEST_DIR"

ADDR_TAF_YEAR="$TAF_YEAR" ADDR_TAF_VERSION="$TAF_VERSION" Rscript -e '
year <- Sys.getenv("ADDR_TAF_YEAR")
version <- Sys.getenv("ADDR_TAF_VERSION")
stopifnot(nrow(addr::taf_zip("45220", year = year, version = version)) > 0)
message("taf_zip verification passed")
'

ADDR_TAF_YEAR="$TAF_YEAR" ADDR_TAF_VERSION="$TAF_VERSION" Rscript -e '
year <- Sys.getenv("ADDR_TAF_YEAR")
version <- Sys.getenv("ADDR_TAF_VERSION")
addr::geocode(
  addr::as_addr("3333 Burnet Ave Cincinnati OH 45229"),
  year = year,
  version = version,
  taf_install = FALSE
)
message("geocode verification passed")
'
