#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat >&2 <<'EOF'
usage: install-addr-nad-fuel.sh ARCHIVE.tar.zst [METADATA.json]

Installs packaged addr NAD county fuel into tools::R_user_dir("addr", "data").
Set R_USER_DATA_DIR before running this script to install somewhere else.
The installer refuses to overwrite either existing revision directory.
EOF
}

die() {
  echo "install-addr-nad-fuel: $*" >&2
  echo "See https://github.com/geomarker-io/addr#nad-fuel-bundle for installation details." >&2
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

validate_sha256() {
  local value="$1"
  local label="$2"
  if [ "${#value}" -ne 64 ]; then
    die "${label} must contain 64 lowercase hexadecimal characters"
  fi
  case "$value" in
    *[!0-9a-f]*) die "${label} must contain 64 lowercase hexadecimal characters" ;;
  esac
}

count_files() {
  find "$1" -type f ! -name '.DS_Store' | wc -l | tr -d '[:space:]'
}

if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
  usage
  exit 1
fi

ARCHIVE="$1"
DEFAULT_JSON="${ARCHIVE%.tar.zst}.json"
if [ "$DEFAULT_JSON" = "$ARCHIVE" ]; then
  die "archive must end in .tar.zst: ${ARCHIVE}"
fi
JSON_FILE="${2:-$DEFAULT_JSON}"

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

ARCHIVE_ABS="$(abs_path "$ARCHIVE")"
ARCHIVE_BASENAME="$(basename "$ARCHIVE_ABS")"
JSON_FILE="$(abs_path "$JSON_FILE")"

ARTIFACT_TYPE="$(json_required artifact_type)"
SCHEMA_VERSION="$(json_required schema_version)"
NAD_REVISION="$(json_required nad_revision)"
META_ADDR_PACKAGE_VERSION="$(json_required addr_package_version)"
META_ADDR_PACKAGE_VERSION_REQUIRED="$(json_required addr_package_version_required)"
META_COUNTY_FILE_FORMAT="$(json_required county_file_format)"
META_DATASET_PARTITIONING="$(json_required dataset_partitioning)"
META_STATE_PARTITION_FIELD="$(json_required state_partition_field)"
META_COUNTY_PARTITION_FIELD="$(json_required county_partition_field)"
META_ARCHIVE_FILE="$(json_required archive_file)"
META_ARCHIVE_SHA256="$(json_required archive_sha256)"
META_ARCHIVE_SIZE_BYTES="$(json_required archive_size_bytes)"
META_CREATED_UTC="$(json_required created_utc)"
META_DATA_PATH="$(json_required data_path)"
META_MANIFEST_PATH="$(json_required manifest_path)"
META_REQUIRED_MANIFEST_FILE="$(json_required required_manifest_file)"
META_COUNTY_COUNT="$(json_required county_count)"
META_DATA_FILE_COUNT="$(json_required data_file_count)"
META_MANIFEST_FILE_COUNT="$(json_required manifest_file_count)"

validate_unsigned_integer "$SCHEMA_VERSION" "metadata schema_version"
validate_unsigned_integer "$NAD_REVISION" "metadata nad_revision"
validate_unsigned_integer "$META_ARCHIVE_SIZE_BYTES" "metadata archive_size_bytes"
validate_unsigned_integer "$META_COUNTY_COUNT" "metadata county_count"
validate_unsigned_integer "$META_DATA_FILE_COUNT" "metadata data_file_count"
validate_unsigned_integer "$META_MANIFEST_FILE_COUNT" "metadata manifest_file_count"
validate_sha256 "$META_ARCHIVE_SHA256" "metadata archive_sha256"
if ! [[ "$META_CREATED_UTC" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$ ]]; then
  die "metadata created_utc must be a UTC ISO-8601 timestamp"
fi

[ "$ARTIFACT_TYPE" = "addr-nad-fuel" ] || die "unexpected artifact_type: ${ARTIFACT_TYPE}"
[ "$SCHEMA_VERSION" = "2" ] || die "unsupported schema_version: ${SCHEMA_VERSION}"
[ "$NAD_REVISION" = "23" ] || die "unsupported NAD revision: ${NAD_REVISION}"
[ "$META_ADDR_PACKAGE_VERSION" = "$META_ADDR_PACKAGE_VERSION_REQUIRED" ] || die "metadata package versions do not match"
[ "$META_COUNTY_FILE_FORMAT" = "parquet" ] || die "unsupported county_file_format: ${META_COUNTY_FILE_FORMAT}"
[ "$META_DATASET_PARTITIONING" = "hive" ] || die "unsupported dataset_partitioning: ${META_DATASET_PARTITIONING}"
[ "$META_STATE_PARTITION_FIELD" = "state" ] || die "unexpected state_partition_field: ${META_STATE_PARTITION_FIELD}"
[ "$META_COUNTY_PARTITION_FIELD" = "county_fips" ] || die "unexpected county_partition_field: ${META_COUNTY_PARTITION_FIELD}"
[ "$META_ARCHIVE_FILE" = "$ARCHIVE_BASENAME" ] || die "metadata archive_file does not match archive: ${META_ARCHIVE_FILE}"
[ "$META_COUNTY_COUNT" -gt 0 ] || die "metadata county_count must be greater than zero"
[ "$META_DATA_FILE_COUNT" = "$META_COUNTY_COUNT" ] || die "metadata data_file_count must match county_count"
[ "$META_MANIFEST_FILE_COUNT" = "1" ] || die "metadata manifest_file_count must be one"

INSTALLED_ADDR_PACKAGE_VERSION="$(
  Rscript -e 'cat(as.character(utils::packageVersion("addr")))'
)"
[ "$INSTALLED_ADDR_PACKAGE_VERSION" = "$META_ADDR_PACKAGE_VERSION_REQUIRED" ] || die "addr package version ${INSTALLED_ADDR_PACKAGE_VERSION} is not compatible with this NAD fuel artifact; expected ${META_ADDR_PACKAGE_VERSION_REQUIRED}"

EXPECTED_DATA_PATH="v2/nad/${NAD_REVISION}"
EXPECTED_MANIFEST_PATH="v2/nad_manifest/${NAD_REVISION}"
EXPECTED_REQUIRED_MANIFEST_FILE="${EXPECTED_MANIFEST_PATH}/counties.parquet"

[ "$META_DATA_PATH" = "$EXPECTED_DATA_PATH" ] || die "metadata data_path does not match NAD revision"
[ "$META_MANIFEST_PATH" = "$EXPECTED_MANIFEST_PATH" ] || die "metadata manifest_path does not match NAD revision"
[ "$META_REQUIRED_MANIFEST_FILE" = "$EXPECTED_REQUIRED_MANIFEST_FILE" ] || die "metadata required_manifest_file is not the expected counties.parquet path"

validate_relative_path "$META_DATA_PATH" "metadata data_path"
validate_relative_path "$META_MANIFEST_PATH" "metadata manifest_path"
validate_relative_path "$META_REQUIRED_MANIFEST_FILE" "metadata required_manifest_file"

ACTUAL_ARCHIVE_SHA256="$(shasum -a 256 "$ARCHIVE_ABS" | awk '{print $1}')"
[ "$ACTUAL_ARCHIVE_SHA256" = "$META_ARCHIVE_SHA256" ] || die "archive sha256 does not match metadata"
ACTUAL_ARCHIVE_SIZE_BYTES="$(wc -c < "$ARCHIVE_ABS" | tr -d '[:space:]')"
[ "$ACTUAL_ARCHIVE_SIZE_BYTES" = "$META_ARCHIVE_SIZE_BYTES" ] || die "archive size does not match metadata"

ADDR_DATA_DIR="$(Rscript -e 'cat(tools::R_user_dir("addr", "data"))')"
NAD_DATA_DIR="${ADDR_DATA_DIR}/${META_DATA_PATH}"
NAD_MANIFEST_DIR="${ADDR_DATA_DIR}/${META_MANIFEST_PATH}"

existing_paths=()
for path in "$NAD_DATA_DIR" "$NAD_MANIFEST_DIR"; do
  if [ -e "$path" ] || [ -L "$path" ]; then
    existing_paths+=("$path")
  fi
done
if [ "${#existing_paths[@]}" -gt 0 ]; then
  {
    echo "install-addr-nad-fuel: addr NAD fuel already exists at:"
    printf '  %s\n' "${existing_paths[@]}"
    echo
    echo "The installer refuses to overwrite existing NAD county data or its manifest."
    echo "Delete both revision directories first, then rerun this script."
    echo "To install somewhere else, set R_USER_DATA_DIR before running this script."
  } >&2
  exit 1
fi

STAGING_DIR="$(mktemp -d "${TMPDIR:-/tmp}/addr-nad-install.XXXXXX")"
cleanup() {
  rm -rf "$STAGING_DIR"
}
trap cleanup EXIT

MEMBERS_FILE="${STAGING_DIR}/archive-members.txt"
MEMBER_TYPES_FILE="${STAGING_DIR}/archive-member-types.txt"
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

zstd -dc "$ARCHIVE_ABS" | tar -tvf - > "$MEMBER_TYPES_FILE"
if awk '
  substr($0, 1, 1) != "-" && substr($0, 1, 1) != "d" {
    invalid = 1
  }
  END { exit invalid ? 0 : 1 }
' "$MEMBER_TYPES_FILE"; then
  die "archive contains a symbolic link or another unsupported member type"
fi

zstd -dc "$ARCHIVE_ABS" | tar -C "$STAGING_DIR" -xf -

STAGED_DATA_DIR="${STAGING_DIR}/${META_DATA_PATH}"
STAGED_MANIFEST_DIR="${STAGING_DIR}/${META_MANIFEST_PATH}"
STAGED_MANIFEST_FILE="${STAGING_DIR}/${META_REQUIRED_MANIFEST_FILE}"
[ -d "$STAGED_DATA_DIR" ] || die "staged data directory missing: ${META_DATA_PATH}"
[ -d "$STAGED_MANIFEST_DIR" ] || die "staged manifest directory missing: ${META_MANIFEST_PATH}"
[ -f "$STAGED_MANIFEST_FILE" ] || die "staged required manifest file missing: ${META_REQUIRED_MANIFEST_FILE}"

STAGED_SYMLINK_COUNT="$(find "$STAGED_DATA_DIR" "$STAGED_MANIFEST_DIR" -type l | wc -l | tr -d '[:space:]')"
[ "$STAGED_SYMLINK_COUNT" = "0" ] || die "staged NAD fuel must not contain symbolic links"
STAGED_DATA_FILE_COUNT="$(count_files "$STAGED_DATA_DIR")"
STAGED_MANIFEST_FILE_COUNT="$(count_files "$STAGED_MANIFEST_DIR")"
[ "$STAGED_DATA_FILE_COUNT" = "$META_DATA_FILE_COUNT" ] || die "staged data file count does not match metadata"
[ "$STAGED_MANIFEST_FILE_COUNT" = "$META_MANIFEST_FILE_COUNT" ] || die "staged manifest file count does not match metadata"

echo "validating staged NAD county manifest and Parquet files"
ADDR_NAD_INSTALL_DATA_DIR="$STAGED_DATA_DIR" \
  ADDR_NAD_INSTALL_MANIFEST_FILE="$STAGED_MANIFEST_FILE" \
  ADDR_NAD_INSTALL_REVISION="$NAD_REVISION" \
  ADDR_NAD_INSTALL_COUNTY_COUNT="$META_COUNTY_COUNT" \
  Rscript - <<'RSCRIPT'
data_dir <- Sys.getenv("ADDR_NAD_INSTALL_DATA_DIR")
manifest_file <- Sys.getenv("ADDR_NAD_INSTALL_MANIFEST_FILE")
revision <- as.integer(Sys.getenv("ADDR_NAD_INSTALL_REVISION"))
expected_count <- as.integer(Sys.getenv("ADDR_NAD_INSTALL_COUNTY_COUNT"))

manifest <- nanoparquet::read_parquet(manifest_file) |>
  tibble::as_tibble()
if (nrow(manifest) != expected_count) {
  stop("staged NAD manifest row count does not match metadata", call. = FALSE)
}
validator <- getFromNamespace("nad_validate_manifest", "addr")
validator(
  manifest,
  data_root = data_dir,
  version = revision,
  verify_files = TRUE
)
RSCRIPT

# Recheck immediately before installation. The manifest moves first so that a
# failed second move cannot expose county Parquet files without their inventory.
for path in "$NAD_DATA_DIR" "$NAD_MANIFEST_DIR"; do
  if [ -e "$path" ] || [ -L "$path" ]; then
    die "destination appeared during validation: ${path}"
  fi
done
mkdir -p "$(dirname "$NAD_DATA_DIR")"
mkdir -p "$(dirname "$NAD_MANIFEST_DIR")"

mv "$STAGED_MANIFEST_DIR" "$NAD_MANIFEST_DIR"
if ! mv "$STAGED_DATA_DIR" "$NAD_DATA_DIR"; then
  if ! mv "$NAD_MANIFEST_DIR" "$STAGED_MANIFEST_DIR"; then
    die "could not install NAD data and could not roll back its manifest"
  fi
  die "could not install NAD data; manifest move was rolled back"
fi

echo "installed addr NAD fuel under: $ADDR_DATA_DIR"
echo "installed data: $NAD_DATA_DIR"
echo "installed manifest: $NAD_MANIFEST_DIR"

ADDR_NAD_INSTALL_REVISION="$NAD_REVISION" \
  ADDR_NAD_INSTALL_COUNTY_COUNT="$META_COUNTY_COUNT" \
  Rscript - <<'RSCRIPT'
revision <- as.integer(Sys.getenv("ADDR_NAD_INSTALL_REVISION"))
expected_count <- as.integer(Sys.getenv("ADDR_NAD_INSTALL_COUNTY_COUNT"))
manifest <- addr::nad_manifest(version = revision, validate = TRUE)
stopifnot(nrow(manifest) == expected_count)
message("NAD fuel verification passed for ", expected_count, " counties")
RSCRIPT
