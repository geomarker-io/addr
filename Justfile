set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

default:
    @just --list

# Build the local addr runtime image.
build:
    container build -f "$PWD/Containerfile" -t addr:local .

# Run local CLI tests outside R CMD check.
test-cli:
    Rscript cli-tests/test-addr-geocode.R

# Build the local image and smoke-test the container entrypoint.
test-container: build
    @tmp="$(mktemp -d)" ; \
    trap 'rm -rf "$tmp"' EXIT ; \
    mkdir -p "$tmp/work" "$tmp/data" "$tmp/tmp" ; \
    TMP_WORK="$tmp/work" Rscript -e 'write.csv(data.frame(id = 1:2, address = c(NA_character_, NA_character_)), file = file.path(Sys.getenv("TMP_WORK"), "addresses.csv"), row.names = FALSE, na = "")' ; \
    container run --rm -v "$tmp/work:/work" -v "$tmp/data:/opt/addr-data" -v "$tmp/tmp:/tmp" addr:local addr-geocode --input /work/addresses.csv ; \
    version="$(Rscript -e 'cat(read.dcf("DESCRIPTION", "Version"))')" ; \
    TMP_WORK="$tmp/work" ADDR_VERSION="$version" Rscript -e 'out <- file.path(Sys.getenv("TMP_WORK"), sprintf("addresses__addr-v%s__geocoded.csv", Sys.getenv("ADDR_VERSION"))); stopifnot(file.exists(out)); d <- read.csv(out, check.names = FALSE); stopifnot(nrow(d) == 2L); stopifnot(all(c("addr_geocode_stage", "addr_matched_zipcode", "addr_matched_street", "addr_longitude", "addr_latitude", "addr_s2_cell") %in% names(d))); print(d)'

# Run the local addr image with its default command.
run:
    @addr_data_dir="$(Rscript -e 'cat(tools::R_user_dir("addr", "data"))')" ; \
    mount_args=() ; \
    if [ -d "$addr_data_dir" ]; then \
        echo "mounting addr data directory: $addr_data_dir -> /opt/addr-data/R/addr" ; \
        mount_args=(-v "$addr_data_dir:/opt/addr-data/R/addr") ; \
    else \
        echo "addr data directory does not exist locally, running without data mount: $addr_data_dir" ; \
    fi ; \
    exec container run --rm -it "${mount_args[@]}" addr:local
