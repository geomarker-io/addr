set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

default:
    @just --list

# Build the local addr runtime image.
build:
    container build -f "$PWD/Containerfile" -t addr:local .

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
