set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

container_runtime := env_var_or_default("CONTAINER_RUNTIME", "docker")
image := env_var_or_default("ADDR_IMAGE", "addr:local")
containerfile := env_var_or_default("ADDR_CONTAINERFILE", "Containerfile")

default:
    @just --list

# Build the local addr runtime image.
build:
    {{container_runtime}} build -f {{containerfile}} -t {{image}} .

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
    exec {{container_runtime}} run --rm -it "${mount_args[@]}" {{image}}
