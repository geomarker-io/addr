set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

default:
    @just --list

# Work around Apple container 1.0.0 context handling for this checkout; direct
# root-context builds hit .git/fsmonitor--daemon.ipc even with ignore files.

# Build the local addr runtime image.
build:
    @build_context="$(mktemp -d /tmp/addr-container-build.XXXXXX)" ; \
    trap 'rm -rf "$build_context"' EXIT ; \
    COPYFILE_DISABLE=1 tar --no-xattrs -czf "$build_context/addr-source.tar.gz" \
        --exclude='.git' \
        --exclude='.agents' \
        --exclude='.codex' \
        --exclude='.github' \
        --exclude='.dockerignore' \
        --exclude='.DS_Store' \
        --exclude='.RData' \
        --exclude='.Rhistory' \
        --exclude='.Rproj.user' \
        --exclude='.Ruserdata' \
        --exclude='*.Rcheck' \
        --exclude='*.tar.gz' \
        --exclude='README.Rmd' \
        --exclude='data-raw' \
        --exclude='diagrams' \
        --exclude='docs' \
        --exclude='inst.csv' \
        --exclude='inst/doc' \
        --exclude='pkgdown' \
        --exclude='src/.cargo' \
        --exclude='src/addr.so' \
        --exclude='src/entrypoint.o' \
        --exclude='src/rust/target' \
        --exclude='target' \
        --exclude='NAD_r*.zip' \
        --exclude='NAD_r*.gdb' \
        --exclude='inst/CAGISOpenDataSpring2024.gdb' \
        -C . . ; \
    awk ' \
        $0 == "COPY . /tmp/addr" { \
            print "COPY addr-source.tar.gz /tmp/build/addr-source.tar.gz" ; \
            print "RUN mkdir -p /tmp/addr && tar -xzf /tmp/build/addr-source.tar.gz -C /tmp/addr && rm /tmp/build/addr-source.tar.gz" ; \
            next ; \
        } \
        { print } \
    ' Containerfile > "$build_context/Containerfile" ; \
    container build -f "$build_context/Containerfile" -t addr:local "$build_context"

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
