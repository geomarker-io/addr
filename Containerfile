ARG R_VERSION=4.4.3
ARG RUST_VERSION=1.95.0

FROM ghcr.io/rocker-org/r-ver:${R_VERSION} AS builder

ARG DEBIAN_FRONTEND=noninteractive
ARG RUST_VERSION

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        build-essential \
        libgdal-dev \
        libgeos-dev \
        libproj-dev \
        libssl-dev \
        libudunits2-dev \
        pkg-config \
    && rm -rf /var/lib/apt/lists/*

ENV CARGO_HOME=/usr/local/cargo
ENV RUSTUP_HOME=/usr/local/rustup
ENV PATH=/usr/local/cargo/bin:$PATH

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --profile minimal --default-toolchain "${RUST_VERSION}" \
    && chmod -R a+w "${RUSTUP_HOME}" "${CARGO_HOME}"

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/manylinux_2_28/latest'))" \
    >> "${R_HOME}/etc/Rprofile.site"

RUN R -q -e "install.packages(c('curl', 'jsonlite', 'mirai', 'nanoparquet', 'wk', 'proxy', 'e1071', 'classInt', 'DBI', 's2', 'S7', 'stringdist', 'tibble', 'units', 'vctrs', 'Rcpp'), Ncpus = 1)" \
    && R -q -e "install.packages('sf', repos = c(CRAN = 'https://cloud.r-project.org'), type = 'source', dependencies = FALSE, Ncpus = 1)"

WORKDIR /tmp/addr
COPY DESCRIPTION LICENSE NAMESPACE configure configure.win cleanup cleanup.win ./
COPY R/ ./R/
COPY inst/ ./inst/
COPY man/ ./man/
COPY src/entrypoint.c src/Makevars src/Makevars.ucrt src/Makevars.win src/addr-win.def ./src/
COPY src/rust/Cargo.toml src/rust/Cargo.lock src/rust/cargo-vendor-config.toml src/rust/vendor.tar.xz ./src/rust/
COPY src/rust/src/ ./src/rust/src/

WORKDIR /tmp/build

RUN R CMD build /tmp/addr --no-manual --no-build-vignettes \
    && R CMD INSTALL --clean addr_*.tar.gz \
    && R -q -e "library(addr); library(sf); library(nanoparquet); library(mirai); library(jsonlite)" \
    && find /usr/local/lib/R/site-library -name '*.so' -exec strip --strip-unneeded {} + \
    && rm -rf /tmp/addr /tmp/build/addr_*.tar.gz

FROM ghcr.io/rocker-org/r-ver:${R_VERSION}

ARG DEBIAN_FRONTEND=noninteractive
ARG BUILD_DATE
ARG VCS_REF
ARG IMAGE_VERSION

LABEL org.opencontainers.image.title="addr" \
      org.opencontainers.image.description="R runtime with the addr package installed" \
      org.opencontainers.image.source="https://github.com/geomarker-io/addr" \
      org.opencontainers.image.url="https://github.com/geomarker-io/addr" \
      org.opencontainers.image.documentation="https://geomarker.io/addr/" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.created="${BUILD_DATE}" \
      org.opencontainers.image.revision="${VCS_REF}" \
      org.opencontainers.image.version="${IMAGE_VERSION}"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libgdal34t64 \
        libgeos-c1t64 \
        libproj25 \
        libudunits2-0 \
    && rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(CRAN = 'https://p3m.dev/cran/__linux__/manylinux_2_28/latest'))" \
    >> "${R_HOME}/etc/Rprofile.site"

COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

RUN Rscript -e 'file.symlink(system.file("exec", "addr-geocode", package = "addr"), "/usr/local/bin/addr-geocode")'

ENV R_USER_DATA_DIR=/opt/addr-data

RUN useradd --create-home --shell /bin/bash addr \
    && mkdir -p /opt/addr-data/R/addr \
    && chown -R addr:addr /opt/addr-data

USER addr
WORKDIR /home/addr

CMD ["R", "--quiet", "--no-save"]
