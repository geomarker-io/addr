devtools::load_all()

cnty_fips <- county_fips_reference$county_fips
taf_years <- 2025L
taf_version <- "v2"
release_asset_dir <- Sys.getenv(
  "ADDR_TAF_RELEASE_DIR",
  unset = getwd()
)
addr_package_version <- unname(
  read.dcf("DESCRIPTION", fields = "Version")[[1L]]
)

# Census publishes FEATNAMES but not ADDRFEAT files for these county
# equivalents in the 2025 TIGER release.
addrfeat_unavailable <- c(
  "60010",
  "60020",
  "60030",
  "60040",
  "60050",
  "69085",
  "69100",
  "69120"
)

pack_taf_fuel <- function(
  year,
  version,
  out_dir,
  package_version,
  data_root = tools::R_user_dir("addr", "data")
) {
  archive_parquet_compression <- "zstd"
  archive_parquet_compression_level <- 9L
  installed_parquet_compression <- "snappy"

  zstd <- Sys.which("zstd")
  if (!nzchar(zstd)) {
    stop("zstd is required to package TAF fuel", call. = FALSE)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = TRUE)
  base <- sprintf("addr-taf-%s-%s", version, year)
  archive_name <- paste0(base, ".tar.zst")
  json_name <- paste0(base, ".json")
  archive_path <- file.path(out_dir, archive_name)
  json_path <- file.path(out_dir, json_name)
  output_paths <- c(archive_path, json_path)
  output_links <- Sys.readlink(output_paths)
  output_links[is.na(output_links)] <- ""
  output_exists <- file.exists(output_paths) | nzchar(output_links)
  if (any(output_exists)) {
    stop(
      "TAF release output already exists: ",
      output_paths[which(output_exists)[[1L]]],
      call. = FALSE
    )
  }

  data_path <- file.path(version, "tiger_addr_feat", year)
  manifest_path <- file.path(version, "tiger_addr_feat_manifest", year)
  required_manifest_file <- file.path(manifest_path, "county_zip.parquet")
  data_dir <- file.path(data_root, data_path)
  manifest_dir <- file.path(data_root, manifest_path)
  manifest_file <- file.path(data_root, required_manifest_file)
  if (!dir.exists(data_dir)) {
    stop("missing TAF data directory: ", data_dir, call. = FALSE)
  }
  if (!dir.exists(manifest_dir)) {
    stop("missing TAF manifest directory: ", manifest_dir, call. = FALSE)
  }
  if (!file.exists(manifest_file)) {
    stop("missing required TAF manifest: ", manifest_file, call. = FALSE)
  }

  tree_files <- function(path) {
    files <- list.files(
      path,
      all.files = TRUE,
      no.. = TRUE,
      recursive = TRUE,
      full.names = TRUE,
      include.dirs = FALSE
    )
    files[basename(files) != ".DS_Store"]
  }
  tree_symlinks <- function(paths) {
    paths <- unique(c(
      paths,
      unlist(
        lapply(paths[dir.exists(paths)], function(path) {
          list.files(
            path,
            all.files = TRUE,
            no.. = TRUE,
            recursive = TRUE,
            full.names = TRUE,
            include.dirs = TRUE
          )
        }),
        use.names = FALSE
      )
    ))
    links <- Sys.readlink(paths)
    links[is.na(links)] <- ""
    paths[nzchar(links)]
  }

  message("validating installed TAF files against the local manifest")
  manifest <- nanoparquet::read_parquet(manifest_file) |>
    tibble::as_tibble()
  taf_validate_manifest(
    manifest,
    data_root = data_dir,
    year = year,
    version = version,
    verify_files = TRUE
  )
  manifest_row_count <- nrow(manifest)
  if (manifest_row_count == 0L) {
    stop("TAF manifest contains no installed files", call. = FALSE)
  }

  data_files <- tree_files(data_dir)
  manifest_files <- tree_files(manifest_dir)
  if (length(data_files) != manifest_row_count) {
    stop("TAF data file count does not match manifest rows", call. = FALSE)
  }
  if (length(manifest_files) != 1L) {
    stop(
      "TAF manifest directory must contain only county_zip.parquet",
      call. = FALSE
    )
  }
  if (length(tree_symlinks(c(data_dir, manifest_dir))) > 0L) {
    stop(
      "TAF fuel source directories must not contain symbolic links",
      call. = FALSE
    )
  }

  temporary_dir <- tempfile(pattern = ".addr-taf-pack.", tmpdir = out_dir)
  dir.create(temporary_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temporary_dir, recursive = TRUE, force = TRUE), add = TRUE)
  staging_root <- file.path(temporary_dir, "staging")
  dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)

  message(
    "transcoding TAF parquet for distribution (",
    archive_parquet_compression,
    " level ",
    archive_parquet_compression_level,
    ")"
  )
  relative_roots <- c(data_path, manifest_path)
  files_total <- length(data_files) + length(manifest_files)
  files_done <- 0L
  parquet_options <- nanoparquet::parquet_options(
    compression_level = archive_parquet_compression_level
  )
  for (relative_root in relative_roots) {
    source_root <- file.path(data_root, relative_root)
    relative_files <- list.files(
      source_root,
      all.files = TRUE,
      no.. = TRUE,
      recursive = TRUE,
      full.names = FALSE,
      include.dirs = FALSE
    )
    relative_files <- relative_files[basename(relative_files) != ".DS_Store"]
    for (relative_file in relative_files) {
      source_file <- file.path(source_root, relative_file)
      staging_file <- file.path(staging_root, relative_root, relative_file)
      dir.create(dirname(staging_file), recursive = TRUE, showWarnings = FALSE)
      if (grepl("[.]parquet$", source_file, ignore.case = TRUE)) {
        value <- nanoparquet::read_parquet(source_file)
        nanoparquet::write_parquet(
          value,
          staging_file,
          compression = archive_parquet_compression,
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

  staged_manifest_file <- file.path(staging_root, required_manifest_file)
  staged_data_dir <- file.path(staging_root, data_path)
  staged_manifest_dir <- file.path(staging_root, manifest_path)
  staged_manifest <- nanoparquet::read_parquet(staged_manifest_file) |>
    tibble::as_tibble()
  staged_manifest <- taf_refresh_manifest_file_metadata(
    staged_manifest,
    data_root = staged_data_dir,
    year = year,
    version = version
  )
  temporary_manifest <- paste0(staged_manifest_file, ".refreshing")
  nanoparquet::write_parquet(
    staged_manifest,
    temporary_manifest,
    compression = archive_parquet_compression,
    options = parquet_options
  )
  unlink(staged_manifest_file, force = TRUE)
  if (!file.rename(temporary_manifest, staged_manifest_file)) {
    stop("could not replace staged TAF manifest", call. = FALSE)
  }

  staged_data_files <- tree_files(staged_data_dir)
  staged_manifest_files <- tree_files(staged_manifest_dir)
  if (length(staged_data_files) != length(data_files)) {
    stop("staged TAF data file count does not match source", call. = FALSE)
  }
  if (length(staged_manifest_files) != length(manifest_files)) {
    stop("staged TAF manifest file count does not match source", call. = FALSE)
  }
  if (length(tree_symlinks(c(staged_data_dir, staged_manifest_dir))) > 0L) {
    stop("staged TAF fuel must not contain symbolic links", call. = FALSE)
  }
  message("validating staged TAF files")
  staged_manifest <- nanoparquet::read_parquet(staged_manifest_file) |>
    tibble::as_tibble()
  taf_validate_manifest(
    staged_manifest,
    data_root = staged_data_dir,
    year = year,
    version = version,
    verify_files = TRUE
  )
  if (nrow(staged_manifest) != manifest_row_count) {
    stop("staged TAF manifest row count does not match source", call. = FALSE)
  }

  temporary_archive <- file.path(temporary_dir, archive_name)
  old_wd <- setwd(staging_root)
  on.exit(setwd(old_wd), add = TRUE)
  zstd_command <- paste(
    shQuote(zstd),
    "-T0 -19 --force -o",
    shQuote(temporary_archive)
  )
  archive_connection <- pipe(zstd_command, open = "wb")
  tar_status <- tryCatch(
    utils::tar(
      tarfile = archive_connection,
      files = relative_roots,
      compression = "none",
      tar = "internal"
    ),
    finally = close(archive_connection)
  )
  setwd(old_wd)
  if (!identical(as.integer(tar_status), 0L)) {
    stop("could not create TAF release archive", call. = FALSE)
  }
  if (
    !file.exists(temporary_archive) || file.info(temporary_archive)$size <= 0
  ) {
    stop("TAF release archive was not created", call. = FALSE)
  }

  json_escape <- function(value) {
    value <- enc2utf8(as.character(value))
    value <- gsub("\\", "\\\\", value, fixed = TRUE)
    value <- gsub("\"", "\\\"", value, fixed = TRUE)
    value <- gsub("\n", "\\n", value, fixed = TRUE)
    value <- gsub("\r", "\\r", value, fixed = TRUE)
    value <- gsub("\t", "\\t", value, fixed = TRUE)
    value
  }
  json_value <- function(value) {
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
      return(paste0("\"", json_escape(value), "\""))
    }
    if (is.numeric(value) && length(value) == 1L && is.finite(value)) {
      return(format(value, scientific = FALSE, trim = TRUE))
    }
    stop("unsupported TAF metadata value", call. = FALSE)
  }

  archive_sha256 <- digest::digest(
    algo = "sha256",
    serialize = FALSE,
    file = temporary_archive
  )
  archive_size_bytes <- unname(file.info(temporary_archive)$size)
  created_utc <- paste0(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "Z"
  )
  metadata <- list(
    artifact_type = "addr-taf-fuel",
    schema_version = 2L,
    taf_version = version,
    taf_year = year,
    addr_package_version = package_version,
    addr_package_version_required = package_version,
    archive_file = archive_name,
    archive_sha256 = archive_sha256,
    archive_size_bytes = archive_size_bytes,
    archive_parquet_compression = archive_parquet_compression,
    archive_parquet_compression_level = archive_parquet_compression_level,
    installed_parquet_compression = installed_parquet_compression,
    created_utc = created_utc,
    data_path = data_path,
    manifest_path = manifest_path,
    required_manifest_file = required_manifest_file,
    manifest_row_count = manifest_row_count,
    data_file_count = length(data_files),
    manifest_file_count = length(manifest_files)
  )
  metadata_lines <- vapply(
    seq_along(metadata),
    function(i) {
      comma <- if (i < length(metadata)) "," else ""
      paste0(
        "  \"",
        json_escape(names(metadata)[[i]]),
        "\": ",
        json_value(metadata[[i]]),
        comma
      )
    },
    character(1)
  )
  temporary_json <- file.path(temporary_dir, json_name)
  writeLines(c("{", metadata_lines, "}"), temporary_json, useBytes = TRUE)

  if (!file.rename(temporary_archive, archive_path)) {
    stop("could not move TAF archive into output directory", call. = FALSE)
  }
  if (!file.rename(temporary_json, json_path)) {
    unlink(archive_path, force = TRUE)
    stop("could not move TAF metadata into output directory", call. = FALSE)
  }
  message("wrote: ", archive_path)
  message("wrote: ", json_path)
  invisible(c(archive = archive_path, metadata = json_path))
}

# Census recommends anonymous FTP for a large number of TIGER files. This
# protocol is intentionally selected only for this bulk development script;
# normal package use remains on encrypted HTTPS.
options(
  nwarnings = 10000,
  addr.tiger_download_protocol = "ftp",
  addr.tiger_download_interval = 1
)

for (yr in as.character(taf_years)) {
  available_fips <- setdiff(cnty_fips, addrfeat_unavailable)

  for (i in seq_along(available_fips)) {
    county <- available_fips[[i]]
    message(sprintf(
      "%s: installing TAF county %s (%d/%d)",
      yr,
      county,
      i,
      length(available_fips)
    ))
    addr::taf_install(
      county,
      year = yr,
      version = taf_version,
      overwrite = TRUE,
      redownload = FALSE
    )
  }

  manifest <- addr::taf_manifest(
    year = yr,
    version = taf_version,
    validate = TRUE
  )
  stopifnot(
    "TAF manifest must contain every county with published ADDRFEAT data" = setequal(
      unique(manifest$county_fips),
      available_fips
    )
  )
  taf_write_catalog(manifest, year = yr, version = taf_version)
  catalog <- nanoparquet::read_parquet(
    taf_catalog_source_path(year = yr, version = taf_version)
  )
  stopifnot(
    "TAF catalog must contain every installed county" = setequal(
      unique(catalog$county_fips),
      unique(manifest$county_fips)
    ),
    "TAF catalog and manifest must have the same number of rows" = nrow(
      catalog
    ) ==
      nrow(manifest)
  )
  pack_taf_fuel(
    year = yr,
    version = taf_version,
    out_dir = release_asset_dir,
    package_version = addr_package_version
  )
}
