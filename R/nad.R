#' Read National Address Database (NAD) tables into R
#'
#' @description
#' The U.S. Department of Transportation partners with address programs from
#' state, local, and tribal governments to compile their authoritative data
#' into a database. Find more information here:
#' <https://www.transportation.gov/gis/national-address-database>
#'
#' `nad_read()` reads data from the NAD geodatabase by county,
#' using source data already downloaded with `nad_download()` or downloading
#' it when `refresh_source = "yes"`, and readies it for R.
#' Counties can be identified either by county name plus state, or by a
#' 5-digit county FIPS identifier. County names and state abbreviations are
#' resolved internally and still determine the cache path and source query.
#' The NAD geodatabase has a very large size on disk (~10 GB).
#'
#' Data binaries are the cached outputs of `nad_read()` for each
#' County/State and are created on first run with `nad()`.
#' Download data binaries to the `tools::R_user_dir()` data directory, or
#' point R to these files on disk, to read NAD tables without downloading the
#' nationwide NAD geodatabase.
#' (Files are organized by major package version,
#' NAD version, state, and named by county; e.g., see
#' `list.files(tools::R_user_dir("addr", "data"), recursive = TRUE)`)
#'
#' @param county character, length one; county name or 5-digit county FIPS
#'   identifier
#' @param state character, length one; name or abbreviation of state. Required
#'   when `county` is a county name; ignored when `county` is a 5-digit county
#'   FIPS identifier
#' @param version integer, length one; NAD revision to use. Defaults to `22L`,
#'   revision 22 of the National Address Database.
#' @param refresh_binary character, length one; choose how to refresh NAD
#' data binaries cached on disk if not already present; "yes" will
#' create data binary if not already present, "no" will
#' error if data binary is not already present, "force" will
#' create the data binary and overwrite any existing data binary
#'
#' @details
#' NAD source geodatabases are downloaded from the transportation.gov data
#' portal:
#' <https://data.transportation.gov/d/yw36-suxr>
#' Downloads use the R `curl` package and resume from any interrupted
#' partial download left in the addr user data directory.
#' If the download cannot complete, `nad_download()` will also work with a
#' NAD ZIP file that was downloaded another way and placed where
#' `tools::R_user_dir("addr", "data")` can find it.
#' For the original schema, see
#' <https://www.transportation.gov/sites/dot.gov/files/2023-07/NAD_Schema_202304.pdf>
#' Before downloading, please read the disclaimer here:
#' <https://www.transportation.gov/mission/open/gis/national-address-database/national-address-database-nad-disclaimer>
#'
#' Investigate individual address points in the online viewer:
#' <https://usdot.maps.arcgis.com/apps/instant/portfolio/index.html?appid=59f7e4fb71994d13b61f424e21a6cffe>
#'
#' The NAD does not distinguish between empty and missing address components.
#' When reading into R, all missing address components are replaced with an
#' empty string (`""`) *except* for address number (digits), street name,
#' and ZIP code.
#' Addresses with malformed ZIP codes are removed.
#'
#' @export
#' @examples
#' # explicitly download source data, then cache county output on first read
#' \dontrun{
#'   nad_download(version = 22L)
#'   nad("Butler", "OH")
#'   nad("39017")
#' }
#'
#' # example data preloaded for Hamilton County, OH
#' # works without downloading NAD gdb first
#' Sys.setenv(R_USER_DATA_DIR = tempfile())
#' nad("Hamilton", "OH", refresh_source = "no", refresh_binary = "no")
#' nad("39061", refresh_source = "no", refresh_binary = "no")
nad <- function(
  county,
  state = NULL,
  version = 22L,
  refresh_binary = c("yes", "no", "force"),
  refresh_source = c("no", "yes", "force")
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state),
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version),
    "refresh_binary must be a character vector" = is.character(refresh_binary),
    "refresh_binary must not contain missing values" = !any(is.na(
      refresh_binary
    )),
    "refresh_source must be a character vector" = is.character(refresh_source),
    "refresh_source must not contain missing values" = !any(is.na(
      refresh_source
    ))
  )
  refresh_binary <- match.arg(refresh_binary)
  refresh_source <- match.arg(refresh_source)
  county_info <- nad_county_info(county, state)
  nad_version_metadata(version)
  nad_sd <- nad_sd_path(
    county = county_info$county,
    state = county_info$state,
    version = version
  )
  if (
    county_info$county_fips == "39061" &&
      !file.exists(nad_sd) &&
      refresh_binary == "no"
  ) {
    return(nad_example_data(match_prepared = FALSE))
  }
  if (!file.exists(nad_sd) || refresh_binary == "force") {
    if (refresh_binary == "no") {
      stop(
        nad_sd,
        " does not exist; set `refresh_binary = 'yes'`",
        " to install from source NAD"
      )
    }
    if (refresh_binary == "yes") {
      message(nad_sd, " does not exist; installing from source...")
    } else {
      message("forcing install from source...")
    }
    dir.create(dirname(nad_sd), recursive = TRUE, showWarnings = FALSE)
    d <- nad_read(
      county = county,
      state = state,
      version = version,
      refresh_source = refresh_source
    )
    saveRDS(d, file = nad_sd)
  }
  readRDS(nad_sd)
}

nad_version_metadata <- function(version = 22L) {
  stopifnot(
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )

  metadata <- switch(
    as.character(version),
    "22" = list(
      flnm = "NAD_r22.zip",
      flid = "1900bfb9-7fcb-4367-96c4-e9b10642dd8d",
      flsz = "10.1 Gb",
      fldt = as.POSIXct("2026-05-06 12:15:49", tz = "UTC"),
      dlurl = paste0(
        "https://data.transportation.gov/api/views/yw36-suxr/files/",
        "1900bfb9-7fcb-4367-96c4-e9b10642dd8d",
        "?download=true&filename=NAD_r22.zip"
      )
    ),
    NULL
  )

  if (is.null(metadata)) {
    stop(
      "NAD version `",
      version,
      "` is not supported; supported versions: 22",
      call. = FALSE
    )
  }

  metadata
}

nad_county_info <- function(county, state = NULL) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state)
  )

  if (!grepl("^[0-9]{5}$", county) && is.null(state)) {
    stop(
      "state must be supplied when county is not a 5-digit FIPS identifier",
      call. = FALSE
    )
  }

  county_info <- county_fips_lookup(county, state)
  list(
    county_fips = county_info$county_fips[[1]],
    county = county_info$county[[1]],
    state = county_info$state[[1]]
  )
}

nad_sd_path <- function(county, state, version = 22L) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be a character vector" = is.character(state),
    "state must be length one" = length(state) == 1L,
    "state must not be missing" = !is.na(state)
  )
  source_file <- nad_version_metadata(version)$flnm
  file.path(
    tools::R_user_dir("addr", "data"),
    "v1",
    sub("^(NAD_r[0-9]+)(_FGDB)?\\.zip$", "\\1", source_file),
    state,
    sprintf("%s.rds", county)
  )
}


#' @rdname nad
nad_read <- function(
  county,
  state = NULL,
  version = 22L,
  refresh_source = c("no", "yes", "force")
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state),
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  county_info <- nad_county_info(county, state)
  nad_version_metadata(version)
  check_installed("sf", "to read from the NAD geodatabase")
  nad_fields <- c(
    "AddNum_Pre",
    "Add_Number",
    "AddNum_Suf",
    "St_PreDir",
    "St_PreMod",
    "St_PreTyp",
    "St_Name",
    "St_PosTyp",
    "St_PosDir",
    "St_PosMod",
    "SubAddress",
    "County",
    "Post_City", # or use Inc_Muni
    "State",
    "Zip_Code",
    "UUID",
    "Latitude",
    "Longitude",
    "Parcel_ID",
    "DateUpdate",
    "NatGrid",
    "Placement",
    "AddrClass",
    "Addr_Type"
  )
  nad_gdb_file <- nad_download(
    version = version,
    refresh_source = refresh_source
  )
  nad_layer_name <- sf::st_layers(nad_gdb_file)[1, "name"]
  the_query <- sprintf(
    "SELECT %s FROM %s WHERE State = '%s' AND County = '%s'",
    paste(nad_fields, collapse = ", "),
    nad_layer_name,
    county_info$state,
    county_info$county
  )
  rnad <- sf::st_read(
    dsn = nad_gdb_file,
    query = the_query
  )
  na_to_empty <- \(x) ifelse(is.na(x), "", x)
  bad_zips <- which(nchar(rnad$Zip_Code) != 5L)
  if (length(bad_zips) > 0) {
    warning(
      "removing ",
      length(bad_zips),
      " address records in ",
      county_info$county_fips,
      " with malformed ZIP codes."
    )
    rnad <- rnad[-bad_zips, ]
  }
  rnad_addr <-
    with(rnad, {
      addr(
        addr_number(
          prefix = na_to_empty(AddNum_Pre),
          digits = as.character(Add_Number),
          suffix = na_to_empty(AddNum_Suf)
        ),
        addr_street(
          predirectional = na_to_empty(St_PreDir),
          premodifier = na_to_empty(St_PreMod),
          pretype = na_to_empty(St_PreTyp),
          name = St_Name,
          posttype = na_to_empty(St_PosTyp),
          postdirectional = na_to_empty(St_PosDir)
        ),
        addr_place(
          name = na_to_empty(Post_City),
          state = na_to_empty(State),
          zip = Zip_Code
        )
      )
    })
  rnad_s2 <- s2::as_s2_cell(s2::s2_lnglat(rnad$Longitude, rnad$Latitude))
  out <- tibble::tibble(
    nad_addr = rnad_addr,
    subaddress = rnad$SubAddress,
    uuid = rnad$UUID,
    date_update = as.Date(rnad$DateUpdate),
    s2 = rnad_s2,
    national_grid = rnad$NatGrid,
    placement = rnad$Placement,
    address_class = rnad$AddrClass,
    address_type = rnad$Addr_Type,
    parcel_id = rnad$Parcel_ID
  )
  return(out)
}

#' @param refresh_source character, length one; choose how to refresh NAD
#' source geodatabase on disk if not already present; "yes" will download
#' the geodatabase if not already present, "no" will error if the file does
#' not already exist, "force" will download and overwrite any existing
#' geodatabase
#' @rdname nad
nad_download <- function(
  version = 22L,
  refresh_source = c("yes", "no", "force")
) {
  refresh_source <- match.arg(refresh_source)
  nad_md <- nad_version_metadata(version)
  source_file <- nad_md$flnm
  old_timeout <- options("timeout")$timeout
  new_timeout <- max(old_timeout, 2500)
  options(timeout = new_timeout)
  on.exit(options(timeout = old_timeout))
  if (refresh_source %in% c("yes", "force")) {
    dest <- nad_data_path(version)
    partial_dest <- nad_partial_path(dest)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (refresh_source == "force") {
      unlink(dest)
      unlink(partial_dest)
    }
    if (!file.exists(dest)) {
      tryCatch(
        {
          nad_download_archive(nad_md$dlurl, partial_dest)
          ok <- file.rename(partial_dest, dest)
          if (!ok) {
            stop("failed to move completed download into place")
          }
        },
        error = function(e) {
          stop(
            nad_download_failure_message(
              dest = dest,
              version = version,
              error_message = conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    }
  } else {
    dest <- nad_data_path(version)
    if (!file.exists(dest)) {
      stop(
        dest,
        " does not exist; set `refresh_source = 'yes'`",
        " to download NAD version ",
        version
      )
    }
  }
  return(
    file.path(
      "/vsizip",
      dest,
      sub("(_FGDB)?\\.zip$", ".gdb", source_file)
    )
  )
}

nad_data_path <- function(version = 22L) {
  file.path(
    tools::R_user_dir("addr", "data"),
    nad_version_metadata(version)$flnm
  )
}

nad_partial_path <- function(dest) {
  stopifnot(
    "dest must be a character vector" = is.character(dest),
    "dest must be length one" = length(dest) == 1L,
    "dest must not be missing" = !is.na(dest)
  )
  paste0(dest, ".part")
}

nad_download_archive <- function(url, dest) {
  stopifnot(
    "url must be a character vector" = is.character(url),
    "url must be length one" = length(url) == 1L,
    "url must not be missing" = !is.na(url),
    "dest must be a character vector" = is.character(dest),
    "dest must be length one" = length(dest) == 1L,
    "dest must not be missing" = !is.na(dest)
  )
  resume_from <- 0
  if (file.exists(dest)) {
    resume_from <- file.info(dest)$size[[1]]
    if (is.na(resume_from)) {
      resume_from <- 0
    }
  }
  if (resume_from > 0) {
    message("resuming interrupted NAD download at ", dest)
  }
  h <- curl::new_handle()
  if (resume_from > 0) {
    curl::handle_setopt(h, resume_from_large = resume_from)
  }
  res <- curl::curl_fetch_disk(url, dest, handle = h)
  if (!is.null(res$status_code) && res$status_code >= 400L) {
    stop("download returned HTTP status ", res$status_code)
  }
  invisible(dest)
}

nad_download_failure_message <- function(dest, version, error_message) {
  stopifnot(
    "dest must be a character vector" = is.character(dest),
    "dest must be length one" = length(dest) == 1L,
    "dest must not be missing" = !is.na(dest),
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version),
    "error_message must be a character vector" = is.character(error_message),
    "error_message must be length one" = length(error_message) == 1L,
    "error_message must not be missing" = !is.na(error_message)
  )
  source_file <- nad_version_metadata(version)$flnm
  paste0(
    "failed to download NAD version `",
    version,
    "` (`",
    source_file,
    "`) to `",
    dest,
    "`: ",
    error_message,
    ". If you can download it another way, place it at `",
    dest,
    "`",
    " or set `R_USER_DATA_DIR` so ",
    "`tools::R_user_dir(\"addr\", \"data\")` points to a directory",
    " that already contains `",
    source_file,
    "`."
  )
}
