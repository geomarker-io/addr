#' Read National Address Database (NAD) tables into R
#'
#' @description
#' The U.S. Department of Transportation partners with address programs from
#' state, local, and tribal governments to compile their authoritative data
#' into a database. Find more information here:
#' <https://www.transportation.gov/gis/national-address-database>
#'
#' `nad_read()` reads data from the NAD geodatabase by county,
#' downloading it first to the R user's data directory for the addr package
#' if not already downloaded with `nad_download()`, and readies it for R.
#' Counties can be identified either by county name plus state, or by a
#' 5-digit county FIPS identifier. County names and state abbreviations are
#' resolved internally and still determine the cache path and source query.
#' The NAD geodatabase has a very large size on disk (~10 GB).
#'
#' Data binaries are the cached outputs of `nad_read()` for each
#' County/State and are created on first run with `nad()`.
#' Download data binaries to the `tools::R_user_dir()` cache directory, or
#' point R to these files on disk, to read NAD tables without downloading the
#' nationwide NAD geodatabase.
#' (Files are organized by major package version,
#' NAD release, state, and named by county; e.g., see
#' `list.files(tools::R_user_dir("addr", "cache"), recursive = TRUE)`)
#'
#' @param county character, length one; county name or 5-digit county FIPS
#'   identifier
#' @param state character, length one; name or abbreviation of state. Required
#'   when `county` is a county name; ignored when `county` is a 5-digit county
#'   FIPS identifier
#' @param refresh_binary character, length one; choose how to refresh NAD
#' data binaries cached on disk if not already present; "yes" will
#' create data binary if not already present, "no" will
#' error if data binary is not already present, "force" will
#' create the data binary and overwrite any existing data binary
#'
#' @details
#' The NAD is downloaded from each release on the transportation.gov
#' data portal:
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
#' # read data from NAD, caching output on first run
#' \dontrun{
#'   nad("Hamilton", "OH")
#'   nad("39061")
#' }
#'
#' # example data preloaded for Hamilton County, OH
#' # works without downloading NAD gdb first
#' nad("Hamilton", "OH", refresh_source = "no", refresh_binary = "no")
#' nad("39061", refresh_source = "no", refresh_binary = "no")
#'
#' # some older releases can still be downloaded
#' \dontrun{
#'   nad_download(release = "NAD_r21_FGDB.zip")
#' }
#' # can also point to older releases if they are already downloaded
#' # or if data binaries are installed
#' \dontrun{
#'   nad_download(release = "NAD_r21_FGDB.zip", refresh_source = "no")
#' }
nad <- function(
  county,
  state = NULL,
  release = "latest",
  refresh_binary = c("yes", "no", "force"),
  refresh_source = c("yes", "no", "force")
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state),
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
  if (release == "latest") {
    release <- fetch_nad_metadata()$flnm
  }
  nad_sd <- nad_sd_path(
    county = county_info$county,
    state = county_info$state,
    release = release
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
        " does not exist; set `refresh = 'yes'`",
        " to install from source NAD,",
        " or use ... to download data binary."
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
      release = release,
      refresh_source = refresh_source
    )
    saveRDS(d, file = nad_sd)
  }
  readRDS(nad_sd)
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

nad_sd_path <- function(county, state, release) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be a character vector" = is.character(state),
    "state must be length one" = length(state) == 1L,
    "state must not be missing" = !is.na(state)
  )
  file.path(
    tools::R_user_dir("addr", "cache"),
    "v1",
    sub("^(NAD_r[0-9]+)(_FGDB)?\\.zip$", "\\1", release),
    state,
    sprintf("%s.rds", county)
  )
}


#' @rdname nad
nad_read <- function(
  county,
  state = NULL,
  release = "latest",
  refresh_source = c("yes", "no", "force")
) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be NULL or a character vector" = is.null(state) ||
      is.character(state),
    "state must be NULL or length one" = is.null(state) || length(state) == 1L,
    "state must be NULL or not missing" = is.null(state) || !is.na(state)
  )
  county_info <- nad_county_info(county, state)
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
    "Addr_Type"
  )
  if (release == "latest") {
    release <- fetch_nad_metadata()$flnm
  }
  nad_gdb_file <- nad_download(
    release = release,
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

fetch_nad_metadata <- function() {
  txt <- readLines(
    "https://data.transportation.gov/api/views/yw36-suxr",
    warn = FALSE,
    encoding = "UTF-8"
  )
  check_installed("jsonlite", "to read downloaded NAD metadata")

  d <- jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = TRUE)
  list(
    flnm = d$blobFilename,
    flid = d$blobId,
    flsz = format(
      structure(d$blobFileSize, class = "object_size"),
      units = "auto"
    ),
    fldt = as.POSIXct(d$viewLastModified, origin = "1970-01-01", tz = "UTC"),
    dlurl = sprintf(
      "https://data.transportation.gov/api/views/%s/files/%s?download=true&filename=%s",
      d$id,
      d$blobId,
      utils::URLencode(d$blobFilename, reserved = TRUE)
    )
  )
}

#' @param release character, length 1; revision of the NAD;
#' the default ("latest") fetches the most recent version available online;
#' specify other versions by their .zip filename (see examples)
#' @param refresh_source character, length one; choose how to refresh NAD
#' source geodatabase on disk if not already present; "yes" will download
#' the geodatabase if not already present, "no" will error if the file does
#' not already exist, "force" will download and overwrite any existing
#' geodatabase
#' @rdname nad
nad_download <- function(
  release = "latest",
  refresh_source = c("yes", "no", "force")
) {
  refresh_source <- match.arg(refresh_source)
  old_timeout <- options("timeout")$timeout
  new_timeout <- max(old_timeout, 2500)
  options(timeout = new_timeout)
  on.exit(options(timeout = old_timeout))
  if (refresh_source %in% c("yes", "force")) {
    nad_md <- fetch_nad_metadata()
    if (release == "latest") {
      release <- nad_md$flnm
    }
    dest <- nad_data_path(release)
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
              release = release,
              error_message = conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    }
  } else {
    dest <- nad_data_path(release)
    if (!file.exists(dest)) {
      stop(
        dest,
        " does not exist; set `refresh = 'yes'`",
        " to download the most recent version of the NAD geodatabase"
      )
    }
  }
  return(file.path("/vsizip", dest, sub("(_FGDB)?\\.zip$", ".gdb", release)))
}

nad_data_path <- function(release) {
  stopifnot(
    "release must be a character vector" = is.character(release),
    "release must be length one" = length(release) == 1L,
    "release must not be missing" = !is.na(release)
  )
  file.path(tools::R_user_dir("addr", "data"), release)
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

nad_download_failure_message <- function(dest, release, error_message) {
  stopifnot(
    "dest must be a character vector" = is.character(dest),
    "dest must be length one" = length(dest) == 1L,
    "dest must not be missing" = !is.na(dest),
    "release must be a character vector" = is.character(release),
    "release must be length one" = length(release) == 1L,
    "release must not be missing" = !is.na(release),
    "error_message must be a character vector" = is.character(error_message),
    "error_message must be length one" = length(error_message) == 1L,
    "error_message must not be missing" = !is.na(error_message)
  )
  paste0(
    "failed to download `", release, "` to `", dest, "`: ",
    error_message,
    ". If you can download it another way, place it at `", dest, "`",
    " or set `R_USER_DATA_DIR` so ",
    "`tools::R_user_dir(\"addr\", \"data\")` points to a directory",
    " that already contains `", release, "`."
  )
}
