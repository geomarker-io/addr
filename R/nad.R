#' Read National Address Database (NAD) tables into R
#'
#' @description
#' The U.S. Department of Transportation partners with address programs from
#' state, local, and tribal governments to compile their authoritative data
#' into a database. Find more information in the source data portal:
#' <https://data.transportation.gov/d/fc2s-wawr>
#'
#' `nad_read()` reads NAD source data by county,
#' using source data already downloaded with `nad_download()` or downloading
#' it when `refresh_source = "yes"`, and readies it for R.
#' Counties can be identified either by county name plus state, or by a
#' 5-digit county FIPS identifier. County names and state abbreviations are
#' resolved internally and still determine the workspace path and source query.
#' The revision 23 source is a roughly 7.6 GB compressed archive containing a
#' roughly 41 GB comma-delimited text member.
#'
#' Data binaries are the persistent outputs of `nad_read()` for each
#' County/State and are created on first run with `nad()`.
#' Source and derived files are kept in the persistent workspace returned by
#' `stow::stow_path(package = "addr", subdir = "nad")`. Point R to files in
#' that workspace to read NAD tables without downloading the nationwide NAD
#' source again.
#' (Files are organized by major package version,
#' NAD version, state, and named by county; e.g., see
#' `list.files(stow::stow_path(package = "addr", subdir = "nad"),
#' recursive = TRUE)`)
#' @param county character, length one; county name or 5-digit county FIPS
#'   identifier
#' @param state character, length one; name or abbreviation of state. Required
#'   when `county` is a county name; ignored when `county` is a 5-digit county
#'   FIPS identifier
#' @param version integer, length one; NAD revision to use. Only revision 23 is
#'   supported.
#' @param refresh_binary character, length one; choose how to refresh NAD
#' data binaries stored on disk if not already present; "yes" will
#' create data binary if not already present, "no" will
#' error if data binary is not already present, "force" will
#' create the data binary and overwrite any existing data binary
#'
#' @details
#' The revision 23 comma-delimited flat source archive is downloaded from the
#' transportation.gov data portal:
#' <https://data.transportation.gov/d/fc2s-wawr>.
#' `nad_download(version = 23L)` installs the pinned compressed archive as a
#' durable managed local copy using `stow::stow()`. County installation streams
#' the nationwide text member directly from that archive, retains the requested
#' county, and writes a managed RDS file.
#' The roughly 41 GB text member is never unpacked on disk.
#' Before downloading, review the source metadata and disclaimer in the data
#' portal.
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
#' # explicitly download source data, then create county output on first read
#' \dontrun{
#'   # install the compressed revision 23 flat source archive, then build a
#'   # county RDS from it
#'   nad_download()
#'   nad("Butler", "OH")
#'   nad("39017")
#' }
#'
#' # small packaged revision 23 fixture derived from Hamilton County, OH
#' nad_example_data()
nad <- function(
  county,
  state = NULL,
  version = 23L,
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
    return(d)
  }
  readRDS(nad_sd)
}

nad_version_metadata <- function(version = 23L) {
  stopifnot(
    "version must be an integer vector" = is.integer(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )

  if (version != 23L) {
    stop(
      "NAD version `",
      version,
      "` is not supported; supported version: 23",
      call. = FALSE
    )
  }

  list(
    cache_dir = "NAD_r23",
    source_size = 7601412707,
    source_members = c(
      "TXT/NAD_r23.txt",
      "TXT/NationalAddressDatabaseMetadata.xml"
    ),
    dlurl = paste0(
      "https://data.transportation.gov/api/views/fc2s-wawr/files/",
      "b189f78b-2262-44e8-b3b6-5c4094c12da5"
    )
  )
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

nad_sd_path <- function(county, state, version = 23L) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "state must be a character vector" = is.character(state),
    "state must be length one" = length(state) == 1L,
    "state must not be missing" = !is.na(state)
  )
  cache_dir <- nad_version_metadata(version)$cache_dir
  file.path(
    nad_workspace_path(),
    "v1",
    cache_dir,
    state,
    sprintf("%s.rds", county)
  )
}


# Columns retained from the NAD flat source.
nad_source_fields <- function() {
  c(
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
}

#' @rdname nad
nad_read <- function(
  county,
  state = NULL,
  version = 23L,
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
  nad_md <- nad_version_metadata(version)
  nad_fields <- nad_source_fields()
  nad_source <- nad_download(
    version = version,
    refresh_source = refresh_source
  )
  rnad <- tibble::as_tibble(nad_flat_extract(
    path = nad_source,
    member = nad_md$source_members[[1L]],
    state = county_info$state,
    county = county_info$county,
    fields = nad_fields
  ))
  rnad$Longitude <- as.numeric(rnad$Longitude)
  rnad$Latitude <- as.numeric(rnad$Latitude)
  nad_transform(rnad, county_info$county_fips)
}

nad_transform <- function(rnad, county_fips) {
  na_to_empty <- \(x) ifelse(is.na(x), "", x)
  bad_zips <- which(nchar(rnad$Zip_Code) != 5L)
  if (length(bad_zips) > 0) {
    warning(
      "removing ",
      length(bad_zips),
      " address records in ",
      county_fips,
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
  tibble::tibble(
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
}

#' @param refresh_source character, length one; choose how to refresh NAD
#' source archive on disk if not already present; "yes" will download the
#' archive if needed, "no" will require an existing local source, and "force"
#' will download and overwrite an existing source
#' @rdname nad
nad_download <- function(
  version = 23L,
  refresh_source = c("yes", "no", "force")
) {
  refresh_source <- match.arg(refresh_source)
  nad_md <- nad_version_metadata(version)
  stow::stow(
    nad_md$dlurl,
    package = "addr",
    subdir = "nad",
    overwrite = identical(refresh_source, "force"),
    offline = identical(refresh_source, "no"),
    etag = FALSE,
    validate = function(path) {
      nad_validate_flat_source(
        path,
        nad_md$source_members,
        nad_md$source_size
      )
    }
  )
}

nad_validate_flat_source <- function(
  path,
  required_members,
  expected_size = NULL
) {
  stopifnot(
    "path must be a character vector" = is.character(path),
    "path must be length one" = length(path) == 1L,
    "path must not be missing" = !is.na(path),
    "required_members must be a character vector" = is.character(
      required_members
    ),
    "required_members must not contain missing values" = !any(is.na(
      required_members
    )),
    "required_members must not be empty" = length(required_members) > 0L,
    "expected_size must be NULL or numeric" = is.null(expected_size) ||
      is.numeric(expected_size),
    "expected_size must be NULL or length one" = is.null(expected_size) ||
      length(expected_size) == 1L,
    "expected_size must be NULL or not missing" = is.null(expected_size) ||
      !is.na(expected_size)
  )
  if (!file.exists(path)) {
    return(FALSE)
  }
  actual_size <- file.info(path)$size
  if (!is.null(expected_size) && actual_size != expected_size) {
    return(FALSE)
  }
  contents <- tryCatch(
    suppressWarnings(utils::unzip(path, list = TRUE)),
    error = function(e) NULL
  )
  listed <- is.data.frame(contents) &&
    "Name" %in% names(contents) &&
    all(required_members %in% contents$Name)
  if (listed) {
    return(TRUE)
  }

  # R's bundled ZIP reader cannot list USDOT's 7.6 GB ZIP64 archive. Fall
  # back to its pinned byte size and central-directory markers without
  # decompressing the roughly 41 GB text member.
  connection <- file(path, open = "rb")
  on.exit(close(connection))
  header <- readBin(connection, what = "raw", n = 4L)
  tail_size <- min(actual_size, 65557)
  seek(connection, where = actual_size - tail_size, origin = "start")
  archive_tail <- readBin(connection, what = "raw", n = tail_size)
  required_patterns <- lapply(required_members, charToRaw)
  all(
    identical(header, charToRaw("PK\003\004")),
    nad_raw_contains(archive_tail, charToRaw("PK\005\006")),
    vapply(
      required_patterns,
      function(pattern) nad_raw_contains(archive_tail, pattern),
      logical(1)
    )
  )
}

nad_raw_contains <- function(x, pattern) {
  limit <- length(x) - length(pattern) + 1L
  if (limit < 1L) {
    return(FALSE)
  }
  starts <- which(x[seq_len(limit)] == pattern[[1L]])
  any(vapply(
    starts,
    function(start) {
      identical(x[start + seq_along(pattern) - 1L], pattern)
    },
    logical(1)
  ))
}

nad_workspace_path <- function() {
  stow::stow_path(package = "addr", subdir = "nad")
}
