#' nad() reads cached NAD data
#'
#' NAD data (r21) is read using the sf package and converted to s2_geography,
#' then saved into the cache R_user_dir for the addr package.
#' Using a data binary does not require gdal nor the sf package and
#' saves the time required to read and process the data from disk.
#' In the future, data binaries will be hosted online and be available
#' to download and use directly witout needing the NAD source database.

#' Read National Address Database (NAD) tables into R
#'
#' @description
#' The U.S. Department of Transportation partners with address programs from
#' state, local, and tribal governments to compile their authoritative data
#' into a database. Find more information here:
#' <https://www.transportation.gov/gis/national-address-database>
#'
#' `nad_read()` reads data from the NAD geodatabase by county and state,
#' downloading it first to the R user's data directory for the addr
#' packages if not already downloaded with `nad_download()`,
#' and readies it for R.
#' The NAD geodatabase has a very large size on disk (~10 GB).
#'
#' Data binaries are the cached outputs of `nad_read()` for each
#' County/State and are created on first run with `nad()`.
#' Download data binaries to the R_user_dir cache folder or point R
#' (`?R_user_dir`) to these files on disk to read NAD tables without
#' having to download the nationwide NAD geodatabase.
#' (`list.files(tools::R_user_dir("addr", "cache"), recursive = TRUE)`)
#'
#' @param county character, length one; name of county
#' @param state character, length one; name of state
#' @param refresh character, length one; choose how to refresh NAD
#' data binaries cached on disk if not already present; "yes" will
#' create data binary if not already present, "no" will
#' error if data binary is not already present, "force" will
#' create the data binary and overwrite any existing data binary
#'
#' @details
#' The NAD is downloaded as the latest release from the transportation.gov
#' data portal:
#' <https://data.transportation.gov/dataset/National-Address-Database-NAD-File-Geodatabase/yw36-suxr/about_data>
#' For the original schema, see
#' <https://www.transportation.gov/sites/dot.gov/files/2023-07/NAD_Schema_202304.pdf>
#'
#' The NAD does not distinguish between empty and missing address components.
#' When reading into R, all missing address components are replaced with an
#' empty string (`""`) *except* for address number (digits), street name,
#' and ZIP code.
#'
#' @export
#' @examples
#' # read data from NAD, caching output on first run
#' \dontrun{
#'   nad("Hamilton", "OH")
#' }
#'
#' # example data preloaded for Hamilton County, OH
#' # works without downloading NAD gdb first
#' nad("Hamilton", "OH", refresh = "no")
nad <- function(county, state, refresh = c("yes", "no", "force")) {
  refresh <- match.arg(refresh)
  nad_sd <- nad_sd_path(county, state)
  if (
    county == "Hamilton" &&
      state == "OH" &&
      !file.exists(nad_sd) &&
      refresh == "no"
  ) {
    return(addr::example_nad_data(match_prepare = FALSE))
  }
  if (!file.exists(nad_sd) || refresh == "force") {
    if (refresh == "no") {
      stop(
        nad_sd,
        " does not exist; set `refresh = 'yes'`",
        " to install from source NAD,",
        " or use ... to download data binary."
      )
    }
    if (refresh == "yes") {
      message(nad_sd, " does not exist; installing from source...")
    } else {
      message("forcing install from source...")
    }
    dir.create(dirname(nad_sd), recursive = TRUE, showWarnings = FALSE)
    d <- nad_read(county = county, state = state)
    saveRDS(d, file = nad_sd)
  }
  readRDS(nad_sd)
}

nad_sd_path <- function(county, state) {
  file.path(
    tools::R_user_dir("addr", "cache"),
    "NAD",
    "v1",
    state,
    sprintf("%s.rds", county)
  )
}


#' @rdname nad
nad_read <- function(county, state) {
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
  the_query <- sprintf(
    "SELECT %s FROM NAD WHERE State = '%s' AND County = '%s'",
    paste(nad_fields, collapse = ", "),
    state,
    county
  )
  rnad <- sf::st_read(dsn = nad_download(), query = the_query)
  na_to_empty <- \(x) ifelse(is.na(x), "", x)
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

#' @rdname nad
nad_download <- function() {
  nad_url <- "https://data.transportation.gov/download/yw36-suxr/application%2Fx-zip-compressed"
  dest <- file.path(
    tools::R_user_dir("addr", "data"),
    "NAD_r21_FGDB.zip"
  )
  if (!file.exists(dest)) {
    tf <- tempfile()
    utils::download.file(nad_url, tf)
    file.copy(tf, dest)
  }
  return(file.path("/vsizip", dest, "NAD_r21.gdb"))
}
