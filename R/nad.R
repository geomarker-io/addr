#' National Address Database (NAD)
#'
#' Download the NAD to the R user's data directory for the addr package
#' and read data into R by county and state.
#'
#' @param county character, length one; name of county
#' @param state character, length one; name of state
#'
#' @details
#'
#' The U.S. Department of Transportation partners with address programs from state,
#' local, and tribal governments to compile their authoritative data into a database.
#' Find more information here: <https://www.transportation.gov/gis/national-address-database>
#' The NAD is downloaded as the latest release from the transportation.gov data portal:
#' <https://data.transportation.gov/dataset/National-Address-Database-NAD-File-Geodatabase/yw36-suxr/about_data>
#' For the original schema, see <https://www.transportation.gov/sites/dot.gov/files/2023-07/NAD_Schema_202304.pdf>
#' @export
#' @examples
#' \dontrun{
#'   nad_read("Hamilton", "OH")
#' }
nad_read <- function(county, state) {
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
  the_query <- glue::glue(
    "SELECT { paste(nad_fields, collapse = ', ') } FROM NAD WHERE State = '{ state }' AND County = '{ county }'"
  )
  rnad <- sf::st_read(dsn = nad_download(), query = the_query)
  rnad_addr <-
    with(rnad, {
      addr(
        addr_number(
          prefix = AddNum_Pre,
          digits = as.character(Add_Number),
          suffix = AddNum_Suf
        ),
        addr_street(
          predirectional = St_PreDir,
          premodifier = St_PreMod,
          pretype = St_PreTyp,
          name = St_Name,
          posttype = St_PosTyp,
          postdirectional = St_PosDir
        ),
        addr_place(name = Post_City, state = State, zip = Zip_Code)
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

#' @rdname nad_read
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
