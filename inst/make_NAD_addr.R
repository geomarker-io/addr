library(sf)

nad_release <- "NAD_r19"

nad_cloud_dsn <- glue::glue(
  "/vsizip",
  "/vsicurl",
  "https:/",
  "nationaladdressdata.s3.amazonaws.com",
  "{ nad_release }.zip",
  "{ nad_release }.gdb",
  .sep = "/"
)

nad_local_dsn <- "NAD_r19.gdb"

# https://www.transportation.gov/sites/dot.gov/files/2023-07/NAD_Schema_202304.pdf
nad_fields <- c(
  "AddNum_Pre",
  "Add_Number",
  "AddNum_Suf",
  "St_PreMod",
  "St_PreDir",
  "St_PreTyp",
  "St_PreSep",
  "St_Name",
  "St_PosTyp",
  "St_PosDir",
  "St_PosMod",
  "SubAddress",
  "County",
  "Post_City",
  "State",
  "Zip_Code",
  "UUID",
  "Latitude",
  "Longitude",
  "Placement",
  "ParcelSrc",
  "Parcel_ID",
  "NAD_Source",
  "DateUpdate"
)

the_state <- "OH"
the_county <- "Hamilton"

the_query <- glue::glue(
  "SELECT { paste(nad_fields, collapse = ', ') } FROM NAD WHERE State = '{ the_state }' AND County = '{ the_county }'"
)

rnad <- sf::st_read(dsn = nad_local_dsn, query = the_query)

paste_na_omit <- function(..., sep = "") {
  args <- lapply(list(...), as.character)
  if (!length(args)) return(character())
  lens <- vapply(args, length, integer(1))
  stopifnot("inputs are same length" = length(unique(lens)) == 1L)
  newargs <- lapply(args, \(x) ifelse(is.na(x), "", x))
  do.call(paste0, newargs)
}

nad_county_addr <-
  addr:::new_addr(
    street_number = as.numeric(rnad$Add_Number),
    street_name = paste_na_omit(
      rnad$St_PreMod,
      rnad$St_PreTyp,
      rnad$St_PreSep,
      rnad$St_Name
    ),
    street_type = paste_na_omit(rnad$St_PosTyp, rnad$St_PosDir, rnad$St_PosMod),
    city = rnad$Post_City,
    state = rnad$State,
    zip_code = rnad$Zip_Code
  )

out <- tibble::tibble(
  nad_addr = nad_county_addr,
  nad_uuid = rnad$UUID,
  nad_placement = rnad$Placement,
  nad_parcel_id = rnad$Parcel_ID,
  nad_source = rnad$NAD_Source,
  nad_lat = rnad$Latitude,
  nad_lon = rnad$Longitude
)

out

saveRDS(out, glue::glue("inst/NAD_{ the_state }_{ the_county }.rds"))
