valid_street_name_post_types <- list(
  Aly = "Alley",
  Arc = "Arcade",
  Ave = c("Avenue", "Av", "Avnue"),
  Bch = "Beach",
  Blvd = c("Boulevard", "Blvrd", "Bv"),
  Br = "Branch",
  Brg = "Bridge",
  Brk = "Brook",
  Byp = "Bypass",
  Cnl = "Canal",
  Cyn = "Canyon",
  Cir = c("Circle", "Cir", "Cr", "Crcl"),
  Cmn = "Common",
  Cor = "Corner",
  Cove = "Cove",
  Cres = "Crescent",
  Cswy = "Causeway",
  Ct = "Court",
  Cv = "Cove",
  Dam = "Dam",
  Dr = c("Drive", "Drv"),
  Expy = "Expressway",
  Ext = "Extension",
  Frwy = "Freeway",
  Fwy = "Freeway",
  Gdn = "Garden",
  Gdns = "Gardens",
  Hbr = "Harbor",
  Hts = "Heights",
  Hwy = c("Highway", "Hgwy", "Hw", "Hway", "hywy"),
  Jct = "Junction",
  Ln = "Lane",
  Loop = "Loop",
  Mdw = "Meadow",
  Mdws = "Meadows",
  Mnr = "Manor",
  Park = "Park",
  Pass = "Pass",
  Path = "Path",
  Pkwy = "Parkway",
  Pl = "Place",
  Plz = "Plaza",
  Pt = "Point",
  Rd = "Road",
  Rdg = "Ridge",
  Riv = "River",
  Row = "Row",
  Run = "Run",
  Sq = "Square",
  St = "Street",
  Ter = "Terrace",
  Tpke = "Turnpike",
  Trl = "Trail",
  Tunl = "Tunnel",
  Via = "Via",
  Vlg = "Village",
  Vw = "View",
  Walk = "Walk",
  Way = c("Way", "Wy"),
  Whrf = "Wharf"
)

valid_street_name_pre_types <- list(
  "I-" = c("Interstate Highway", "Interstate", "I", "I-"),
  "US Hwy" = c(
    "US Highway",
    "U.S. Highway",
    "US Hwy",
    "U.S. Hwy",
    "US",
    "U.S."
  ),
  "St Hwy" = c("State Highway", "State Hwy", "St Hwy"),
  "Co Hwy" = c("County Highway", "County Hwy", "Co Hwy", "CoHw"),
  "Co Rd" = c("County Road", "County Rd", "Co Rd", "CoRd"),
  "Co Rte" = c("County Route", "County Rte", "Co Rte", "CoRt"),
  "Par Rd" = c("Parish Road", "Parish Rd", "Par Rd", "ParR"),
  "Rte" = c("Route", "Rte"),
  "R Rte" = c("Rural Route", "R Rte"),
  "NF Hwy" = c("National Forest Highway", "NF Hwy", "NFHw"),
  "NFD" = c("National Forest Development Road", "NFD"),
  "FS Rd" = c("Forest Service Road", "FS Rd", "FSRd"),
  "For Hwy" = c("Forest Highway", "Forest Hwy", "For Hwy", "ForH"),
  "For Rd" = c("Forest Road", "Forest Rd", "For Rd", "ForR"),
  "Farm Rd" = c("Farm Road", "Farm Rd", "Fm Rd", "FmRd"),
  "F-M Rd" = c(
    "Farm-to-Market Road",
    "Farm to Market Road",
    "Farm-to-Market Rd",
    "Farm to Market Rd",
    "F-M Rd",
    "FMRd"
  ),
  "Ranch Rd" = c("Ranch Road", "Ranch Rd"),
  "R-M Rd" = c(
    "Ranch to Market Road",
    "Ranch to Market Rd",
    "R-M Rd",
    "RMRd"
  ),
  "Res Hwy" = c("Reservation Highway", "Reservation Hwy", "Res Hwy", "ResH"),
  "Sv Rte" = c("Service Route", "Service Rte", "Sv Rte", "SvRt"),
  "Srv Rd" = c("Service Road", "Service Rd", "Srv Rd", "SrvR"),
  "Lg Rd" = c("Logging Road", "Logging Rd", "Lg Rd", "LgRd"),
  "FR Rd" = c("Fire Road", "Fire Rd", "FR Rd", "FRRd"),
  "FR Rte" = c("Fire Route", "Fire Rte", "FR Rte", "FRRt"),
  "FR Trl" = c("Fire Trail", "Fire Trl", "FR Trl", "FRTr"),
  "Ind Rte" = c("Indian Route", "Ind Rte", "IndR"),
  "IndSvRte" = c("Indian Service Route", "IndSvRte", "IndS"),
  "NSv Rte" = c("Navajo Service Route", "NSv Rte", "NSvR"),
  "IL Rte" = c("Illinois Route", "IL Rte", "ILRt"),
  "NJ Rte" = c("New Jersey Route", "NJ Rte", "NJRt"),
  "KS StHwy" = c("Kansas State Highway", "KS StHwy", "KStH")
)

valid_directions <- list(
  N = c("North", "N.", "N"),
  S = c("South", "S.", "S"),
  E = c("East", "E.", "E"),
  W = c("West", "W.", "W"),
  NE = c("Northeast", "North East", "North-East", "NE", "N.E."),
  NW = c("Northwest", "North West", "North-West", "NW", "N.W."),
  SE = c("Southeast", "South East", "South-East", "SE", "S.E."),
  SW = c("Southwest", "South West", "South-West", "SW", "S.W.")
)

map_street_name_post_type <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  x_chr <- as.character(x)
  x_norm <- tolower(trimws(x_chr))

  type_names <- names(valid_street_name_post_types)
  type_norm <- tolower(type_names)
  type_values_norm <- lapply(valid_street_name_post_types, function(vals) {
    tolower(trimws(as.character(vals)))
  })

  vapply(
    x_norm,
    function(val) {
      if (is.na(val)) {
        return(NA_character_)
      }
      if (val == "") {
        return("")
      }
      hit_type <- match(val, type_norm)
      if (!is.na(hit_type)) {
        return(type_names[hit_type])
      }
      hit_vals <- vapply(
        type_values_norm,
        function(vals) val %in% vals,
        logical(1)
      )
      if (any(hit_vals)) {
        return(type_names[which(hit_vals)[1]])
      }
      NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
}

map_street_name_pre_type <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  x_chr <- as.character(x)
  x_norm <- tolower(trimws(x_chr))

  type_names <- names(valid_street_name_pre_types)
  type_norm <- tolower(type_names)
  type_values_norm <- lapply(valid_street_name_pre_types, function(vals) {
    tolower(trimws(as.character(vals)))
  })

  vapply(
    x_norm,
    function(val) {
      if (is.na(val)) {
        return(NA_character_)
      }
      if (val == "") {
        return("")
      }
      hit_type <- match(val, type_norm)
      if (!is.na(hit_type)) {
        return(type_names[hit_type])
      }
      hit_vals <- vapply(
        type_values_norm,
        function(vals) val %in% vals,
        logical(1)
      )
      if (any(hit_vals)) {
        return(type_names[which(hit_vals)[1]])
      }
      NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
}

map_direction <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  x_chr <- as.character(x)
  x_norm <- tolower(trimws(x_chr))

  dir_names <- names(valid_directions)
  dir_norm <- tolower(dir_names)
  dir_values_norm <- lapply(valid_directions, function(vals) {
    tolower(trimws(as.character(vals)))
  })

  vapply(
    x_norm,
    function(val) {
      if (is.na(val)) {
        return(NA_character_)
      }
      if (val == "") {
        return("")
      }
      hit_dir <- match(val, dir_norm)
      if (!is.na(hit_dir)) {
        return(dir_names[hit_dir])
      }
      hit_vals <- vapply(
        dir_values_norm,
        function(vals) val %in% vals,
        logical(1)
      )
      if (any(hit_vals)) {
        return(dir_names[which(hit_vals)[1]])
      }
      NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
}

valid_states <- list(
  AL = "Alabama",
  AK = "Alaska",
  AZ = "Arizona",
  AR = "Arkansas",
  CA = "California",
  CO = "Colorado",
  CT = "Connecticut",
  DE = "Delaware",
  FL = "Florida",
  GA = "Georgia",
  HI = "Hawaii",
  ID = "Idaho",
  IL = "Illinois",
  IN = "Indiana",
  IA = "Iowa",
  KS = "Kansas",
  KY = "Kentucky",
  LA = "Louisiana",
  ME = "Maine",
  MD = "Maryland",
  MA = "Massachusetts",
  MI = "Michigan",
  MN = "Minnesota",
  MS = "Mississippi",
  MO = "Missouri",
  MT = "Montana",
  NE = "Nebraska",
  NV = "Nevada",
  NH = "New Hampshire",
  NJ = "New Jersey",
  NM = "New Mexico",
  NY = "New York",
  NC = "North Carolina",
  ND = "North Dakota",
  OH = "Ohio",
  OK = "Oklahoma",
  OR = "Oregon",
  PA = "Pennsylvania",
  RI = "Rhode Island",
  SC = "South Carolina",
  SD = "South Dakota",
  TN = "Tennessee",
  TX = "Texas",
  UT = "Utah",
  VT = "Vermont",
  VA = "Virginia",
  WA = "Washington",
  WV = "West Virginia",
  WI = "Wisconsin",
  WY = "Wyoming",
  DC = "District of Columbia",
  AS = "American Samoa",
  GU = "Guam",
  MP = "Northern Mariana Islands",
  PR = "Puerto Rico",
  VI = "U.S. Virgin Islands"
)

map_state_to_abbrev <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  x_in <- as_norm_char(x)
  abb <- as_norm_char(names(valid_states))
  name <- as_norm_char(unlist(valid_states, use.names = FALSE))

  out <- rep(NA_character_, length(x_in))
  out[x_in == ""] <- ""
  hit_name <- match(x_in, name)
  hit_abb <- match(x_in, abb)

  out[!is.na(hit_name)] <- abb[hit_name[!is.na(hit_name)]]
  out[!is.na(hit_abb)] <- abb[hit_abb[!is.na(hit_abb)]]
  toupper(out)
}

#' map_state_to_abbrev(c("ohio", "ak", "arkansas", "hi", "foofy"))
