# from USPS Publication 28, Appendix C1
# https://pe.usps.com/text/pub28/28apc_002.htm
valid_street_name_post_types <- list(
  Aly = c("Allee", "Alley", "Ally"),
  Anx = c("Anex", "Annex", "Annx"),
  Arc = "Arcade",
  Ave = c("Av", "Aven", "Avenu", "Avenue", "Avn", "Avnue"),
  Byu = c("Bayoo", "Bayou"),
  Bch = "Beach",
  Bnd = "Bend",
  Blf = c("Bluff", "Bluf"),
  Blfs = "Bluffs",
  Btm = c("Bot", "Bottm", "Bottom"),
  Blvd = c("Boulevard", "Boulv", "Blvrd", "Boulv", "Bv"),
  Br = c("Branch", "Brnch"),
  Brg = c("Bridge", "Brdge"),
  Brk = "Brook",
  Brks = "Brooks",
  Bg = "Burg",
  Bgs = "Burgs",
  Byp = c("Bypass", "Bypa", "Bypas", "Byps"),
  Cp = c("Camp", "Cmp"),
  Cyn = c("Canyon", "Cnyn", "Canyn"),
  Cpe = "Cape",
  Cswy = c("Causeway", "Causwa"),
  Ctr = c("Center", "Cen", "Ctr", "Centr", "Centre", "Cntr", "Cent"),
  Ctrs = "Centers",
  Cir = c("Circle", "Cir", "Circ", "Circl", "Crcle", "Cr", "Crcl"),
  Cirs = "Circles",
  Clf = "Cliff",
  Clfs = "Cliffs",
  Clb = "Club",
  Cmn = "Common",
  Cmns = "Commons",
  Cor = "Corner",
  Cors = "Corners",
  Crse = "Course",
  Ct = "Court",
  Cts = "Courts",
  Cv = "Cove",
  Cvs = "Coves",
  Crk = "Creek",
  Cres = c("Crescent", "Crsent", "Crsnt"),
  Crst = "Crest",
  Xing = c("Crossing", "Crssng"),
  Xrd = "Crossroad",
  Xrds = "Crossroads",
  Curv = "Curve",
  Dl = "Dale",
  Dam = "Dam",
  Dv = c("Divide", "Div", "Dvd"),
  Dr = c("Drive", "Driv", "Drv"),
  Drs = "Drives",
  Est = "Estate",
  Ests = "Estates",
  Expy = c("Expressway", "Exp", "Expr", "Express", "Expw"),
  Ext = c("Extension", "Extn", "Extnsn"),
  Exts = "Extensions",
  Fall = "Fall",
  Fls = "Falls",
  Fry = c("Ferry", "Frry"),
  Fld = "Field",
  Flds = "Fields",
  Flt = "Flat",
  Flts = "Flats",
  Frd = "Ford",
  Frds = "Fords",
  Frst = c("Forest", "Forests"),
  Frg = c("Forge", "Forg"),
  Frgs = "Forges",
  Frk = "Fork",
  Frks = "Forks",
  Ft = c("Fort", "Frt"),
  Fwy = c("Freeway", "Frwy", "Freewy", "Frway"),
  Gdn = c("Garden", "Gardn", "Grden", "Grdn"),
  Gdns = c("Gardens", "Grdns"),
  Gtwy = c("Gateway", "Gatewy", "Gatway", "Gtway"),
  Gln = "Glen",
  Glns = "Glens",
  Grn = "Green",
  Grns = "Greens",
  Grv = c("Grove", "Grov"),
  Grvs = "Groves",
  Hbr = c("Harbor", "Harb", "Harbr", "Hrbor"),
  Hbrs = "Harbors",
  Hvn = "Haven",
  Hts = "Heights",
  Hwy = c(
    "Highway",
    "Highwy",
    "Hiway",
    "Hiwy",
    "Hway",
    "Hgwy",
    "Hw",
    "Hway",
    "Hywy"
  ),
  Hl = "Hill",
  Hls = "Hills",
  Holw = c("Hollow", "Hollows", "Holws", "Hllw"),
  Inlt = "Inlet",
  Is = c("Island", "Islnd"),
  Iss = c("Islands", "Islnds"),
  Isle = c("Isle", "Isles"),
  Jct = c("Junction", "Jction", "Jctn", "Junctn", "Juncton"),
  Jcts = c("Junctions", "Jctns"),
  Ky = "Key",
  Kys = "Keys",
  Knl = c("Knoll", "Knol"),
  Lk = "Lake",
  Lks = "Lakes",
  Land = "Land",
  Lndg = c("Landing", "Lndng"),
  Ln = "Lane",
  Lgt = "Light",
  Lgts = "Lights",
  Lf = "Loaf",
  Lck = "Lock",
  Lcks = "Locks",
  Ldg = c("Ldge", "Lodg", "Lodge"),
  Loop = "Loop",
  Mall = "Mall",
  Mnr = "Manor",
  Mnrs = "Manors",
  Mdw = "Meadow",
  Mdws = c("Meadows", "Medows"),
  Mews = "Mews",
  Ml = "Mill",
  Mls = "Mills",
  Msn = c("Mission", "Missn", "Mssn"),
  Mtwy = "Motorway",
  Mt = c("Mnt", "Mount"),
  Mtn = c("Mntain", "Mntn", "Mountain", "Mountin", "Mtin"),
  Mnts = "Mountains",
  Nck = "Neck",
  Orch = c("Orchard", "Orchrd"),
  Oval = "Ovl",
  Opas = "Overpass",
  Park = c("Prk", "Parks"),
  Pkwy = c("Parkway", "Parkwy", "Pkway", "Pky", "Parkways", "Pkwys"),
  Pass = "Pass",
  Psge = "Passage",
  Path = c("Path", "Paths"),
  Pike = c("Pike", "Pikes"),
  Pne = "Pine",
  Pnes = "Pines",
  Pl = "Place",
  Pln = "Plain",
  Plns = "Plains",
  Plz = c("Plaza", "Plza"),
  Pt = "Point",
  Pts = "Points",
  Prt = "Port",
  Prts = "Ports",
  Pr = c("Prarie", "Prr"),
  Radl = c("Rad", "Radial", "Radiel"),
  Ramp = "Ramp",
  Rnch = c("Ranch", "Ranches", "Rnchs"),
  Rpd = "Rapid",
  Rpds = "Rapids",
  Rst = "Rest",
  Rdg = c("Rdge", "Ridge"),
  Rdgs = "Ridges",
  Riv = c("River", "Rvr", "Rivr"),
  Rd = "Road",
  Rds = "Roads",
  Rte = "Route",
  Row = "Row",
  Rue = "Rue",
  Run = "Run",
  Shl = "Shoal",
  Shls = "Shoals",
  Shr = c("Shore", "Shoar"),
  Shrs = c("Shores", "Shoars"),
  Skwy = "Skyway",
  Spg = c("Spring", "Spng", "Sprng"),
  Spgs = c("Spngs", "Springs", "Sprngs"),
  Spur = "Spur",
  Spurs = "Spurs",
  Sq = c("Square", "Sqr", "Sqre", "Squ"),
  Sqs = "Squares",
  Sta = c("Station", "Statn", "Stn"),
  Stra = c("Strav", "Straven", "Stravenue", "Stravn", "Strvn", "Strvnue"),
  Strm = c("Stream", "Streme"),
  St = c("Street", "Strt", "Str"),
  Sts = c("Streets"),
  Smt = c("Sumit", "Sumitt", "Summit"),
  Ter = c("Terrace", "Terr"),
  Trwy = "Throughway",
  Trce = c("Trace", "Traces"),
  Trak = c("Track", "Tracks", "Trk", "Trks"),
  Trfy = "Trafficway",
  Trl = c("Trail", "Trails", "Trls"),
  Trlr = c("Trailer", "Trlrs"),
  Tunl = c("Tunel", "Tunls", "Tunnel", "Tunnels", "Tunnl"),
  Tpke = c("Trnpk", "Turnpike", "Turnpk"),
  Upas = "Underpass",
  Un = "Union",
  Uns = "Unions",
  Vly = c("Valley", "Vally", "Vlly"),
  Vlys = "Valleys",
  Via = c("Vdct", "Viadct", "Viaduct"),
  Vw = "View",
  Vws = "Views",
  Vlg = c("Vill", "Villag", "Village", "Villg", "Villiage"),
  Vlgs = "Villages",
  Vl = "Ville",
  Vis = c("Vist", "Vista", "Vst", "Vsta"),
  Walk = "Walks",
  Wall = "Wall",
  Way = "Wy",
  Ways = "Ways",
  Wl = "Well",
  Wls = "Wells"
)

valid_street_name_pre_types <- list(
  "Ave" = c("Ave", "Av", "Avenue"),
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
      warning(
        "street name post type parsed but not mapped: ",
        val,
        call. = FALSE
      )
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
