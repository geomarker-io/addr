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
