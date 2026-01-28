library(dplyr)
library(readr)

# might have to update the date in the url to get the download to work
rd <- read_csv(
  "https://votehamiltoncountyohio.gov/download.php?file=VoterListExport-20240912-no.csv",
  col_types = cols_only(
    AddressPreDirectional = col_character(),
    AddressNumber = col_double(),
    AddressStreet = col_character(),
    AddressSuffix = col_character(),
    CityName = col_character(),
    AddressZip = col_character(),
  )
)

d <-
  rd |>
  dplyr::mutate(
    voter_address = paste(
      AddressPreDirectional,
      AddressNumber,
      AddressStreet,
      AddressSuffix,
      CityName,
      "OH",
      AddressZip
    ),
    .keep = "unused"
  )

# remove missing address components left in the paste
d$address <- gsub("NA ", "", d$voter_address, fixed = TRUE)

out <- unique(d$address)

length(out)

saveRDS(out, fs::path("inst", "voter_addresses.rds"))
