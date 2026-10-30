# Match two CSVs using their address columns.
library(addr)
library(dplyr, warn.conflicts = FALSE)

# Replace these example files with your own CSVs.
addresses <- read.csv("my_addresses.csv") |> as_tibble()
reference <- read.csv("reference_addresses.csv")
addresses
reference

# Parse the address columns, then join the tables.
addresses$addr <- as_addr(addresses$address)
reference$addr <- as_addr(reference$address)
matched <- addr_left_join(addresses, reference, by = "addr")

# Keep your original rows and the matched property IDs.
matched <- matched |> select(id, address = address.x, property_id)
matched

# Save a regular CSV for your next analysis step.
write.csv(matched, "matched_addresses.csv", row.names = FALSE)
