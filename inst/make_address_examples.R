# Maintainer preparation of the legacy-named address-string fixture.
# Uses only the already bundled, unrestricted USDOT NAD revision 23 fixture.
library(addr)

addresses <- unique(format(readRDS("inst/extdata/nad_39061.rds")$nad_addr))
stopifnot(length(addresses) >= 2000L)
saveRDS(addresses[seq_len(2000L)], "inst/extdata/voter_addresses.rds")
