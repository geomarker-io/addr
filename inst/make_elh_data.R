devtools::load_all()
library(dplyr, warn.conflicts = FALSE)

d <-
  readr::read_csv(
    "https://evictionlab.org/uploads/cincinnati_hotspots_media_report.csv",
    show_col_types = FALSE
  )

saveRDS(d, "inst/elh_data.rds")
