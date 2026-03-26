devtools::load_all()

d <-
  readr::read_csv(
    "https://evictionlab.org/uploads/cincinnati_hotspots_media_report.csv",
    show_col_types = FALSE
  )

saveRDS(d, file.path("inst", "extdata", "elh_data.rds"))
