devtools::load_all()

d <- nad("Hamilton", "OH", refresh = "force", release = "NAD_r22.zip")
saveRDS(d, file.path("inst", "extdata", "nad_39061.rds"))
saveRDS(
  addr_match_prepare(d$nad_addr),
  file.path("inst", "extdata", "nad_39061_mp.rds")
)
