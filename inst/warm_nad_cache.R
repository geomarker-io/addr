devtools::load_all()

d <- nad("Hamilton", "OH", refresh = "force")
saveRDS(d, file.path("inst", "extdata", "nad_39061.rds"))
saveRDS(
  addr_match_prepare(d$nad_addr),
  file.path("inst", "extdata", "nad_39061_mp.rds")
)
