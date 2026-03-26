devtools::load_all(compile = FALSE)
if (file.exists(file.path("inst", "extdata", "nad_39061.rds"))) {
  out <- readRDS(file.path("inst", "extdata", "nad_39061.rds"))
} else {
  out <- nad_read("Hamilton", "OH")
}
saveRDS(out, file.path("inst", "extdata", "nad_39061.rds"))
saveRDS(
  addr_match_prepare(out$nad_addr),
  file.path("inst", "extdata", "nad_39061_match_prepared.rds")
)
