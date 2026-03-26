devtools::load_all(compile = FALSE)
if (file.exists("inst/nad_39061.rds")) {
  out <- readRDS("inst/nad_39061.rds")
} else {
  out <- nad_read("Hamilton", "OH")
}
saveRDS(out, "inst/nad_39061.rds")
saveRDS(addr_match_prepare(out$nad_addr), "inst/nad_39061_match_prepared.rds")
