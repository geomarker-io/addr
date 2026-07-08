addr_run_network_tests <- function() {
  tolower(Sys.getenv("ADDR_RUN_NETWORK_TESTS", unset = "")) %in%
    c("1", "true", "yes")
}

skip_live_tiger_downloads <- function() {
  testthat::skip_if_not(
    addr_run_network_tests(),
    "live TIGER download tests are disabled"
  )
  testthat::skip_if_offline()
  testthat::skip_on_cran()
}
