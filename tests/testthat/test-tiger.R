test_that("can download files from tiger", {
  skip_if_offline()
  skip_on_cran()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  dl_file <- tiger_download(
    "TIGER2024/INTERNATIONALBOUNDARY/tl_2024_us_internationalboundary.zip"
  )
  expect_true(file.exists(dl_file))
})

test_that("tiger_addr_feat() can download addr feat from tiger", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  withr::local_envvar(list("R_USER_DATA_DIR" = tempfile()))
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})

test_that("tiger_addr_feat() works with existing user data dir", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()
  d <- tiger_addr_feat(county = "39061", year = "2024")
  expect_s3_class(d, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_true(nrow(d) > 0)
  expect_true(all(c("LINEARID", "FULLNAME", "ZIP") %in% names(d)))
  expect_s3_class(d$s2_geography, "s2_geography")
})
