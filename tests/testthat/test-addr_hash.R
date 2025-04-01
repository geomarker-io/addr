test_that("addr_hash works", {
  expect_equal(
    addr_hash(c("3333 Burnet Ave Cincinnati OH 45229", "1324 Burnet Ave Cincinnati OH 45229")),
    c("7b45711615560c1b4f1814ce99337d24", "8d4f27952b95c0a151b5c68f0a0adc93")
  )
  expect_equal(
    addr_hash("3333 Burnet Ave Cincinnati OH 45229"),
    addr_hash("3333 burnet Avenue Cincinnati OH 45229")
  )
})
