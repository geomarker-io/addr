test_that("addr_join works", {
  my_addr <-
    tibble::tibble(
      id = 1:3,
      addr = as_addr(c(
        "222 E Central Parkway Cincinnati OH 45000",
        "222 East Central Parkway Cincinnati OH 45000",
        "222 East Central Pkwy Cincinnati OH 45000"
      ))
    )
  the_addr <- tibble::tibble(
    addr = as_addr("222 E CENTRAL PKWY CINCINNATI OH 45000"),
    id = 1
  )

  out <- addr_fuzzy_left_join(my_addr, the_addr)

  expect_equal(names(out), c("id.x", "addr", "addr.y", "id.y"))
  expect_equal(nrow(out), 3)
  expect_equal(out$id.y, c(1, 1, 1))
})


test_that("addr_match with NAD", {
  skip("addr_fuzzy_left_join is deprecated")
  my_addresses <- c(
    "781 GREENWOOD AVE APT 1 CINCINNATI OHIO 45229",
    "781 GREENWOOD AV CINCINNATI OHIO 45229",
    "515 FOREST AVE CINCINNATI OHIO 45229",
    "1540 DUDLEY WALK APT F CINCINNATI OHIO 45214",
    "3333 BURNET AVE CINCINNATI OH 45219", # wrong zipcode
    "3333 BURNET AVE CINCINNATI OH 45229",
    "806 BLAIR AVE APT 12 CINCINNATI OHIO 45229",
    "300 OAK CREEK CT 13 FAIRFIELD OHIO 45014",
    "5130 RAPID RUN RD CINCINNATI OHIO 45238",
    "5131 RAPID RUN RD CINCINNATI OHIO 45238", # off by one street number
    "2583 RIVERSIDE DR CINCINNATI OHIO 45202",
    "7839 DAWN RD APT 6 CINCINNATI OHIO 45237",
    "222 EAST CENTRAL PKWY CINCINNATI OH 45202", # JFS
    "222 E CENTRAL PKWY CINCINNATI OHIO 45202", # JFS
    "222 CENTRAL PKWY CINCINNATI OHIO 45202", # missng the East
    "4571 TIMBERLAKE DR BATAVIA OHIO 45103", # outside reference addr zip codes
    "31 HIGHRIDGE DR LOVELAND OHIO 45140",
    "117 E 12TH ST CINCINNATI, OH 45202" # Greater Cinti Coalition for The Homeless
  )

  my_addr <- tibble::tibble(
    my_addresses = my_addresses,
    addr = as_addr(my_addresses)
  )

  the_addr <- nad_example_data()

  cagis_matches <- addr_fuzzy_left_join(
    my_addr,
    the_addr,
    by = c("addr", "nad_addr")
  )

  expect_s3_class(cagis_matches, c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(cagis_matches$nad_addr.y, c("addr"))
  expect_equal(sum(is.na(cagis_matches$uuid)), 5)
  expect_equal(cagis_matches[1, "uuid"], cagis_matches[2, "uuid"])
  expect_equal(nrow(cagis_matches), 18)

  expect_true(all(my_addresses %in% cagis_matches$my_addresses))
})
