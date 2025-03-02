test_that("addr_match_street() works", {
  addr_match_street(
    addr(c("224 Woolper Ave Cincinnati OH 45220", "123 Nain Street Cincinnati OH 45123", "3333 Burnet Ave Cincinnati OH 45219")),
    addr(c("Woolper Ave", "Main Street", "Burnet Ave", "Bulnet Ave"))
  ) |>
    expect_identical(list(1L, 2L, 3L))

  addr_match_street(
    addr(c("224 Woolper Ave Cincinnati OH 45220", "123 Nain Street Cincinnati OH 45123", "3333 Burnet Ave Cincinnati OH 45219")),
    addr(c("Woolper Ave", "Main Street", "Burnet Ave", "Bulnet Ave")),
    stringdist_match = "exact"
  ) |>
    expect_identical(list(1L, integer(0), 3L))

  addr_match_street(
    addr(c("224 Woolper St Cincinnati OH 45220", "123 Nain Street Cincinnati OH 45123", "3333 Burnet Ave Cincinnati OH 45219")),
    addr(c("Woolper Ave", "Main Street", "Burnet Ave", "Bulnet Ave")),
    match_street_type = "none"
  ) |>
    expect_identical(list(1L, 2L, 3L))

  addr_match_street(
    addr(c("224 Woolper Ave Cincinnati OH 45220", "123 Nain Street Cincinnati OH 45123", "3333 Burnet Ave Cincinnati OH 45219")),
    addr(c("Woolper Ave", "Main Street", "Nain Street", "Burnet Ave", "Bulnet Ave"))
  ) |>
    expect_identical(list(1L, 3L, 4L))

  addr_match_street(
    addr(c("224 Woolper Ave Cincinnati OH 45220", "123 Nain Street Cincinnati OH 45123", "3333 Burnet Ave Cincinnati OH 45219")),
    addr(c("Woolper Ave", "Main Street", "Burnet Ave", "Bulnet Ave")),
    stringdist_match = "exact"
  ) |>
    expect_identical(list(1L, integer(0), 3L))
})

test_that("addr_match_street_name_and_number works", {
  as_addr(c(
    "222 E Central Parkway Foofyville SJ 00000",
    "222 East Central Parkway",
    "221 E Central Parkway Somewhere OS 00000",
    "222 East Central Cincinnati"
  )) |>
    addr_match_street_name_and_number(as_addr(c("222 E CENTRAL PKWY", "221 E CENTRAL PKWY", "222 CENTRAL PKWY", "222 E CENTRAL PKWY")),
                                      simplify = FALSE) |>
    sapply(length) |>
    expect_identical(c(
      `222 E Central Parkway Foofyville SJ 00000` = 2L, `222 E Central Parkway` = 2L,
      `221 E Central Parkway Somewhere OS 00000` = 1L, `222 E Central Cincinnati` = 0L
    ))
})

test_that("addr_match works", {

  addr_match(
    addr(c(
      "222 E Central Parkway Cincinnati OH 45000",
      "222 East Central Parkway Cincinnati OH 45000",
      "222 East Central Pkwy Cincinnati OH 45000"
    )),
    addr("222 E CENTRAL PKWY CINCINNATI OH 45000")
  ) |>
    unique() |>
    expect_equal(addr("222 E CENTRAL PKWY CINCINNATI OH 45000"))

  addr_match(
    addr(c(
      "14 E 14th street cincinnati oh 45000", "14 east 14th street cincinnati oh 45000", "14 W 14th street cincinnati oh 45000",
      "3333 Burnet Ave cincinnati oh 45000"
    )),
    addr(c("14 E 14TH STREET CINCINNATI OH 45000", "14 W 14TH STREET CINCINNATI OH 45000")),
    simplify = FALSE
  ) |>
    expect_equal(
      list(`14 E 14th Street Cincinnati OH 45000` = structure(list(
        street_number = 14, street_name = "e 14th", street_type = "street",
        city = "cincinnati", state = "oh", zip_code = "45000"
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )), `14 E 14th Street Cincinnati OH 45000` = structure(list(
        street_number = 14, street_name = "e 14th", street_type = "street",
        city = "cincinnati", state = "oh", zip_code = "45000"
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )), `14 W 14th Street Cincinnati OH 45000` = structure(list(
        street_number = 14, street_name = "w 14th", street_type = "street",
        city = "cincinnati", state = "oh", zip_code = "45000"
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )), `3333 Burnet Avenue Cincinnati OH 45000` = structure(list(
        street_number = numeric(0), street_name = character(0), street_type = character(0),
        city = character(0), state = character(0), zip_code = character(0)
      ), class = c(
        "addr",
        "vctrs_rcrd", "vctrs_vctr"
      )))
    )
})


test_that("addr_match with cagis works", {

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

  cagis_matches <- addr_match(as_addr(my_addresses), cagis_addr()$cagis_addr, simplify = FALSE)

  sapply(cagis_matches, length) |>
    expect_equal(
      c(
        `781 Greenwood Avenue Cincinnati OHIO 45229` = 1L,
        `781 Greenwood Avenue Cincinnati OHIO 45229` = 1L,
        `515 Forest Avenue Cincinnati OHIO 45229` = 1L,
        `1540 Dudley Walk Cincinnati OHIO 45214` = 1L,
        `3333 Burnet Avenue Cincinnati OH 45219` = 0L,
        `3333 Burnet Avenue Cincinnati OH 45229` = 1L,
        `806 Blair Avenue Cincinnati OHIO 45229` = 1L,
        `300 Oak Creek Court Fairfield OHIO 45014` = 0L,
        `5130 Rapid Run Road Cincinnati OHIO 45238` = 1L,
        `5131 Rapid Run Road Cincinnati OHIO 45238` = 0L,
        `2583 Riverside Drive Cincinnati OHIO 45202` = 1L,
        `7839 Dawn Road Cincinnati OHIO 45237` = 1L,
        `222 E Central Parkway Cincinnati OH 45202` = 1L,
        `222 E Central Parkway Cincinnati OHIO 45202` = 1L,
        `222 Central Parkway Cincinnati OHIO 45202` = 0L,
        `4571 Timberlake Drive Batavia OHIO 45103` = 0L,
        `31 Highridge Drive Loveland OHIO 45140` = 1L,
        `117 E 12th Street Cincinnati OH 45202` = 1L
      )
    )

  expect_identical(cagis_matches[[16]], NULL)

  dd <- addr_match(as_addr(my_addresses), cagis_addr()$cagis_addr, simplify = TRUE)

  which(is.na(dd)) |>
    expect_identical(c(5L, 8L, 10L, 15L, 16L))
  expect_identical(
    dd,
    structure(list(street_number = c(
      781, 781, 515, 1540, NA, 3333,
      806, NA, 5130, NA, 2583, 7839, 222, 222, NA, NA, 31, 117
    ), street_name = c(
      "greenwood",
      "greenwood", "forest", "dudley", NA, "burnet", "blair", NA, "rapid run",
      NA, "riverside", "dawn", "e central", "e central", NA, NA, "highridge",
      "e 12th"
    ), street_type = c(
      "avenue", "avenue", "avenue", "walk",
      NA, "avenue", "avenue", NA, "road", NA, "drive", "road", "parkway",
      "parkway", NA, NA, "drive", "street"
    ), city = c(
      "cincinnati",
      "cincinnati", "cincinnati", "cincinnati", NA, "cincinnati", "cincinnati",
      NA, "delhi township", NA, "cincinnati", "cincinnati", "cincinnati",
      "cincinnati", NA, NA, "loveland", "cincinnati"
    ), state = c(
      "oh",
      "oh", "oh", "oh", NA, "oh", "oh", NA, "oh", NA, "oh", "oh", "oh",
      "oh", NA, NA, "oh", "oh"
    ), zip_code = c(
      "45229", "45229", "45229",
      "45214", NA, "45229", "45229", NA, "45238", NA, "45202", "45237",
      "45202", "45202", NA, NA, "45140", "45202"
    )), class = c(
      "addr",
      "vctrs_rcrd", "vctrs_vctr"
    ))
  )
})
