test_that("addr() works", {
  addr(c(
    "290 Ludlow Avenue Apt #2 Cincinnati OH 45220",
    "200 W 14th Street Cincinnati OH 45222",
    "3333 Burnet Ave Cincinnati OH 45219",
    "111 State Route 32 Cincinnati OH 45912",
    "202 Riva Ridge Ct Cincinnati OH 45140"
  )) |>
    expect_s3_class("addr") |>
    as.data.frame() |>
    expect_equal(structure(
      list(
        street_number = c(290, 200, 3333, 111, 202),
        street_name = c(
          "ludlow",
          "w 14th",
          "burnet",
          "state route 32",
          "riva ridge"
        ),
        street_type = c("avenue", "street", "avenue", NA, "court"),
        city = c(
          "cincinnati",
          "cincinnati",
          "cincinnati",
          "cincinnati",
          "cincinnati"
        ),
        state = c("oh", "oh", "oh", "oh", "oh"),
        zip_code = c("45220", "45222", "45219", "45912", "45140")
      ),
      class = "data.frame",
      row.names = c(
        NA,
        -5L
      )
    ))
})

test_that("as_addr() works", {
  as_addr(c(
    "290 Ludlow Avenue Apt #2 Cincinnati OH 45220",
    "290 Ludlow Avenue Apt #2 Cincinnati OH 45220",
    "290 Ludlow Avenue Apt #2 Cincinnati OH 45220",
    "200 W 14th Street Cincinnati OH 45222",
    "3333 Burnet Ave Cincinnati OH 45219",
    "111 State Route 32 Cincinnati OH 45912",
    "202 Riva Ridge Ct Cincinnati OH 45140"
  )) |>
    expect_s3_class("addr") |>
    as.data.frame() |>
    expect_equal(structure(
      list(
        street_number = c(290, 290, 290, 200, 3333, 111, 202),
        street_name = c(
          "ludlow",
          "ludlow",
          "ludlow",
          "w 14th",
          "burnet",
          "state route 32",
          "riva ridge"
        ),
        street_type = c(
          "avenue",
          "avenue",
          "avenue",
          "street",
          "avenue",
          NA,
          "court"
        ),
        city = c(
          "cincinnati",
          "cincinnati",
          "cincinnati",
          "cincinnati",
          "cincinnati",
          "cincinnati",
          "cincinnati"
        ),
        state = c("oh", "oh", "oh", "oh", "oh", "oh", "oh"),
        zip_code = c(
          "45220",
          "45220",
          "45220",
          "45222",
          "45219",
          "45912",
          "45140"
        )
      ),
      class = "data.frame",
      row.names = c(
        NA,
        -7L
      )
    ))
})

test_that("addr can cast to character", {
  addr(c(
    "290 Ludlow Avenue Apt #2 Cincinnati Ohio 45220",
    "200 W 14th Street Cincinnati OH 45222",
    "3333 Burnet Ave Cincinnati OH 45229",
    "111 State Route 32 Cincinnati OH 45912",
    "202 Riva Ridge Ct Cincinnati OH 45140"
  )) |>
    as.character() |>
    expect_equal(c(
      "290 Ludlow Avenue Cincinnati OHIO 45220",
      "200 W 14th Street Cincinnati OH 45222",
      "3333 Burnet Avenue Cincinnati OH 45229",
      "111 State Route 32 Cincinnati OH 45912",
      "202 Riva Ridge Court Cincinnati OH 45140"
    ))

  addr("3319 Main Street Anytown MN 45.355") |>
    as.character() |>
    expect_equal("3319 Main Street Anytown MN 45355")
})

test_that("addr abbreviates cardinal directions", {
  addr(c(
    "222 east Central Parkway Cincinnati OH 45000",
    "222 East Central Parkway Cincinnati OH 45000",
    "222 EAST Central Parkway Cincinnati OH 45000",
    "222 E Central Parkway Cincinnati OH 45000",
    "222 e Central Parkway Cincinnati OH 45000"
  )) |>
    as.character() |>
    expect_equal(rep("222 E Central Parkway Cincinnati OH 45000", 5))
})

test_that("addr deals with non-numeric street number address", {
  addr("1234B Main St Cincinnati OH 45000") |>
    expect_equal(addr("1234 Main St Cincinnati OH 45000"))
})
