test_that("addr_left_join expands duplicate matched rows in y", {
  x <- tibble::tibble(
    id = 1:2,
    addr = as_addr(c(
      "10 MAIN ST CINCINNATI OH 45220",
      "20 MAIN ST CINCINNATI OH 45220"
    ))
  )
  y <- tibble::tibble(
    addr = as_addr(c(
      "10 MAIN ST CINCINNATI OH 45220",
      "10 MAIN ST CINCINNATI OH 45220",
      "20 MAIN ST CINCINNATI OH 45220"
    )),
    id = c(101, 102, 201)
  )

  out <- addr_left_join(x, y, progress = FALSE)

  expect_equal(names(out), c("id.x", "addr", "addr.y", "id.y"))
  expect_equal(nrow(out), 3L)
  expect_equal(out$id.x, c(1, 1, 2))
  expect_equal(out$id.y, c(101, 102, 201))
  expect_equal(
    format(out$addr.y),
    c(
      "10 MAIN St CINCINNATI OH 45220",
      "10 MAIN St CINCINNATI OH 45220",
      "20 MAIN St CINCINNATI OH 45220"
    )
  )
})

test_that("addr_left_join does not expand partial matches to multiple y rows", {
  x <- tibble::tibble(
    id = 1L,
    addr = as_addr("99 MAIN ST CINCINNATI OH 45220")
  )
  y <- tibble::tibble(
    addr = as_addr(c(
      "10 MAIN ST CINCINNATI OH 45220",
      "11 MAIN ST CINCINNATI OH 45220"
    )),
    id = c(101, 102)
  )

  out <- addr_left_join(x, y, progress = FALSE)
  matched <- addr_match(x$addr, y$addr, progress = FALSE)

  expect_equal(nrow(out), 1L)
  expect_equal(addr_match_stage(matched), ordered("street",
    levels = c("none", "zip", "street", "number")
  ))
  expect_true(is.na(out$id.y))
  expect_true(is.na(out$addr.y))
})
