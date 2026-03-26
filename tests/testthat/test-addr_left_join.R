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
  expect_equal(
    addr_match_stage(matched),
    ordered("street", levels = c("none", "zip", "street", "number"))
  )
  expect_true(is.na(out$id.y))
  expect_true(is.na(out$addr.y))
})

test_that("addr_left_join forwards addr_match tuning arguments", {
  demo_addr <- function(name, pretype = "", postdirectional = "") {
    addr(
      addr_number(prefix = "", digits = "10", suffix = ""),
      addr_street(
        predirectional = "",
        premodifier = "",
        pretype = pretype,
        name = name,
        posttype = "Rd",
        postdirectional = postdirectional,
        map_pretype = FALSE,
        map_posttype = FALSE,
        map_directional = FALSE,
        map_ordinal = FALSE
      ),
      addr_place(name = "Testville", state = "OH", zipcode = "45220")
    )
  }

  x <- tibble::tibble(
    id = 1L,
    addr = demo_addr("Mian", pretype = "US Hwy", postdirectional = "E")
  )
  y <- tibble::tibble(
    addr = vctrs::vec_c(
      demo_addr("Main"),
      demo_addr("Main", pretype = "US Hwy", postdirectional = "E")
    ),
    y_id = c(101L, 202L)
  )

  out_pretype_optional <- addr_left_join(
    x,
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_pretype = FALSE,
    progress = FALSE
  )
  out_default <- addr_left_join(
    x,
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    progress = FALSE
  )
  out_required <- addr_left_join(
    x,
    y,
    name_phonetic_dist = 0L,
    name_fuzzy_dist = 1L,
    match_street_postdirectional = TRUE,
    progress = FALSE
  )

  expect_equal(out_pretype_optional$y_id, 101L)
  expect_equal(out_default$y_id, 202L)
  expect_equal(out_required$y_id, 202L)
})
