tag_map_collisions <- function(x) {
  token_map <- do.call(
    rbind,
    Map(function(canonical, aliases) {
      tokens <- unique(tolower(trimws(c(canonical, as.character(aliases)))))
      data.frame(
        token = tokens,
        canonical = canonical,
        stringsAsFactors = FALSE
      )
    }, names(x), x)
  )
  token_map <- unique(token_map)
  canonical_by_token <- split(token_map$canonical, token_map$token)
  collisions <- lapply(canonical_by_token, unique)
  collisions[lengths(collisions) > 1L]
}

test_that("tag maps have exclusive normalized input tokens", {
  no_collisions <- setNames(list(), character())
  expect_equal(tag_map_collisions(valid_street_name_post_types), no_collisions)
  expect_equal(tag_map_collisions(valid_street_name_pre_types), no_collisions)
  expect_equal(tag_map_collisions(valid_directions), no_collisions)
  expect_equal(tag_map_collisions(valid_states), no_collisions)
})

test_that("map_street_name_post_type maps variants and preserves blanks", {
  expect_warning(
    out <- map_street_name_post_type(c("foofy", "st", "lane")),
    "foofy"
  )
  expect_equal(out, c("foofy", "St", "Ln"))
  suppressWarnings(
    expect_equal(
      map_street_name_post_type(c("Avenue", "Avnue", "Blvrd", "", NA, "Woop")),
      c("Ave", "Ave", "Blvd", "", NA, "Woop")
    )
  )
})

test_that("map_street_name_post_type handles NULL and trims", {
  expect_equal(map_street_name_post_type(NULL), NA_character_)
  expect_equal(
    map_street_name_post_type(c(" Ave ", "Rd", "Riv ")),
    c("Ave", "Rd", "Riv")
  )
})

test_that("map_street_name_post_type maps observed official-source suffixes", {
  expect_equal(
    map_street_name_post_type(c("woods", "knls", "bl", "la", "ledge", "end")),
    c("Woods", "Knls", "Blvd", "Ln", "Ledge", "End")
  )
  expect_equal(
    map_street_name_post_type(c(
      "close",
      "cutoff",
      "acres",
      "taxiway",
      "access road",
      "overlook"
    )),
    c("Close", "Cutoff", "Acres", "Taxiway", "Access Road", "Overlook")
  )
})

test_that("map_street_name_pre_type maps variants and preserves blanks", {
  expect_equal(
    map_street_name_pre_type(c("US", "U.S.", "I-", "", NA, "Nope")),
    c("US Hwy", "US Hwy", "I-", "", NA, "Nope")
  )
})

test_that("map_street_name_pre_type handles NULL and trims", {
  expect_equal(map_street_name_pre_type(NULL), NA_character_)
  expect_equal(
    map_street_name_pre_type(c(" U.S. Hwy ", "Co Rd", "Rte ")),
    c("US Hwy", "Co Rd", "Rte")
  )
})

test_that("map_direction maps variants and preserves blanks", {
  expect_warning(
    out <- map_direction(c("North", "N.E.", "south west", "", NA, "Nope")),
    "nope"
  )
  expect_equal(out, c("N", "NE", "SW", "", NA, "Nope"))
})

test_that("map_direction handles NULL and trims", {
  expect_equal(map_direction(NULL), NA_character_)
  expect_equal(
    map_direction(c(" N ", "S.", "North-East ")),
    c("N", "S", "NE")
  )
})

test_that("map_state_to_abbrev handles names, abbreviations, and empty strings", {
  expect_equal(
    map_state_to_abbrev(c(
      "Ohio",
      "oh",
      "District of Columbia",
      "PR",
      "foo",
      "",
      NA
    )),
    c("OH", "OH", "DC", "PR", NA, "", NA)
  )

  expect_equal(
    map_state_to_abbrev(c("  new york ", "U.S. Virgin Islands")),
    c("NY", "VI")
  )
})
