script <- Sys.getenv(
  "ADDR_GEOCODE_CLI_SCRIPT",
  unset = file.path(getwd(), "exec", "addr-geocode")
)
if (!file.exists(script)) {
  stop("addr-geocode CLI script does not exist: ", script, call. = FALSE)
}

assert <- function(x, message) {
  if (!isTRUE(x)) {
    stop(message, call. = FALSE)
  }
}

expect_error <- function(expr, pattern) {
  err <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  if (is.null(err)) {
    stop("expected an error", call. = FALSE)
  }
  if (!grepl(pattern, conditionMessage(err))) {
    stop(
      sprintf(
        "error did not match %s: %s",
        pattern,
        conditionMessage(err)
      ),
      call. = FALSE
    )
  }
  invisible(err)
}

Sys.setenv(ADDR_GEOCODE_CLI_SOURCE_ONLY = "1")
source(script)

tmp <- tempfile("addr-geocode-cli-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

opts <- addr_geocode_cli_parse_args(c(
  "--input",
  "addresses.csv",
  "--workers",
  "2",
  "--preset=strict",
  "--overwrite"
))
assert(identical(opts$input, "addresses.csv"), "input option was not parsed")
assert(identical(opts$workers, 2L), "workers option was not parsed")
assert(identical(opts$preset, "strict"), "preset option was not parsed")
assert(isTRUE(opts$overwrite), "overwrite option was not parsed")

presets <- addr_geocode_cli_presets()
assert(
  identical(names(presets), c("default", "strict", "exact-zip", "loose")),
  "preset names are wrong"
)
assert(length(presets$default) == 0L, "default preset must not override geocode")
assert(
  identical(
    presets$strict,
    list(
      name_phonetic_dist = 0L,
      name_fuzzy_dist = 0L,
      place_zip_variants = FALSE,
      zip_variants = FALSE
    )
  ),
  "strict preset arguments are wrong"
)
assert(
  identical(
    presets[["exact-zip"]],
    list(place_zip_variants = FALSE, zip_variants = FALSE)
  ),
  "exact-zip preset arguments are wrong"
)
assert(
  identical(
    presets$loose,
    list(
      name_fuzzy_dist = 3L,
      match_street_type = "ignore",
      match_street_directional = "ignore"
    )
  ),
  "loose preset arguments are wrong"
)

usage <- addr_geocode_cli_usage()
for (preset in names(presets)) {
  assert(
    grepl(preset, usage, fixed = TRUE),
    paste("help does not describe preset:", preset)
  )
}
assert(
  grepl(
    "https://geomarker.io/addr/reference/geocode.html",
    usage,
    fixed = TRUE
  ),
  "help does not link to the geocode reference"
)

expect_error(
  addr_geocode_cli_parse_args(character()),
  "--input is required"
)
expect_error(
  addr_geocode_cli_parse_args(c(
    "--input",
    "x.csv",
    "--address-column",
    "addr"
  )),
  "unknown option"
)
expect_error(
  addr_geocode_cli_parse_args(c(
    "--input",
    "x.csv",
    "--preset",
    "unknown"
  )),
  "--preset must be one of"
)
expect_error(
  addr_geocode_cli_parse_args(c(
    "--input",
    "x.csv",
    "--taf-year",
    "2025"
  )),
  "unknown option"
)

out_path <- addr_geocode_cli_output_path(
  file.path(tmp, "addresses.csv"),
  version = "1.2.0"
)
assert(
  identical(
    basename(out_path),
    "addresses__addr-v1.2.0__preset-default__geocoded.csv"
  ),
  "csv output path was not deterministic"
)

parquet_out_path <- addr_geocode_cli_output_path(
  file.path(tmp, "addresses.parquet"),
  version = "1.2.0",
  preset = "strict"
)
assert(
  identical(
    basename(parquet_out_path),
    "addresses__addr-v1.2.0__preset-strict__geocoded.parquet"
  ),
  "parquet output path was not deterministic"
)

input <- data.frame(
  id = 1:2,
  address = c("10 Main St Cincinnati OH 45220", "No ZIP"),
  stringsAsFactors = FALSE
)
addr_geocode_cli_validate_input(input)

expect_error(
  addr_geocode_cli_validate_input(data.frame(id = 1L)),
  "column named exactly `address`"
)
expect_error(
  addr_geocode_cli_validate_input(data.frame(
    address = "x",
    addr_geocode_stage = "none"
  )),
  "output column"
)

csv_path <- file.path(tmp, "addresses.csv")
utils::write.csv(input, csv_path, row.names = FALSE)
csv_in <- addr_geocode_cli_read(csv_path)
assert(identical(names(csv_in), names(input)), "csv names were not preserved")
csv_out <- addr_geocode_cli_output_path(csv_path, version = "1.2.0")
addr_geocode_cli_write(
  cbind(csv_in, addr_geocode_cli_empty_output(nrow(csv_in))),
  csv_out
)
assert(file.exists(csv_out), "csv output was not written")

if (requireNamespace("nanoparquet", quietly = TRUE)) {
  parquet_path <- file.path(tmp, "addresses.parquet")
  nanoparquet::write_parquet(input, parquet_path)
  parquet_in <- addr_geocode_cli_read(parquet_path)
  assert(
    identical(names(parquet_in), names(input)),
    "parquet names were not preserved"
  )
  parquet_out <- addr_geocode_cli_output_path(parquet_path, version = "1.2.0")
  addr_geocode_cli_write(
    cbind(parquet_in, addr_geocode_cli_empty_output(nrow(parquet_in))),
    parquet_out
  )
  assert(file.exists(parquet_out), "parquet output was not written")
}

gcd <- tibble::tibble(
  addr = addr::as_addr(c(
    "10 Main St Cincinnati OH 45220",
    "11 Oak Rd Cincinnati OH 45221",
    NA_character_
  )),
  matched_zipcode = c("45220", "45221", NA_character_),
  matched_street = addr::addr_street(
    name = c("Main", "Oak", NA_character_),
    posttype = c("St", "Rd", NA_character_)
  ),
  matched_geography = s2::as_s2_geography(c(
    "POINT (-84.5 39.1)",
    NA_character_,
    NA_character_
  )),
  s2_cell = s2::as_s2_cell(s2::as_s2_geography(c(
    "POINT (-84.5 39.1)",
    NA_character_,
    NA_character_
  )))
)
flat <- addr_geocode_cli_flatten(gcd)
assert(
  identical(
    names(flat),
    c(
      "addr_geocode_stage",
      "addr_matched_zipcode",
      "addr_matched_street",
      "addr_longitude",
      "addr_latitude",
      "addr_s2_cell"
    )
  ),
  "flattened output names are wrong"
)
assert(
  identical(flat$addr_geocode_stage, c("range", "street", "none")),
  "geocode stage flattening failed"
)
assert(identical(flat$addr_longitude[[1]], -84.5), "longitude was not added")
assert(is.na(flat$addr_latitude[[2]]), "missing latitude was not preserved")

run_input <- file.path(tmp, "local.csv")
utils::write.csv(
  data.frame(id = 1:2, address = c(NA_character_, NA_character_)),
  run_input,
  row.names = FALSE,
  na = ""
)
Sys.unsetenv("ADDR_GEOCODE_CLI_SOURCE_ONLY")
run_preset <- if (
  "place_zip_variants" %in% names(formals(addr::geocode))
) {
  "strict"
} else {
  "default"
}
run_out <- system2(
  script,
  c(
    "--input",
    run_input,
    "--preset",
    run_preset,
    "--data-dir",
    file.path(tmp, "addr-data")
  ),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(run_out, "status")
if (is.null(status)) {
  status <- 0L
}
assert(identical(status, 0L), paste(run_out, collapse = "\n"))
assert(
  any(grepl("preparing geocoding input", run_out, fixed = TRUE)),
  "CLI invocation did not emit geocode progress"
)
expected_run_output <- addr_geocode_cli_output_path(
  run_input,
  preset = run_preset
)
assert(file.exists(expected_run_output), "CLI invocation failed")

cat("addr-geocode CLI tests passed\n")
