#' Geocode addr vectors with Census TIGER address features
#'
#' @description
#' `geocode()` geocodes addr vectors using Census TIGER address
#' features (see `?taf`) by:
#' 1. searching for a matching street (see `?match_addr_street`),
#' within the same ZIP code, also searching similar ZIP codes for a matching
#' street if necessary
#' 2. using the address number to select the best address feature range and
#' side of the street (even/odd), breaking ties on smallest width and spread
#' 3. linearly interpolating a geographic point along the best range line based
#' on the actual and potential range of address numbers
#' 4. offsetting the interpolated point from the range line perpendicularly
#'
#' Only matched input addresses return non-missing matched ZIP code and street
#' values. Missing or unmatched ZIP codes return missing matched ZIP code,
#' street, geography, and s2 cell values. If all ranges on the matched ZIP code
#' and street exclude the address number, only the geography and s2 cell values
#' return `NA`.
#' @param x an addr vector (`?as_addr`)
#' @param offset number of meters to offset geocode from street line
#' @param progress logical; show progress messages and a ZIP-code progress bar
#'   while geocoding?
#' @param taf_install logical; install missing county TAF files needed for
#'   input ZIP codes and selected ZIP code variants before geocoding? If
#'   `FALSE`, geocoding proceeds with installed files only and warns when
#'   needed county files are missing.
#' @param taf_redownload logical; re-download cached TIGER ZIP files when
#'   installing missing TAF counties?
#' @inheritParams match_addr_street
#' @inheritParams match_zipcodes
#' @inheritParams taf
#' @returns A tibble with columns `addr` (the input addr vector),
#'   `matched_zipcode` (character vector), `matched_street` (`addr_street`
#'   vector), `matched_geography` (`s2_geography` point vector), and `s2_cell`
#'   (`s2_cell` vector).
#' @details
#' `geocode_zip()` is the workhorse function and operates on addr vectors
#' with the same ZIP code; use `geocode()` to geocode an addr vector
#' with multiple ZIP codes by grouping them by ZIP code and processing
#' serially by default.
#' At a lower level, grouping addr vectors by ZIP code and applying
#' `geocode_zip()` facilitates more control (e.g., parallel processing).
#' Before ZIP grouping, `geocode()` deduplicates formatted `addr` values
#' internally and restores the output to the original input order and length.
#' Exact duplicates therefore do not trigger repeated TAF reads, street
#' matching, or range interpolation, so callers usually do not need to call
#' `unique()` themselves for geocoding performance.
#'
#' If the mirai package is installed and mirai daemons have already been
#' configured by the caller, `geocode()` uses them for ZIP-code-level
#' parallel processing. Otherwise it falls back to sequential processing.
#'
#' `geocode()` and `geocode_zip()` both download and install tiger address
#' features by county (`?taf_install`) as needed based on the input addr ZIP
#' codes (and possibly ZIP code variants). TAF install checks run before
#' reading TAF ZIP files so parallel geocoding workers do not try to download
#' county files at the same time.
#'
#' @export
#' @examples
#' x <- as_addr(voter_addresses()[1:100])
#'
#' # for example purposes, only install one county
#' Sys.setenv("R_USER_DATA_DIR" = tempfile())
#' taf_install("39061", "2025")
#' # and geocode without installing other counties
#' gcd <- geocode(x, taf_install = FALSE)
#'
#' # this is only for example purposes and usually not required; e.g.
#'
#' \dontrun{
#'   gcd <- geocode(x)
#' }
#'
#' gcd
#'
#' table(geocode_stage(gcd))
#'
#' geocode_table(gcd)
#'
#' leaflet::leaflet(wk::wk_coords(gcd$matched_geography)) |>
#'   leaflet::addTiles() |>
#'   leaflet::addCircleMarkers(lng = ~x, lat = ~y, label = ~feature_id)
#'
#' # use mirai for parallel processing
#' \dontrun{
#'   mirai::daemons(2)
#'   geocode(x)
#'   mirai::daemons(0)
#' }
geocode <- function(
  x,
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_type = c("exact", "compatible", "ignore"),
  match_street_directional = c("exact", "swap", "ignore"),
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  year = as.character(2025:2011),
  version = "v1",
  taf_install = TRUE,
  taf_redownload = FALSE,
  offset = 10L,
  progress = interactive()
) {
  stopifnot(
    "x must be an addr vector" = inherits(x, "addr"),
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants),
    "taf_install must be TRUE or FALSE" = is.logical(taf_install) &&
      length(taf_install) == 1L &&
      !is.na(taf_install),
    "taf_redownload must be TRUE or FALSE" = is.logical(taf_redownload) &&
      length(taf_redownload) == 1L &&
      !is.na(taf_redownload),
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version),
    "progress must be TRUE or FALSE" = is.logical(progress) &&
      length(progress) == 1L &&
      !is.na(progress)
  )
  year <- match.arg(year)
  zip_variant <- validate_zip_variant(zip_variant)
  validate_geocode_offset(offset)
  match_args <- validate_match_addr_street_args(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_type = match_street_type,
    match_street_directional = match_street_directional
  )
  if (progress) {
    geocode_progress_message(
      "preparing geocoding input (",
      geocode_format_count(length(x)),
      " addr)"
    )
  }
  xu <- unique(x)
  missing_zip <- is.na(xu@place@zipcode) | xu@place@zipcode == ""
  if (any(!missing_zip)) {
    if (progress) {
      geocode_progress_message(
        geocode_prepare_taf_progress_text(
          xu[!missing_zip],
          taf_install = taf_install,
          zip_variants = zip_variants
        )
      )
    }
    geocode_prepare_taf(
      xu[!missing_zip],
      year = year,
      version = version,
      zip_variants = zip_variants,
      zip_variant = zip_variant,
      taf_install = taf_install,
      taf_redownload = taf_redownload
    )
    if (progress) {
      geocode_progress_message("TIGER address feature file check complete")
    }
  }
  z_list <- split(xu[!missing_zip], xu[!missing_zip]@place@zipcode)
  if (progress) {
    geocode_progress_message(geocode_group_progress_text(z_list))
  }

  gcd <- geocode_map(
    z_list,
    geocode_zip,
    progress = progress,
    offset = offset,
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_type = match_args$match_street_type,
    match_street_directional = match_args$match_street_directional,
    zip_variants = zip_variants,
    zip_variant = zip_variant,
    year = year,
    version = version,
    taf_install = FALSE,
    taf_redownload = taf_redownload,
    taf_check = FALSE
  )
  if (any(missing_zip)) {
    if (progress) {
      geocode_progress_message(
        "adding no-match results for ",
        geocode_addr_count_text(sum(missing_zip)),
        " without ZIP codes"
      )
    }
    gcd <- c(gcd, list(missing_zip = geocode_no_match(xu[missing_zip])))
  }
  if (length(gcd) == 0L) {
    if (progress) {
      geocode_progress_message("creating no-match geocode results")
    }
    gcd <- list(geocode_no_match(xu))
  }
  if (progress) {
    geocode_progress_message(
      "combining geocode results from ",
      geocode_result_group_count_text(length(gcd))
    )
  }
  gcd <- do.call(rbind, gcd)

  if (progress) {
    geocode_progress_message(
      "restoring geocode output to ",
      geocode_input_addr_count_text(length(x))
    )
  }
  out <- gcd[match(format(x), format(gcd$addr)), ]
  if (progress) {
    geocode_progress_message("computing S2 cells")
  }
  out$s2_cell <- s2::as_s2_cell(out$matched_geography)
  if (progress) {
    geocode_progress_message("geocoding complete")
  }
  return(out)
}

geocode_map <- function(x, FUN, progress, ...) {
  if (length(x) == 0L) {
    return(list())
  }
  if (geocode_use_mirai()) {
    if (progress) {
      geocode_progress_message(geocode_mirai_progress_text(x))
    }
    return(geocode_map_mirai(x, FUN, progress = progress, ...))
  }
  if (progress) {
    return(lapply_pb(x, FUN, ...))
  }
  lapply(x, function(.x) geocode_call_with_context(.x, FUN, ...))
}

geocode_use_mirai <- function() {
  requireNamespace("mirai", quietly = TRUE) &&
    mirai::daemons_set()
}

geocode_map_mirai <- function(x, FUN, progress, ...) {
  geocode_args <- list(...)
  load_dev <- geocode_load_dev_workers()
  package_path <- if (load_dev) {
    getNamespaceInfo(asNamespace("addr"), "path")
  } else {
    NULL
  }
  out <- mirai::mirai_map(
    x,
    geocode_worker,
    .args = list(
      geocode_args = geocode_args,
      package_path = package_path,
      load_dev = load_dev
    )
  )
  out <- geocode_collect_mirai(out, x, progress = progress)
  if (progress) {
    geocode_progress_message("mirai workers complete; restoring geographies")
  }
  geocode_check_parallel_results(out)
  out <- lapply(out, geocode_restore_parallel_result)
  if (progress) {
    geocode_progress_message("mirai geocoding results restored")
  }
  out
}

geocode_load_dev_workers <- function() {
  requireNamespace("pkgload", quietly = TRUE) &&
    pkgload::is_dev_package("addr")
}

geocode_collect_mirai <- function(x, tasks, progress) {
  if (!progress) {
    return(x[])
  }

  total_groups <- length(x)
  task_sizes <- lengths(tasks)
  total_addr <- sum(task_sizes)
  done <- rep(FALSE, total_groups)
  out <- vector("list", total_groups)
  names(out) <- names(x)
  started <- proc.time()[["elapsed"]]
  last_completed <- NA_integer_
  last_update <- 0

  geocode_mirai_status_update(
    completed_groups = 0L,
    total_groups = total_groups,
    completed_addr = 0L,
    total_addr = total_addr,
    last_zip = NA_character_,
    last_addr = NA_integer_,
    elapsed = 0
  )
  on.exit(cat("\n"), add = TRUE)

  while (!all(done)) {
    resolved <- !vapply(x, mirai::unresolved, logical(1))
    newly_done <- which(resolved & !done)
    if (length(newly_done) > 0L) {
      for (i in newly_done) {
        out[[i]] <- x[[i]]$data
      }
      done[newly_done] <- TRUE
      last_completed <- newly_done[[length(newly_done)]]
    }

    elapsed <- proc.time()[["elapsed"]] - started
    should_update <- length(newly_done) > 0L ||
      elapsed - last_update >= geocode_mirai_progress_interval()
    if (should_update) {
      last_update <- elapsed
      last_zip <- if (is.na(last_completed)) {
        NA_character_
      } else {
        names(x)[[last_completed]]
      }
      last_addr <- if (is.na(last_completed)) {
        NA_integer_
      } else {
        task_sizes[[last_completed]]
      }
      geocode_mirai_status_update(
        completed_groups = sum(done),
        total_groups = total_groups,
        completed_addr = sum(task_sizes[done]),
        total_addr = total_addr,
        last_zip = last_zip,
        last_addr = last_addr,
        elapsed = elapsed
      )
    }

    if (!all(done)) {
      Sys.sleep(0.1)
    }
  }

  out
}

geocode_worker <- function(
  x,
  geocode_args,
  package_path = NULL,
  load_dev = FALSE
) {
  if (load_dev) {
    if (!requireNamespace("pkgload", quietly = TRUE)) {
      stop(
        "pkgload must be installed to use mirai workers with a dev-loaded addr package",
        call. = FALSE
      )
    }
    pkgload::load_all(
      path = package_path,
      reset = TRUE,
      compile = NA,
      attach = FALSE,
      export_all = FALSE,
      export_imports = FALSE,
      helpers = FALSE,
      attach_testthat = FALSE,
      quiet = TRUE,
      recompile = FALSE,
      warn_conflicts = FALSE
    )
  }
  ns <- loadNamespace("addr")
  geocode_zip_fn <- get("geocode_zip", envir = ns, inherits = FALSE)
  context_message_fn <- get(
    "geocode_context_error_message",
    envir = ns,
    inherits = FALSE
  )
  out <- tryCatch(
    do.call(geocode_zip_fn, c(list(x), geocode_args)),
    error = function(cnd) {
      stop(context_message_fn(x, cnd), call. = FALSE)
    }
  )
  out$matched_geography <- s2::s2_as_text(out$matched_geography)
  out
}

geocode_check_parallel_results <- function(x) {
  err <- vapply(x, inherits, logical(1), "miraiError")
  if (!any(err)) {
    return(invisible(x))
  }
  first_err <- x[[which(err)[1]]]
  msg <- attr(first_err, "message", exact = TRUE)
  if (is.null(msg) || !length(msg)) {
    msg <- as.character(first_err[[1]])
  }
  stop(
    "parallel geocoding failed in a mirai worker: ",
    msg,
    call. = FALSE
  )
}

geocode_restore_parallel_result <- function(x) {
  x$matched_geography <- s2::as_s2_geography(x$matched_geography)
  x
}

geocode_call_with_context <- function(x, FUN, ...) {
  tryCatch(
    FUN(x, ...),
    error = function(cnd) {
      stop(geocode_context_error_message(x, cnd), call. = FALSE)
    }
  )
}

geocode_context_error_message <- function(x, cnd) {
  zip <- unique(x@place@zipcode)
  zip <- zip[!is.na(zip) & zip != ""]
  zip <- if (length(zip) == 0L) {
    "<missing>"
  } else {
    paste(zip, collapse = ", ")
  }
  n_addr <- length(x)
  noun <- if (n_addr == 1L) "address" else "addresses"
  examples <- addr_problem_examples(format(x), seq_along(x))
  paste0(
    "geocoding failed for ZIP ",
    zip,
    " (",
    n_addr,
    " input ",
    noun,
    "). Affected address examples: ",
    examples,
    ". Original error: ",
    conditionMessage(cnd)
  )
}

lapply_pb <- function(x, FUN, ...) {
  total <- sum(lengths(x))
  processed <- 0L
  addr_progress_update(
    processed,
    total,
    "geocoding addr vectors",
    first = TRUE
  )
  on.exit(cat("\n"), add = TRUE)

  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    zip <- names(x)[[i]]
    x_n <- length(x[[i]])
    text <- geocode_progress_text(zip, x_n, NA_integer_)
    progress_callback <- function(y_n) {
      text <<- geocode_progress_text(zip, x_n, y_n)
      addr_progress_update(
        processed,
        total,
        text,
        first = FALSE
      )
    }
    args <- c(list(x[[i]]), list(...))
    if ("progress_callback" %in% names(formals(FUN))) {
      args$progress_callback <- progress_callback
    }
    out[[i]] <- tryCatch(
      do.call(FUN, args),
      error = function(cnd) {
        stop(geocode_context_error_message(x[[i]], cnd), call. = FALSE)
      }
    )
    processed <- processed + x_n
    addr_progress_update(
      processed,
      total,
      text,
      first = FALSE
    )
  }
  out
}

geocode_progress_text <- function(zip, x_n, y_n) {
  sprintf(
    "geocoding %s (%s addr to %s addr_street)",
    zip,
    geocode_format_count(x_n),
    geocode_format_count(y_n)
  )
}

geocode_progress_message <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

geocode_format_count <- function(x) {
  prettyNum(x, big.mark = ",", preserve.width = "none")
}

geocode_prepare_taf_progress_text <- function(
  x,
  taf_install,
  zip_variants
) {
  has_zip <- !is.na(x@place@zipcode) & x@place@zipcode != ""
  zipcodes <- unique(x@place@zipcode[has_zip])
  install_text <- if (taf_install) {
    "installing missing county files if needed"
  } else {
    "using installed files only"
  }
  variant_text <- if (zip_variants) {
    " plus ZIP variants"
  } else {
    ""
  }
  sprintf(
    "checking TIGER address feature files for %s%s (%s)",
    geocode_zip_count_text(length(zipcodes)),
    variant_text,
    install_text
  )
}

geocode_group_progress_text <- function(x) {
  sprintf(
    "grouped %s into %s",
    geocode_addr_count_text(sum(lengths(x))),
    geocode_zip_group_count_text(length(x))
  )
}

geocode_mirai_progress_text <- function(x) {
  sprintf(
    "dispatching %s across mirai workers (%s)",
    geocode_zip_group_count_text(length(x)),
    geocode_addr_count_text(sum(lengths(x)))
  )
}

geocode_mirai_status_update <- function(
  completed_groups,
  total_groups,
  completed_addr,
  total_addr,
  last_zip,
  last_addr,
  elapsed
) {
  cat(
    "\r\033[2K",
    geocode_mirai_status_text(
      completed_groups = completed_groups,
      total_groups = total_groups,
      completed_addr = completed_addr,
      total_addr = total_addr,
      last_zip = last_zip,
      last_addr = last_addr,
      elapsed = elapsed
    ),
    sep = ""
  )
  utils::flush.console()
}

geocode_mirai_status_text <- function(
  completed_groups,
  total_groups,
  completed_addr,
  total_addr,
  last_zip,
  last_addr,
  elapsed
) {
  last_text <- if (is.na(last_zip) || is.na(last_addr)) {
    "last completed: none yet"
  } else {
    sprintf(
      "last completed: %s (%s)",
      last_zip,
      geocode_addr_count_text(last_addr)
    )
  }
  sprintf(
    "mirai geocoding: %s/%s ZIP groups complete; %s/%s unique addr complete; %s; elapsed %s",
    geocode_format_count(completed_groups),
    geocode_format_count(total_groups),
    geocode_format_count(completed_addr),
    geocode_format_count(total_addr),
    last_text,
    geocode_elapsed_text(elapsed)
  )
}

geocode_elapsed_text <- function(x) {
  if (x < 60) {
    return(sprintf("%ss", floor(x)))
  }
  sprintf("%sm %02ds", floor(x / 60), floor(x %% 60))
}

geocode_mirai_progress_interval <- function() {
  interval <- getOption("addr.geocode_mirai_progress_interval", 1)
  if (length(interval) == 0L) {
    return(1)
  }
  interval <- suppressWarnings(as.numeric(interval[[1]]))
  if (is.na(interval) || interval <= 0) {
    1
  } else {
    interval
  }
}

geocode_addr_count_text <- function(n) {
  paste(geocode_format_count(n), "unique addr")
}

geocode_input_addr_count_text <- function(n) {
  paste(geocode_format_count(n), "input addr")
}

geocode_result_group_count_text <- function(n) {
  sprintf(
    "%s result %s",
    geocode_format_count(n),
    if (n == 1L) "group" else "groups"
  )
}

geocode_zip_group_count_text <- function(n) {
  sprintf(
    "%s ZIP %s",
    geocode_format_count(n),
    if (n == 1L) "group" else "groups"
  )
}

geocode_zip_count_text <- function(n) {
  sprintf(
    "%s ZIP %s",
    geocode_format_count(n),
    if (n == 1L) "code" else "codes"
  )
}

geocode_no_match <- function(x) {
  tibble::tibble(
    addr = x,
    matched_zipcode = rep(NA_character_, length(x)),
    matched_street = addr_street(rep(NA_character_, length(x))),
    matched_geography = s2::as_s2_geography(rep(NA_character_, length(x)))
  )
}

#' Convert geocode objects to JSON-safe tables
#'
#' @description
#' `geocode_table()` converts the rich output from [geocode()] to a flat table
#' with only JSON-safe column types.
#'
#' @param x a data frame returned by [geocode()]
#' @returns A tibble with atomic columns suitable for JSON serialization.
#'   `geocode_table()` includes the input address, geocode stage, matched ZIP
#'   code, matched street, and S2 cell as character columns.
#' @export
geocode_table <- function(x) {
  stopifnot(
    "x must be a data frame" = is.data.frame(x),
    "x must contain an addr column" = "addr" %in% names(x),
    "x$addr must be an addr vector" = inherits(x$addr, "addr"),
    "x must contain a matched_zipcode column" = "matched_zipcode" %in% names(x),
    "x must contain a matched_street column" = "matched_street" %in% names(x),
    "x$matched_street must be an addr_street vector" = inherits(
      x$matched_street,
      "addr_street"
    ),
    "x must contain an s2_cell column" = "s2_cell" %in% names(x),
    "x$s2_cell must be an s2_cell vector" = inherits(x$s2_cell, "s2_cell")
  )

  out <- data.frame(
    addr = as.character(x$addr),
    geocode_stage = as.character(geocode_stage(x)),
    matched_zipcode = x$matched_zipcode,
    matched_street = as.character(x$matched_street),
    s2_cell = as.character(x$s2_cell),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  tibble::as_tibble(out)
}

#' Classify geocode stage
#'
#' Classify geocode results into staged outcomes returned by [geocode()]:
#' no match, street match, or interpolated range match, distinguishing exact
#' ZIP-code matches from ZIP-code variant matches.
#'
#' @param x a data frame returned by [geocode()]
#' @return an ordered factor with levels `none`, `street_variant`, `street`,
#'   `range_variant`, `range`
#' @export
geocode_stage <- function(x) {
  stopifnot(
    "x must be a data frame" = is.data.frame(x),
    "x must contain an addr column" = "addr" %in% names(x),
    "x$addr must be an addr vector" = inherits(x$addr, "addr"),
    "x must contain a matched_zipcode column" = "matched_zipcode" %in% names(x),
    "x must contain a matched_street column" = "matched_street" %in% names(x),
    "x$matched_street must be an addr_street vector" = inherits(
      x$matched_street,
      "addr_street"
    ),
    "x must contain an s2_cell column" = "s2_cell" %in% names(x),
    "x$s2_cell must be an s2_cell vector" = inherits(x$s2_cell, "s2_cell")
  )

  matched_street <- as.character(x$matched_street)
  input_zipcode <- x$addr@place@zipcode
  has_zipcode <- !is.na(x$matched_zipcode) & x$matched_zipcode != ""
  has_street <- has_zipcode & !is.na(matched_street) & matched_street != ""
  has_range <- has_street & !is.na(x$s2_cell)
  has_exact_zipcode <- has_street &
    !is.na(input_zipcode) &
    input_zipcode != "" &
    x$matched_zipcode == input_zipcode
  has_variant <- has_street & !has_exact_zipcode

  out <- rep("none", nrow(x))
  out[has_street] <- "street"
  out[has_street & has_variant] <- "street_variant"
  out[has_range] <- "range"
  out[has_range & has_variant] <- "range_variant"
  ordered(
    out,
    levels = c(
      "none",
      "street_variant",
      "street",
      "range_variant",
      "range"
    )
  )
}

geocode_prepare_taf <- function(
  x,
  year,
  version,
  zip_variants,
  zip_variant,
  taf_install,
  taf_redownload
) {
  if (length(x) == 0L) {
    return(invisible(NULL))
  }

  missing_args <- list(
    x = x,
    year = year,
    version = version,
    zip_variants = zip_variants,
    zip_variant = zip_variant
  )

  if (taf_install) {
    taf_ensure_serial(
      x,
      year = year,
      version = version,
      zip_variants = zip_variants,
      zip_variant = zip_variant,
      redownload = taf_redownload
    )
    geocode_require_taf_installed(do.call(taf_missing_counties, missing_args))
  } else {
    taf_warn_missing_counties(do.call(taf_missing_counties, missing_args))
  }

  invisible(NULL)
}

geocode_require_taf_installed <- function(missing) {
  if (nrow(missing) == 0L) {
    return(invisible(missing))
  }

  counties <- unique(missing$county_fips)
  zip_variants <- unique(paste0(
    missing$ZIP,
    " (",
    missing$source_zip_variant,
    " from ",
    missing$source_zip,
    ")"
  ))
  stop(
    "TAF files are still missing after installation for ",
    length(counties),
    " county/counties; geocoding cannot continue. Missing counties: ",
    taf_collapse_for_message(counties),
    ". Affected ZIPs: ",
    taf_collapse_for_message(zip_variants),
    ".",
    call. = FALSE
  )
}


# # convert s2 cell and point geographies to text
# gcd_tbl$s2_cell <- as.character(gcd_tbl$s2_cell)
# gcd_tbl$wkt <- s2::s2_as_text(x_s2_geo)

#' @param progress_callback optional callback used internally by `geocode()`
#'   to update progress after ZIP-code reference data is loaded
#' @param taf_check logical; check for missing TAF counties? Used internally
#'   by `geocode()` after checking once for the full input vector.
#' @rdname geocode
geocode_zip <- function(
  x,
  offset = 10L,
  name_phonetic_dist = 1L,
  name_fuzzy_dist = 2L,
  match_street_type = c("exact", "compatible", "ignore"),
  match_street_directional = c("exact", "swap", "ignore"),
  zip_variants = TRUE,
  zip_variant = c("minus1", "plus1", "sub5", "sub4", "swap"),
  year = as.character(2025:2011),
  version = "v1",
  taf_install = TRUE,
  taf_redownload = FALSE,
  progress_callback = NULL,
  taf_check = TRUE
) {
  stopifnot("x must be an addr vector" = inherits(x, "addr"))
  stopifnot(
    "zip_variants must be TRUE or FALSE" = is.logical(zip_variants) &&
      length(zip_variants) == 1L &&
      !is.na(zip_variants),
    "taf_install must be TRUE or FALSE" = is.logical(taf_install) &&
      length(taf_install) == 1L &&
      !is.na(taf_install),
    "taf_redownload must be TRUE or FALSE" = is.logical(taf_redownload) &&
      length(taf_redownload) == 1L &&
      !is.na(taf_redownload),
    "taf_check must be TRUE or FALSE" = is.logical(taf_check) &&
      length(taf_check) == 1L &&
      !is.na(taf_check),
    "version must be a character vector" = is.character(version),
    "version must be length one" = length(version) == 1L,
    "version must not be missing" = !is.na(version)
  )
  year <- match.arg(year)
  zip_variant <- validate_zip_variant(zip_variant)
  validate_geocode_offset(offset)
  match_args <- validate_match_addr_street_args(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_type = match_street_type,
    match_street_directional = match_street_directional
  )
  street_match_args <- list(
    name_phonetic_dist = name_phonetic_dist,
    name_fuzzy_dist = name_fuzzy_dist,
    match_street_type = match_args$match_street_type,
    match_street_directional = match_args$match_street_directional
  )
  zpcd <- unique(x@place@zipcode)
  if (length(zpcd) != 1) {
    stop(
      "addr vectors passed to geocode_zip()",
      " must only have one unique zipcode;",
      " found ",
      length(zpcd),
      " unique zipcodes;",
      " use `geocode()` with more than one unique zipcode;",
      call. = FALSE
    )
  }

  out <- geocode_no_match(x)

  if (is.na(zpcd) || zpcd == "") {
    return(out)
  }

  if (taf_check) {
    geocode_prepare_taf(
      x,
      year = year,
      version = version,
      zip_variants = zip_variants,
      zip_variant = zip_variant,
      taf_install = taf_install,
      taf_redownload = taf_redownload
    )
  }

  ref_exact <- taf_zip(zpcd, map = TRUE, year = year, version = version)
  ref_exact_streets <- geocode_unique_ref_streets(ref_exact)
  ref_exact_rng_idx <- geocode_ref_range_index(ref_exact)
  if (is.function(progress_callback)) {
    progress_callback(length(ref_exact$addr_street))
  }
  out$matched_street <- do.call(
    match_addr_street,
    c(list(x = x@street, y = ref_exact_streets$addr_street), street_match_args)
  )

  no <- which(is.na(out$matched_street))
  out$matched_zipcode <- zpcd

  variant_zipcodes <- if (zip_variants) {
    zipcode_variant(zpcd, variant = zip_variant)
  } else {
    character()
  }

  if (length(no) != 0 && zip_variants && length(variant_zipcodes) > 0L) {
    out$matched_zipcode[no] <- NA_character_
    ref_variant <- taf_zip(
      variant_zipcodes,
      map = TRUE,
      year = year,
      version = version
    )
    ref_variant_streets <- geocode_unique_ref_streets(ref_variant)
    out$matched_street[no] <- do.call(
      match_addr_street,
      c(
        list(x = x@street[no], y = ref_variant_streets$addr_street),
        street_match_args
      )
    )
    out$matched_zipcode[no] <-
      ref_variant_streets[
        match(
          geocode_street_key(out$matched_street[no]),
          geocode_street_key(ref_variant_streets$addr_street)
        ),
        "ZIP",
        drop = TRUE
      ]
  } else if (length(no) != 0) {
    out$matched_zipcode[no] <- NA_character_
    ref_variant <- ref_exact[0, ]
    ref_variant_streets <- geocode_unique_ref_streets(ref_variant)
  } else {
    ref_variant <- ref_exact[0, ]
    ref_variant_streets <- geocode_unique_ref_streets(ref_variant)
  }

  ref_variant_rng_idx <- geocode_ref_range_index(ref_variant)

  out$matched_geography <-
    lapply(seq_along(x), \(.i) {
      sn <- to_int(x@number@digits[.i])
      if (
        is.na(sn) ||
          is.na(out$matched_zipcode[.i]) ||
          is.na(out$matched_street[.i])
      ) {
        return(s2::as_s2_geography(NA_character_))
      }
      if (out$matched_zipcode[.i] == zpcd) {
        cand_rows <- geocode_ref_range_rows(
          ref_exact_rng_idx,
          out$matched_zipcode[.i],
          out$matched_street[.i]
        )
        ref <- ref_exact
      } else {
        cand_rows <- geocode_ref_range_rows(
          ref_variant_rng_idx,
          out$matched_zipcode[.i],
          out$matched_street[.i]
        )
        ref <- ref_variant
      }
      if (length(cand_rows) == 0L) {
        return(s2::as_s2_geography(NA_character_))
      }
      sn_par <- ifelse(sn %% 2 == 0, "E", "O")
      cand0 <- ref[cand_rows, , drop = FALSE]
      has_range <- !is.na(cand0$FROMHN) & !is.na(cand0$TOHN)
      cand0$in_range <- has_range &
        sn >= pmin(cand0$FROMHN, cand0$TOHN) &
        sn <= pmax(cand0$FROMHN, cand0$TOHN)
      cand0$par_ok <- cand0$in_range &
        (is.na(cand0$PARITY) | cand0$PARITY %in% c("B", sn_par))
      cand0$width <- ifelse(
        cand0$in_range,
        abs(cand0$FROMHN - cand0$TOHN),
        NA_integer_
      )
      cand0$mid <- ifelse(
        cand0$in_range,
        (cand0$FROMHN + cand0$TOHN) / 2,
        NA_real_
      )
      cand0$mid_dist <- abs(sn - cand0$mid)
      cand0 <- cand0[cand0$par_ok, ]
      cand0 <- cand0[order(cand0$width, cand0$mid_dist, na.last = TRUE), ]
      if (nrow(cand0) == 0L) {
        return(s2::as_s2_geography(NA_character_))
      }
      brm <- cand0[1, ]
      fraction <- geocode_range_fraction(sn, brm$FROMHN, brm$TOHN)
      point <- s2::s2_interpolate_normalized(
        brm$s2_geography,
        fraction
      )
      # TIGER side is relative to the digitized street line direction.
      range_offset <- if (
        "OFFSET" %in% names(brm) &&
          length(brm$OFFSET) == 1L &&
          !is.na(brm$OFFSET) &&
          brm$OFFSET == "Y"
      ) {
        0
      } else {
        offset
      }
      geocode_offset_point(
        point,
        brm$s2_geography,
        fraction,
        brm$side,
        range_offset
      )
    }) |>
    do.call(c, args = _)

  out
}

geocode_unique_ref_streets <- function(x) {
  keep <- !duplicated(as.data.frame(x$addr_street, stringsAsFactors = FALSE))
  x[keep, c("ZIP", "addr_street"), drop = FALSE]
}

geocode_ref_range_index <- function(x) {
  if (nrow(x) == 0L) {
    return(list())
  }
  split(seq_len(nrow(x)), geocode_ref_range_key(x$ZIP, x$addr_street))
}

geocode_ref_range_rows <- function(index, zipcode, street) {
  rows <- index[[geocode_ref_range_key(zipcode, street)]]
  if (is.null(rows)) {
    integer()
  } else {
    rows
  }
}

geocode_ref_range_key <- function(zipcode, street) {
  paste(zipcode, geocode_street_key(street), sep = "\r")
}

geocode_street_key <- function(x) {
  format(x)
}

validate_geocode_offset <- function(offset) {
  stopifnot(
    "offset must be numeric" = is.numeric(offset),
    "offset must be length one" = length(offset) == 1L,
    "offset must not be missing" = !is.na(offset),
    "offset must be finite" = is.finite(offset),
    "offset must be non-negative" = offset >= 0
  )
}

geocode_range_fraction <- function(number, from, to) {
  if (is.na(number) || is.na(from) || is.na(to)) {
    return(NA_real_)
  }
  if (from == to) {
    return(0.5)
  }
  max(0, min(1, (number - from) / (to - from)))
}

geocode_offset_point <- function(point, line, fraction, side, offset) {
  if (offset == 0 || is.na(side) || !side %in% c("L", "R")) {
    return(point)
  }

  bearing <- geocode_line_bearing(line, fraction)
  if (is.na(bearing)) {
    return(point)
  }

  offset_bearing <- bearing + if (side == "L") -90 else 90
  geocode_direct_point(point, offset_bearing, offset)
}

geocode_line_bearing <- function(line, fraction) {
  delta <- 1e-6
  from <- s2::s2_interpolate_normalized(line, max(0, fraction - delta))
  to <- s2::s2_interpolate_normalized(line, min(1, fraction + delta))

  if (s2::s2_distance(from, to) == 0) {
    return(NA_real_)
  }

  geocode_bearing(from, to)
}

geocode_bearing <- function(from, to) {
  from_lon <- geocode_degrees_to_radians(s2::s2_x(from))
  from_lat <- geocode_degrees_to_radians(s2::s2_y(from))
  to_lon <- geocode_degrees_to_radians(s2::s2_x(to))
  to_lat <- geocode_degrees_to_radians(s2::s2_y(to))

  d_lon <- to_lon - from_lon
  x <- sin(d_lon) * cos(to_lat)
  y <- cos(from_lat) * sin(to_lat) - sin(from_lat) * cos(to_lat) * cos(d_lon)

  geocode_radians_to_degrees(atan2(x, y))
}

geocode_direct_point <- function(point, bearing, distance) {
  radius <- s2::s2_earth_radius_meters()
  angular_distance <- distance / radius
  lon <- geocode_degrees_to_radians(s2::s2_x(point))
  lat <- geocode_degrees_to_radians(s2::s2_y(point))
  bearing <- geocode_degrees_to_radians(bearing)

  out_lat <- asin(
    sin(lat) *
      cos(angular_distance) +
      cos(lat) * sin(angular_distance) * cos(bearing)
  )
  out_lon <- lon +
    atan2(
      sin(bearing) * sin(angular_distance) * cos(lat),
      cos(angular_distance) - sin(lat) * sin(out_lat)
    )

  s2::s2_geog_point(
    geocode_normalize_longitude(geocode_radians_to_degrees(out_lon)),
    geocode_radians_to_degrees(out_lat)
  )
}

geocode_degrees_to_radians <- function(x) {
  x * pi / 180
}

geocode_radians_to_degrees <- function(x) {
  x * 180 / pi
}

geocode_normalize_longitude <- function(x) {
  ((x + 540) %% 360) - 180
}
