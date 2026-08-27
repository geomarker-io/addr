tiger_download_state <- new.env(parent = emptyenv())
tiger_download_state$last_request <- NULL
tiger_download_state$rejection <- NULL

tiger_download <- function(
  x,
  subdir,
  overwrite = FALSE,
  offline = FALSE
) {
  stopifnot(
    "x must be a character vector" = is.character(x),
    "x must be length one" = length(x) == 1L,
    "x must not be missing" = !is.na(x),
    "subdir must be a character vector" = is.character(subdir),
    "subdir must be length one" = length(subdir) == 1L,
    "subdir must not be missing" = !is.na(subdir),
    "overwrite must be logical" = is.logical(overwrite),
    "overwrite must be length one" = length(overwrite) == 1L,
    "overwrite must not be missing" = !is.na(overwrite),
    "offline must be logical" = is.logical(offline),
    "offline must be length one" = length(offline) == 1L,
    "offline must not be missing" = !is.na(offline)
  )
  url <- tiger_download_url(x)
  stow_args <- list(
    url,
    package = "addr",
    subdir = subdir,
    overwrite = overwrite,
    etag = FALSE,
    validate = tiger_validate_zip
  )

  if (offline) {
    stow_args$offline <- TRUE
    return(do.call(stow::stow, stow_args))
  }

  # Probe the exact managed path without making a network request. This keeps
  # request pacing from slowing down reads of already cached source ZIPs.
  if (!overwrite) {
    cached_args <- stow_args
    cached_args$offline <- TRUE
    cached <- tryCatch(
      do.call(stow::stow, cached_args),
      error = identity
    )
    if (!inherits(cached, "error")) {
      return(cached)
    }
  }

  attempts <- tiger_download_attempts()
  rejection <- NULL
  last_error <- NULL
  attempts_made <- 0L

  for (attempt in seq_len(attempts)) {
    attempts_made <- attempt
    tiger_download_state$rejection <- NULL
    tiger_download_wait()
    stow_args$offline <- FALSE
    result <- tryCatch(
      do.call(stow::stow, stow_args),
      error = identity
    )
    if (!inherits(result, "error")) {
      return(result)
    }

    last_error <- result
    if (!is.null(tiger_download_state$rejection)) {
      rejection <- tiger_download_state$rejection
    }
    if (attempt == attempts || !tiger_download_retryable(result)) {
      break
    }

    delay <- tiger_download_retry_delay(attempt)
    message(
      "TIGER download attempt ",
      attempt,
      " of ",
      attempts,
      " failed; retrying in ",
      format(round(delay, 1L), nsmall = 1L),
      " seconds."
    )
    Sys.sleep(delay)
  }

  tiger_download_abort(
    error = last_error,
    url = url,
    attempts = attempts_made,
    rejection = rejection
  )
}

tiger_download_url <- function(x) {
  protocol <- getOption("addr.tiger_download_protocol", "https")
  if (
    !is.character(protocol) ||
      length(protocol) != 1L ||
      is.na(protocol) ||
      !protocol %in% c("https", "ftp")
  ) {
    stop(
      "`options(addr.tiger_download_protocol = ...)` must be ",
      'either "https" or "ftp".',
      call. = FALSE
    )
  }
  base_url <- switch(
    protocol,
    https = "https://www2.census.gov/geo/tiger/",
    ftp = "ftp://ftp2.census.gov/geo/tiger/"
  )
  paste0(base_url, x)
}

tiger_download_attempts <- function() {
  attempts <- getOption("addr.tiger_download_attempts", 4L)
  if (
    !is.numeric(attempts) ||
      length(attempts) != 1L ||
      is.na(attempts) ||
      !is.finite(attempts) ||
      attempts < 1 ||
      attempts != as.integer(attempts)
  ) {
    return(4L)
  }
  as.integer(attempts)
}

tiger_download_wait <- function() {
  interval <- getOption("addr.tiger_download_interval", 1)
  if (
    !is.numeric(interval) ||
      length(interval) != 1L ||
      is.na(interval) ||
      !is.finite(interval) ||
      interval < 0
  ) {
    interval <- 1
  }
  now <- proc.time()[["elapsed"]]
  last <- tiger_download_state$last_request
  if (!is.null(last)) {
    Sys.sleep(max(0, interval - (now - last)))
  }
  tiger_download_state$last_request <- proc.time()[["elapsed"]]
  invisible(NULL)
}

tiger_download_retry_delay <- function(attempt) {
  base <- getOption("addr.tiger_download_retry_base", 5)
  if (
    !is.numeric(base) ||
      length(base) != 1L ||
      is.na(base) ||
      !is.finite(base) ||
      base < 0
  ) {
    base <- 5
  }
  jitter <- getOption("addr.tiger_download_retry_jitter", 1)
  if (
    !is.numeric(jitter) ||
      length(jitter) != 1L ||
      is.na(jitter) ||
      !is.finite(jitter) ||
      jitter < 0
  ) {
    jitter <- 1
  }
  base * 3^(attempt - 1L) + stats::runif(1L, min = 0, max = jitter)
}

tiger_download_retryable <- function(error) {
  message <- conditionMessage(error)
  missing_file <- grepl(
    "(^|[^0-9])404([^0-9]|$)|550[^\\n]*(not found|no such file)",
    message,
    ignore.case = TRUE,
    perl = TRUE
  )
  !missing_file && grepl(
    "Download failed|Downloaded content failed validation",
    message,
    fixed = FALSE
  )
}

tiger_download_abort <- function(error, url, attempts, rejection = NULL) {
  attempt_word <- if (attempts == 1L) "attempt" else "attempts"
  if (!is.null(rejection)) {
    support <- if (is.null(rejection$support_id)) {
      ""
    } else {
      paste0("\nCensus support ID: ", rejection$support_id)
    }
    stop(
      "Census TIGER rejected the download request and returned HTML instead ",
      "of the requested ZIP file. This is a server rejection, not an empty ",
      "or valid TIGER archive. addr did not cache the response.\n",
      "URL: ", url, support, "\n",
      "The download failed after ", attempts, " ", attempt_word, ". Retry ",
      "later with `redownload = FALSE`; completed source ZIPs remain cached. ",
      "For persistent HTTPS rejection, the documented FTP option is available ",
      "but is not encrypted.\n",
      "Original error: ", conditionMessage(error),
      call. = FALSE
    )
  }
  stop(
    "Census TIGER download failed after ",
    attempts,
    " ",
    attempt_word,
    ".\nURL: ",
    url,
    "\nCompleted source ZIPs remain cached, so retrying with ",
    "`redownload = FALSE` resumes the operation.\nOriginal error: ",
    conditionMessage(error),
    call. = FALSE
  )
}

tiger_validate_zip <- function(path) {
  stopifnot(
    "path must be a character vector" = is.character(path),
    "path must be length one" = length(path) == 1L,
    "path must not be missing" = !is.na(path)
  )
  contents <- tryCatch(
    suppressWarnings(utils::unzip(path, list = TRUE)),
    error = function(e) NULL
  )
  valid <- is.data.frame(contents) && nrow(contents) > 0L
  if (!valid) {
    tiger_download_state$rejection <- tiger_download_rejection(path)
  }
  valid
}

tiger_download_rejection <- function(path) {
  text <- tryCatch(
    {
      bytes <- readBin(path, what = "raw", n = 8192L)
      code_points <- as.integer(bytes)
      code_points[code_points < 32L | code_points > 126L] <- 32L
      intToUtf8(code_points)
    },
    error = function(e) ""
  )
  if (!grepl("request rejected|support ID", text, ignore.case = TRUE)) {
    return(NULL)
  }
  support_match <- regexec(
    "support ID(?:[[:space:]]+is)?[^[:alnum:]]+([[:alnum:]-]+)",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
  support_parts <- regmatches(text, support_match)[[1L]]
  support_id <- if (length(support_parts) >= 2L) support_parts[[2L]] else NULL
  list(support_id = support_id)
}

tiger_feat_names_download <- function(county, year, redownload) {
  tiger_download(
    sprintf(
      "TIGER%s/FEATNAMES/tl_%s_%s_featnames.zip",
      year,
      year,
      county
    ),
    subdir = "tiger_feat_names",
    overwrite = redownload
  )
}

tiger_addr_feat_download <- function(county, year, redownload) {
  tiger_download(
    sprintf(
      "TIGER%s/ADDRFEAT/tl_%s_%s_addrfeat.zip",
      year,
      year,
      county
    ),
    subdir = "tiger_addr_feat",
    overwrite = redownload
  )
}

#' Get names for tiger street ranges
#'
#' @description
#'
#' TIGER primary feature names are read from compressed feature-name databases
#' for each county and Census vintage.
#' If not already present, compressed addrfeat (address feature) shapefiles are
#' downloaded from the Census TIGER HTTPS endpoint by default as durable
#' managed local copies in addr's `stow/tiger_feat_names` directory. Files from
#' the former unmanaged TIGER layout are not searched.
#'
#' When reading into R, the data is filtered to addressable MTFCCs
#' (S1100, S1200, S1400, S1640) that have a name.
#' @inheritParams tiger_addr_feat
#' @param redownload logical, length 1; replace the durable managed local copy
#'   of the TIGER ZIP file?
#' @returns a tibble with unique `LINEARID` and `addr` columns
#' @export
#' @examples
#' \dontrun{
#'   tiger_feat_names("39061", "2025")
#' }
tiger_feat_names <- function(county, year, redownload = FALSE) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year),
    "redownload must be logical" = is.logical(redownload),
    "redownload must be length one" = length(redownload) == 1L,
    "redownload must not be missing" = !is.na(redownload)
  )
  check_installed("sf", "to read TIGER feature names")
  tp <- tiger_feat_names_download(county, year, redownload) |>
    paste0("/vsizip/", file_path = _)
  rd <- sf::st_read(
    tp,
    quiet = TRUE,
    stringsAsFactors = FALSE,
    as_tibble = TRUE
  )
  rd <- rd[rd$MTFCC %in% c("S1100", "S1200", "S1400", "S1630", "S1640"), ]
  rd <- rd[!is.na(rd$FULLNAME), ]
  rd <- rd[rd$PAFLAG == "P", ]
  na_to_empty <- \(x) ifelse(is.na(x), "", x)
  d <- tibble::tibble(
    LINEARID = rd$LINEARID,
    premodifier = na_to_empty(rd$PREQUALABR),
    predirectional = na_to_empty(rd$PREDIRABRV),
    pretype = na_to_empty(rd$PRETYPABRV),
    name = na_to_empty(rd$NAME),
    posttype = na_to_empty(rd$SUFTYPABRV),
    postdirectional = na_to_empty(rd$SUFDIRABRV),
  ) |>
    unique()
  d$addr_street <- addr::addr_street(
    premodifier = d$premodifier,
    predirectional = d$predirectional,
    pretype = d$pretype,
    name = d$name,
    posttype = d$posttype,
    postdirectional = d$postdirectional,
    map_posttype = FALSE,
    map_directional = FALSE,
    map_pretype = FALSE,
    map_ordinal = FALSE
  )
  d$premodifier <- NULL
  d$predirectional <- NULL
  d$pretype <- NULL
  d$name <- NULL
  d$posttype <- NULL
  d$postdirectional <- NULL
  d
}

#' Get s2_geography for tiger street ranges
#'
#' @description
#'
#' TIGER address features (street address ranges) are read from compressed
#' addrfeat (address feature) shapefiles for each county and Census vintage.
#' If not already present, compressed addrfeat shapefiles are downloaded from
#' the Census TIGER HTTPS endpoint by default as durable managed local copies in
#' addr's `stow/tiger_addr_feat` directory. Files from the former unmanaged
#' TIGER layout are not searched.
#'
#' When reading into R, the data is converted to one row per street side
#' (`L`/`R`) for use by `taf_install()`.
#' @param county character string of county FIPS identifier
#' @param year character year of the Census TIGER/Line product
#' @param redownload logical, length 1; replace the durable managed local copy
#'   of the TIGER ZIP file?
#' @returns a tibble with `LINEARID`, `FULLNAME`, `side`, `ZIP`,
#' `FROMHN`, `TOHN`, `PARITY`, `OFFSET`, and `s2_geography` columns
#' @export
#' @examples
#' \dontrun{
#'   tiger_addr_feat("39061", "2025")
#' }
tiger_addr_feat <- function(county, year, redownload = FALSE) {
  stopifnot(
    "county must be a character vector" = is.character(county),
    "county must be length one" = length(county) == 1L,
    "county must not be missing" = !is.na(county),
    "year must be a character vector" = is.character(year),
    "year must be length one" = length(year) == 1L,
    "year must not be missing" = !is.na(year),
    "redownload must be logical" = is.logical(redownload),
    "redownload must be length one" = length(redownload) == 1L,
    "redownload must not be missing" = !is.na(redownload)
  )
  check_installed("sf", "to read address range shapefiles")

  tp <- tiger_addr_feat_download(county, year, redownload) |>
    paste0("/vsizip/", file_path = _)

  rd <-
    sf::st_read(
      tp,
      query = sprintf(
        "SELECT LINEARID, FULLNAME, ZIPL, ZIPR, LFROMHN, LTOHN, RFROMHN, RTOHN, PARITYL, PARITYR, OFFSETL, OFFSETR FROM tl_%s_%s_addrfeat",
        year,
        county
      ),
      quiet = TRUE,
      stringsAsFactors = FALSE,
      as_tibble = TRUE
    )

  out <- pivot_addrfeat_sides(rd) |>
    stats::na.omit()

  out$FROMHN <- to_int(out$FROMHN)
  out$TOHN <- to_int(out$TOHN)

  out$s2_geography <- s2::as_s2_geography(out$geometry)
  sf::st_drop_geometry(out)
}

pivot_addrfeat_sides <- function(x) {
  side_cols <- c(
    "ZIPL",
    "ZIPR",
    "LFROMHN",
    "LTOHN",
    "RFROMHN",
    "RTOHN",
    "PARITYL",
    "PARITYR",
    "OFFSETL",
    "OFFSETR"
  )
  base_cols <- setdiff(names(x), side_cols)
  left <- x[base_cols]
  left$side <- "L"
  left$ZIP <- x$ZIPL
  left$FROMHN <- x$LFROMHN
  left$TOHN <- x$LTOHN
  left$PARITY <- x$PARITYL
  left$OFFSET <- x$OFFSETL
  right <- x[base_cols]
  right$side <- "R"
  right$ZIP <- x$ZIPR
  right$FROMHN <- x$RFROMHN
  right$TOHN <- x$RTOHN
  right$PARITY <- x$PARITYR
  right$OFFSET <- x$OFFSETR
  rbind(left, right)
}
