#' Launch the address parsing and matching explorer
#'
#' Opens a Shiny app that shows how an input address is tagged with
#' `tag_usaddress()`, normalized by `as_addr()`, and then matched in stages
#' against `nad_example_data()`.
#'
#' @param launch.browser logical; passed to [shiny::runApp()]
#' @returns Invisibly returns the result of [shiny::runApp()]
#' @export
#' @examples
#' \dontrun{
#'   run_addr_explorer()
#' }
run_addr_explorer <- function(launch.browser = interactive()) {
  check_installed("shiny", "to run the address explorer app")
  check_installed("htmltools", "to render the address explorer app")
  invisible(
    shiny::runApp(
      addr_explorer_app(),
      launch.browser = launch.browser
    )
  )
}

addr_explorer_app <- function() {
  check_installed("shiny", "to run the address explorer app")
  check_installed("htmltools", "to render the address explorer app")

  voter_pool <- voter_addresses()
  nad_index <- nad_example_data(match_prepare = TRUE)

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
        body {
          background:
            radial-gradient(circle at top left, #f8efe2 0, #f8efe2 18%, transparent 42%),
            linear-gradient(180deg, #fffaf3 0%, #f4efe8 100%);
          color: #1f1f1b;
          font-family: Avenir Next, Segoe UI, Helvetica Neue, sans-serif;
        }
        .addr-title {
          margin: 18px 0 6px 0;
          font-size: 34px;
          font-weight: 700;
          letter-spacing: -0.03em;
        }
        .addr-subtitle {
          margin-bottom: 18px;
          max-width: 980px;
          color: #534d43;
          font-size: 15px;
          line-height: 1.5;
        }
        .addr-panel {
          background: rgba(255, 255, 255, 0.82);
          border: 1px solid rgba(80, 62, 41, 0.12);
          border-radius: 18px;
          box-shadow: 0 10px 30px rgba(64, 50, 31, 0.08);
          padding: 18px;
          margin-bottom: 18px;
          backdrop-filter: blur(6px);
        }
        .addr-panel h3 {
          margin-top: 0;
          margin-bottom: 10px;
          font-size: 18px;
          letter-spacing: -0.02em;
        }
        .addr-flow {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          gap: 14px;
        }
        .addr-card {
          border-radius: 16px;
          padding: 14px;
          border: 1px solid rgba(80, 62, 41, 0.12);
          background: linear-gradient(180deg, rgba(255,255,255,0.95), rgba(246,241,233,0.95));
          min-height: 170px;
        }
        .addr-card-title {
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #6a6258;
          margin-bottom: 8px;
        }
        .addr-card-main {
          font-size: 18px;
          font-weight: 700;
          line-height: 1.25;
          margin-bottom: 10px;
        }
        .addr-card-note {
          font-size: 13px;
          line-height: 1.45;
          color: #514a41;
          white-space: pre-wrap;
        }
        .addr-pill {
          display: inline-block;
          border-radius: 999px;
          padding: 4px 10px;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          margin-bottom: 8px;
        }
        .addr-pill-match {
          color: #0d4f3d;
          background: #d7f3e7;
        }
        .addr-pill-partial {
          color: #785403;
          background: #f7ebba;
        }
        .addr-pill-miss {
          color: #7d1d24;
          background: #f8d9dc;
        }
        .addr-pill-info {
          color: #1c4666;
          background: #d9ebfa;
        }
        .addr-kv {
          display: grid;
          grid-template-columns: 130px 1fr;
          gap: 8px 12px;
          font-size: 13px;
          line-height: 1.4;
        }
        .addr-kv-key {
          color: #6a6258;
          font-weight: 600;
        }
        .addr-kv-value {
          word-break: break-word;
        }
        .addr-table-wrap {
          overflow-x: auto;
        }
        table.addr-table {
          width: 100%;
          border-collapse: collapse;
          font-size: 13px;
          line-height: 1.35;
        }
        .addr-table th {
          text-align: left;
          background: #efe7dc;
          color: #473f35;
          padding: 9px 10px;
          border-bottom: 1px solid rgba(80, 62, 41, 0.14);
          white-space: nowrap;
        }
        .addr-table td {
          padding: 8px 10px;
          border-bottom: 1px solid rgba(80, 62, 41, 0.08);
          vertical-align: top;
        }
        .addr-empty {
          color: #7a7368;
          font-style: italic;
          margin: 0;
        }
        .addr-code {
          font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
          font-size: 12px;
        }
        .addr-error {
          border-radius: 14px;
          background: #f8d9dc;
          color: #651c22;
          padding: 12px 14px;
          font-weight: 600;
        }
        .form-control, .btn, .checkbox, .control-label, .nav-tabs {
          font-size: 13px;
        }
        "
      ))
    ),
    shiny::div(
      class = "addr-title",
      "Address parse and match explorer"
    ),
    shiny::div(
      class = "addr-subtitle",
      paste(
        "Type an address or pull a random example from voter_addresses().",
        "The app shows parser tags, how as_addr() maps those tags into addr",
        "fields, and which parts did or did not participate in matching",
        "against nad_example_data()$nad_addr."
      )
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::textInput(
          "address_input",
          "Address",
          value = voter_pool[[1]]
        ),
        shiny::actionButton("random_address", "Random voter address"),
        shiny::tags$hr(),
        shiny::checkboxInput("clean", "Clean text before tagging", value = TRUE),
        shiny::checkboxInput("map_state", "Map states to abbreviations", value = TRUE),
        shiny::checkboxInput("map_posttype", "Map street posttypes", value = TRUE),
        shiny::checkboxInput("map_directional", "Map directionals", value = TRUE),
        shiny::checkboxInput("map_pretype", "Map street pretypes", value = TRUE),
        shiny::tags$hr(),
        shiny::checkboxInput("zip_variants", "Allow ZIP variants", value = TRUE),
        shiny::numericInput(
          "name_phonetic_dist",
          "Street phonetic distance",
          value = 2,
          min = 0,
          step = 1
        ),
        shiny::numericInput(
          "name_fuzzy_dist",
          "Street fuzzy distance",
          value = 1,
          min = 0,
          step = 1
        ),
        shiny::numericInput(
          "number_fuzzy_dist",
          "House-number fuzzy distance",
          value = 1,
          min = 0,
          step = 1
        ),
        shiny::tags$hr(),
        shiny::checkboxInput(
          "match_street_predirectional",
          "Require street predirectional",
          value = TRUE
        ),
        shiny::checkboxInput(
          "match_street_posttype",
          "Require street posttype",
          value = TRUE
        ),
        shiny::checkboxInput(
          "match_street_pretype",
          "Require street pretype",
          value = TRUE
        ),
        shiny::checkboxInput(
          "match_street_postdirectional",
          "Require street postdirectional",
          value = FALSE
        )
      ),
      shiny::mainPanel(
        width = 8,
        shiny::uiOutput("app_body")
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$random_address, {
      shiny::updateTextInput(
        session,
        "address_input",
        value = sample(voter_pool, size = 1L)
      )
    })

    address_text <- shiny::debounce(
      shiny::reactive(trimws(input$address_input)),
      millis = 250
    )

    analysis <- shiny::reactive({
      if (!nzchar(address_text())) {
        return(list(error = "Enter an address to inspect."))
      }

      clean <- isTRUE(input$clean)
      clean_preview <- if (clean) clean_address_text(address_text()) else address_text()

      tryCatch(
        {
          parsed <- as_addr(
            address_text(),
            clean = clean,
            map_state = isTRUE(input$map_state),
            map_posttype = isTRUE(input$map_posttype),
            map_directional = isTRUE(input$map_directional),
            map_pretype = isTRUE(input$map_pretype)
          )

          diag <- addr_explorer_match_diagnostics(
            parsed = parsed,
            nad_index = nad_index,
            zip_variants = isTRUE(input$zip_variants),
            name_phonetic_dist = as.integer(input$name_phonetic_dist),
            name_fuzzy_dist = as.integer(input$name_fuzzy_dist),
            number_fuzzy_dist = as.integer(input$number_fuzzy_dist),
            match_street_predirectional = isTRUE(input$match_street_predirectional),
            match_street_posttype = isTRUE(input$match_street_posttype),
            match_street_pretype = isTRUE(input$match_street_pretype),
            match_street_postdirectional = isTRUE(input$match_street_postdirectional)
          )

          list(
            raw_input = address_text(),
            clean_preview = clean_preview,
            parsed = parsed,
            parsed_df = addr_explorer_one_row(parsed),
            tag_table = addr_explorer_tag_table(
              address_text(),
              parsed = parsed,
              clean = clean
            ),
            field_usage = addr_explorer_field_usage_table(
              parsed = parsed,
              diag = diag,
              match_street_predirectional =
                isTRUE(input$match_street_predirectional),
              match_street_posttype = isTRUE(input$match_street_posttype),
              match_street_pretype = isTRUE(input$match_street_pretype),
              match_street_postdirectional =
                isTRUE(input$match_street_postdirectional)
            ),
            stage_table = addr_explorer_stage_table(parsed = parsed, diag = diag),
            diag = diag
          )
        },
        error = function(e) {
          list(error = conditionMessage(e))
        }
      )
    })

    output$app_body <- shiny::renderUI({
      x <- analysis()

      if (!is.null(x$error)) {
        return(shiny::div(class = "addr-error", x$error))
      }

      shiny::tagList(
        shiny::div(
          class = "addr-panel",
          shiny::h3("Flow"),
          addr_explorer_flow_ui(x)
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("Parser tags to addr fields"),
          addr_explorer_table_ui(x$tag_table)
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("Normalized addr fields"),
          addr_explorer_table_ui(x$parsed_df)
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("How each addr field was used in matching"),
          addr_explorer_table_ui(x$field_usage)
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("Stage diagnostics"),
          addr_explorer_table_ui(x$stage_table)
        )
      )
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

addr_explorer_tag_map <- function() {
  data.frame(
    tag_label = c(
      "AddressNumberPrefix",
      "AddressNumber",
      "AddressNumberSuffix",
      "StreetNamePreDirectional",
      "StreetNamePreModifier",
      "StreetNamePreType",
      "StreetName",
      "StreetNamePostType",
      "StreetNamePostDirectional",
      "PlaceName",
      "StateName",
      "ZipCode",
      "BuildingName",
      "CornerOf",
      "IntersectionSeparator",
      "LandmarkName",
      "NotAddress",
      "OccupancyIdentifier",
      "OccupancyType",
      "Recipient",
      "SubaddressIdentifier",
      "SubaddressType",
      "USPSBoxGroupID",
      "USPSBoxGroupType",
      "USPSBoxID",
      "USPSBoxType"
    ),
    addr_field = c(
      "number_prefix",
      "number_digits",
      "number_suffix",
      "street_predirectional",
      "street_premodifier",
      "street_pretype",
      "street_name",
      "street_posttype",
      "street_postdirectional",
      "place_name",
      "place_state",
      "place_zipcode",
      rep(NA_character_, 14L)
    ),
    stringsAsFactors = FALSE
  )
}

addr_explorer_extract_tag <- function(tag_vec, label, collapse = " ") {
  if (length(tag_vec) == 1L && is.na(tag_vec)) {
    return(NA_character_)
  }
  if (length(tag_vec) == 0L) {
    return("")
  }
  vals <- unname(tag_vec[names(tag_vec) == label])
  if (length(vals) == 0L) {
    return("")
  }
  paste(unique(vals), collapse = collapse)
}

addr_explorer_one_row <- function(x) {
  as.data.frame(x[1], stringsAsFactors = FALSE)
}

addr_explorer_value <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x[[1]])) {
    return(NA_character_)
  }
  x[[1]]
}

addr_explorer_format_value <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x[[1]])) {
    return("NA")
  }
  if (!nzchar(x[[1]])) {
    return("<empty>")
  }
  x[[1]]
}

addr_explorer_tag_table <- function(address_text, parsed, clean = TRUE) {
  tags <- tag_usaddress(address_text, clean = clean)[[1]]
  if (length(tags) == 1L && is.na(tags)) {
    return(data.frame(
      parser_tag = "NA",
      parser_value = "NA",
      addr_field = "NA",
      mapped_value = "NA",
      note = "No tags were produced.",
      stringsAsFactors = FALSE
    ))
  }

  tag_df <- data.frame(
    parser_tag = names(tags),
    parser_value = unname(tags),
    stringsAsFactors = FALSE
  )
  tag_df$tag_index <- seq_len(nrow(tag_df))

  map_df <- addr_explorer_tag_map()
  out <- merge(tag_df, map_df, by.x = "parser_tag", by.y = "tag_label", all.x = TRUE)
  out <- out[order(out$tag_index), , drop = FALSE]

  parsed_df <- addr_explorer_one_row(parsed)
  raw_combined <- vapply(
    out$parser_tag,
    function(label) {
      addr_explorer_extract_tag(
        tags,
        label = label,
        collapse = if (label %in% c(
          "AddressNumberPrefix",
          "AddressNumber",
          "AddressNumberSuffix",
          "StateName",
          "ZipCode"
        )) "" else " "
      )
    },
    character(1)
  )

  mapped_value <- vapply(
    out$addr_field,
    function(field) {
      if (is.na(field)) {
        return(NA_character_)
      }
      parsed_df[[field]][[1]]
    },
    character(1)
  )

  note <- vapply(
    seq_len(nrow(out)),
    function(i) {
      if (is.na(out$addr_field[[i]])) {
        return("Ignored by as_addr().")
      }

      if (sum(out$parser_tag == out$parser_tag[[i]]) > 1L &&
          raw_combined[[i]] != out$parser_value[[i]]) {
        if (identical(raw_combined[[i]], mapped_value[[i]])) {
          return("Combined with other values sharing the same parser tag.")
        }
        return(paste("Combined, then normalized to", addr_explorer_format_value(mapped_value[[i]]), "."))
      }

      if (!identical(raw_combined[[i]], mapped_value[[i]])) {
        return(paste("Normalized to", addr_explorer_format_value(mapped_value[[i]]), "."))
      }

      "Copied into the addr field."
    },
    character(1)
  )

  data.frame(
    parser_tag = out$parser_tag,
    parser_value = out$parser_value,
    addr_field = ifelse(is.na(out$addr_field), "<unused>", out$addr_field),
    mapped_value = vapply(mapped_value, addr_explorer_format_value, character(1)),
    note = note,
    stringsAsFactors = FALSE
  )
}

addr_explorer_match_diagnostics <- function(
  parsed,
  nad_index,
  zip_variants = TRUE,
  name_phonetic_dist = 2L,
  name_fuzzy_dist = 1L,
  number_fuzzy_dist = 1L,
  match_street_predirectional = TRUE,
  match_street_posttype = TRUE,
  match_street_pretype = TRUE,
  match_street_postdirectional = FALSE
) {
  stopifnot(inherits(parsed, "addr"), length(parsed) == 1L)

  matched_zipcode <- match_zipcodes_prepared(
    parsed@place@zipcode,
    nad_index$zipcodes,
    zip_variants = zip_variants
  )
  zip_matched <- !is.na(matched_zipcode[[1]])

  zip_data <- if (zip_matched) nad_index$by_zip[[matched_zipcode[[1]]]] else NULL
  zip_pool_n <- if (is.null(zip_data)) 0L else length(zip_data$y)

  street_match <- addr_street()
  street_matched <- FALSE
  street_pool_n <- 0L
  street_scope_n <- 0L
  street_y_idx <- integer(0)

  if (zip_matched && !is.null(zip_data) && length(zip_data$y) > 0L) {
    street_scope_n <- length(unique(as.character(zip_data$y@street)))
    street_match <- match_addr_street(
      parsed@street,
      zip_data$y@street,
      name_phonetic_dist = as.integer(name_phonetic_dist),
      name_fuzzy_dist = as.integer(name_fuzzy_dist),
      match_street_predirectional = match_street_predirectional,
      match_street_posttype = match_street_posttype,
      match_street_pretype = match_street_pretype,
      match_street_postdirectional = match_street_postdirectional
    )
    street_matched <- !is.na(street_match)

    if (street_matched) {
      street_key <- addr_match_key(street_match)
      street_y_idx <- zip_data$y_by_street[[street_key]]
      street_pool_n <- length(street_y_idx)
    }
  }

  number_match <- addr_number()
  number_matched <- FALSE
  reference_match <- addr()

  if (street_matched && length(street_y_idx) > 0L) {
    number_match <- match_addr_number(
      parsed@number,
      zip_data$y[street_y_idx]@number,
      number_fuzzy_dist = as.integer(number_fuzzy_dist)
    )
    number_matched <- !is.na(number_match)

    if (number_matched) {
      street_key <- addr_match_key(street_match)
      number_key <- addr_match_key(number_match)
      number_bucket <- zip_data$y_by_number[[street_key]][[number_key]]
      reference_match <- zip_data$y[number_bucket[[1]]]
    }
  }

  final_match <- addr_match(
    parsed,
    nad_index,
    zip_variants = zip_variants,
    name_phonetic_dist = as.integer(name_phonetic_dist),
    name_fuzzy_dist = as.integer(name_fuzzy_dist),
    number_fuzzy_dist = as.integer(number_fuzzy_dist),
    match_street_predirectional = match_street_predirectional,
    match_street_posttype = match_street_posttype,
    match_street_pretype = match_street_pretype,
    match_street_postdirectional = match_street_postdirectional,
    progress = FALSE
  )

  list(
    matched_zipcode = matched_zipcode[[1]],
    zip_match_type = if (!zip_matched) {
      "none"
    } else if (identical(parsed@place@zipcode[[1]], matched_zipcode[[1]])) {
      "exact"
    } else {
      "variant"
    },
    zip_pool_n = zip_pool_n,
    street_match = street_match,
    street_match_type = if (!street_matched) {
      "none"
    } else if (identical(as.character(parsed@street), as.character(street_match))) {
      "exact"
    } else {
      "fuzzy_or_phonetic"
    },
    street_scope_n = street_scope_n,
    street_pool_n = street_pool_n,
    number_match = number_match,
    number_match_type = if (!number_matched) {
      "none"
    } else if (identical(as.character(parsed@number), as.character(number_match))) {
      "exact"
    } else {
      "fuzzy"
    },
    final_match = final_match,
    final_stage = as.character(addr_match_stage(final_match)),
    reference_match = reference_match
  )
}

addr_explorer_field_usage_table <- function(
  parsed,
  diag,
  match_street_predirectional = TRUE,
  match_street_posttype = TRUE,
  match_street_pretype = TRUE,
  match_street_postdirectional = FALSE
) {
  parsed_df <- addr_explorer_one_row(parsed)
  street_df <- addr_explorer_one_row(diag$street_match)
  number_df <- addr_explorer_one_row(diag$number_match)
  final_df <- addr_explorer_one_row(diag$final_match)

  zip_reached <- !identical(diag$zip_match_type, "none")
  street_reached <- zip_reached
  number_reached <- !identical(diag$street_match_type, "none")

  field_rule <- function(field) {
    switch(
      field,
      number_prefix = "Compared as part of the full addr_number after a street match.",
      number_digits = "Exact first, then fuzzy OSA matching within the matched street and ZIP.",
      number_suffix = "Compared as part of the full addr_number after a street match.",
      street_predirectional = if (match_street_predirectional) {
        "Required exact street-bucket component within the matched ZIP."
      } else {
        "Ignored because predirectional matching is turned off."
      },
      street_premodifier = "Required exact street-bucket component within the matched ZIP.",
      street_pretype = if (match_street_pretype) {
        "Required exact street-bucket component within the matched ZIP."
      } else {
        "Ignored because pretype matching is turned off."
      },
      street_name = "Exact street key first, then fuzzy and phonetic search within the matched ZIP.",
      street_posttype = if (match_street_posttype) {
        "Required exact street-bucket component within the matched ZIP."
      } else {
        "Ignored because posttype matching is turned off."
      },
      street_postdirectional = if (match_street_postdirectional) {
        "Required exact street-bucket component within the matched ZIP."
      } else {
        "Ignored because postdirectional matching is turned off."
      },
      place_name = "Not used by addr_match().",
      place_state = "Not used by addr_match().",
      place_zipcode = "Exact ZIP first, then optional ZIP-variant search.",
      "Package field."
    )
  }

  field_used <- function(field) {
    if (field %in% c("place_name", "place_state")) {
      return("no")
    }
    if (field == "place_zipcode") {
      if (is.na(parsed_df[[field]][[1]]) || !nzchar(parsed_df[[field]][[1]])) {
        return("no")
      }
      return("yes")
    }
    if (grepl("^street_", field)) {
      if (!street_reached) {
        return("not reached")
      }
      if (field == "street_predirectional" && !match_street_predirectional) {
        return("off")
      }
      if (field == "street_pretype" && !match_street_pretype) {
        return("off")
      }
      if (field == "street_posttype" && !match_street_posttype) {
        return("off")
      }
      if (field == "street_postdirectional" && !match_street_postdirectional) {
        return("off")
      }
      return("yes")
    }
    if (!number_reached) {
      return("not reached")
    }
    "yes"
  }

  matched_value <- function(field) {
    if (field == "place_zipcode") {
      return(addr_explorer_format_value(diag$matched_zipcode))
    }
    if (grepl("^street_", field)) {
      return(addr_explorer_format_value(street_df[[field]][[1]]))
    }
    if (grepl("^number_", field)) {
      return(addr_explorer_format_value(number_df[[field]][[1]]))
    }
    addr_explorer_format_value(final_df[[field]][[1]])
  }

  fields <- names(parsed_df)
  data.frame(
    addr_field = fields,
    parsed_value = vapply(parsed_df[1, fields, drop = FALSE], addr_explorer_format_value, character(1)),
    match_stage = c(
      rep("number", 3L),
      rep("street", 6L),
      "none",
      "none",
      "zip"
    ),
    used_this_run = vapply(fields, field_used, character(1)),
    matched_value = vapply(fields, matched_value, character(1)),
    rule = vapply(fields, field_rule, character(1)),
    stringsAsFactors = FALSE
  )
}

addr_explorer_stage_table <- function(parsed, diag) {
  data.frame(
    stage = c("zip", "street", "number", "final"),
    status = c(
      if (identical(diag$zip_match_type, "none")) "no match" else diag$zip_match_type,
      if (identical(diag$zip_match_type, "none")) "skipped" else if (identical(diag$street_match_type, "none")) "no match" else diag$street_match_type,
      if (identical(diag$street_match_type, "none")) "skipped" else if (identical(diag$number_match_type, "none")) "no match" else diag$number_match_type,
      diag$final_stage
    ),
    input = c(
      addr_explorer_format_value(parsed@place@zipcode),
      addr_explorer_format_value(as.character(parsed@street)),
      addr_explorer_format_value(as.character(parsed@number)),
      addr_explorer_format_value(as.character(parsed))
    ),
    matched = c(
      addr_explorer_format_value(diag$matched_zipcode),
      addr_explorer_format_value(as.character(diag$street_match)),
      addr_explorer_format_value(as.character(diag$number_match)),
      addr_explorer_format_value(as.character(diag$final_match))
    ),
    candidate_pool = c(
      prettyNum(diag$zip_pool_n, big.mark = ","),
      if (identical(diag$zip_match_type, "none")) {
        "0"
      } else {
        paste0(
          prettyNum(diag$street_scope_n, big.mark = ","),
          " streets / ",
          prettyNum(diag$zip_pool_n, big.mark = ","),
          " addresses"
        )
      },
      if (identical(diag$street_match_type, "none")) {
        "0"
      } else {
        prettyNum(diag$street_pool_n, big.mark = ",")
      },
      if (identical(diag$final_stage, "number")) {
        "1 best match"
      } else if (identical(diag$final_stage, "street")) {
        "street-only result"
      } else if (identical(diag$final_stage, "zip")) {
        "zip-only result"
      } else {
        "no result"
      }
    ),
    stringsAsFactors = FALSE
  )
}

addr_explorer_pill <- function(text, class) {
  shiny::tags$span(class = paste("addr-pill", class), text)
}

addr_explorer_card <- function(title, pill, main, note = NULL) {
  shiny::tags$div(
    class = "addr-card",
    shiny::tags$div(class = "addr-card-title", title),
    pill,
    shiny::tags$div(class = "addr-card-main", main),
    shiny::tags$div(class = "addr-card-note", note)
  )
}

addr_explorer_flow_ui <- function(x) {
  diag <- x$diag

  clean_note <- if (identical(x$raw_input, x$clean_preview)) {
    paste("Input:", x$raw_input)
  } else {
    paste("Input:", x$raw_input, "\nCleaned:", x$clean_preview)
  }

  zip_card <- if (identical(diag$zip_match_type, "none")) {
    addr_explorer_card(
      "ZIP stage",
      addr_explorer_pill("no match", "addr-pill-miss"),
      "No ZIP candidate",
      paste(
        "Parsed ZIP:",
        addr_explorer_format_value(x$parsed@place@zipcode),
        "\nNo reference ZIP matched."
      )
    )
  } else {
    addr_explorer_card(
      "ZIP stage",
      addr_explorer_pill(diag$zip_match_type, if (identical(diag$zip_match_type, "exact")) "addr-pill-match" else "addr-pill-partial"),
      addr_explorer_format_value(diag$matched_zipcode),
      paste(
        "Parsed ZIP:", addr_explorer_format_value(x$parsed@place@zipcode),
        "\nUnique NAD addresses in ZIP:", prettyNum(diag$zip_pool_n, big.mark = ",")
      )
    )
  }

  street_card <- if (identical(diag$zip_match_type, "none")) {
    addr_explorer_card(
      "Street stage",
      addr_explorer_pill("skipped", "addr-pill-info"),
      "ZIP match required first",
      "Street matching was not attempted because the ZIP stage failed."
    )
  } else if (identical(diag$street_match_type, "none")) {
    addr_explorer_card(
      "Street stage",
      addr_explorer_pill("no match", "addr-pill-miss"),
      "No street candidate",
      paste(
        "Parsed street:", addr_explorer_format_value(as.character(x$parsed@street)),
        "\nStreet scope:", prettyNum(diag$street_scope_n, big.mark = ","), "unique streets"
      )
    )
  } else {
    addr_explorer_card(
      "Street stage",
      addr_explorer_pill(gsub("_", "/", diag$street_match_type), if (identical(diag$street_match_type, "exact")) "addr-pill-match" else "addr-pill-partial"),
      addr_explorer_format_value(as.character(diag$street_match)),
      paste(
        "Parsed street:", addr_explorer_format_value(as.character(x$parsed@street)),
        "\nAddresses sharing the matched street:", prettyNum(diag$street_pool_n, big.mark = ",")
      )
    )
  }

  number_card <- if (identical(diag$street_match_type, "none")) {
    addr_explorer_card(
      "Number stage",
      addr_explorer_pill("skipped", "addr-pill-info"),
      "Street match required first",
      "House-number matching was not attempted because the street stage failed."
    )
  } else if (identical(diag$number_match_type, "none")) {
    addr_explorer_card(
      "Number stage",
      addr_explorer_pill("no match", "addr-pill-miss"),
      "No number candidate",
      paste(
        "Parsed number:", addr_explorer_format_value(as.character(x$parsed@number)),
        "\nCandidate addresses on matched street:", prettyNum(diag$street_pool_n, big.mark = ",")
      )
    )
  } else {
    addr_explorer_card(
      "Number stage",
      addr_explorer_pill(diag$number_match_type, if (identical(diag$number_match_type, "exact")) "addr-pill-match" else "addr-pill-partial"),
      addr_explorer_format_value(as.character(diag$number_match)),
      paste(
        "Parsed number:", addr_explorer_format_value(as.character(x$parsed@number)),
        "\nFinal matched address:", addr_explorer_format_value(as.character(diag$final_match))
      )
    )
  }

  shiny::tags$div(
    class = "addr-flow",
    addr_explorer_card(
      "Input",
      addr_explorer_pill("source", "addr-pill-info"),
      addr_explorer_format_value(x$raw_input),
      clean_note
    ),
    addr_explorer_card(
      "as_addr()",
      addr_explorer_pill("normalized", "addr-pill-info"),
      addr_explorer_format_value(as.character(x$parsed)),
      paste(
        "Street:", addr_explorer_format_value(as.character(x$parsed@street)),
        "\nPlace:", paste(
          addr_explorer_format_value(x$parsed@place@name),
          addr_explorer_format_value(x$parsed@place@state),
          addr_explorer_format_value(x$parsed@place@zipcode)
        )
      )
    ),
    zip_card,
    street_card,
    number_card
  )
}

addr_explorer_table_ui <- function(df) {
  if (nrow(df) == 0L) {
    return(shiny::tags$p(class = "addr-empty", "No rows to show."))
  }

  header <- shiny::tags$tr(
    lapply(names(df), function(name) shiny::tags$th(name))
  )
  body <- lapply(seq_len(nrow(df)), function(i) {
    shiny::tags$tr(
      lapply(df[i, , drop = FALSE], function(value) {
        shiny::tags$td(
          class = "addr-code",
          htmltools::htmlEscape(addr_explorer_format_value(value[[1]]))
        )
      })
    )
  })

  shiny::tags$div(
    class = "addr-table-wrap",
    shiny::tags$table(
      class = "addr-table",
      shiny::tags$thead(header),
      shiny::tags$tbody(body)
    )
  )
}
