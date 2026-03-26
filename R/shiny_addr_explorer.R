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
            radial-gradient(circle at top left, rgba(234, 206, 197, 0.42) 0, rgba(234, 206, 197, 0.42) 18%, transparent 42%),
            linear-gradient(180deg, #ffffff 0%, #fcf6f4 100%);
          color: #396175;
          font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif;
        }
        .container-fluid {
          max-width: 1540px;
          padding: 0 20px 24px 20px;
        }
        .addr-title {
          margin: 18px 0 6px 0;
          font-size: 34px;
          font-weight: 700;
          letter-spacing: -0.03em;
          color: #396175;
        }
        .addr-subtitle {
          margin-bottom: 18px;
          max-width: 980px;
          color: #4a7283;
          font-size: 15px;
          line-height: 1.5;
        }
        .well {
          background: rgba(255, 255, 255, 0.94);
          border: 1px solid rgba(194, 130, 115, 0.22);
          border-radius: 18px;
          box-shadow: 0 12px 30px rgba(57, 97, 117, 0.08);
          padding: 16px;
        }
        .addr-panel {
          background: rgba(255, 255, 255, 0.9);
          border: 1px solid rgba(194, 130, 115, 0.18);
          border-radius: 18px;
          box-shadow: 0 12px 30px rgba(57, 97, 117, 0.08);
          padding: 20px;
          margin-bottom: 18px;
          backdrop-filter: blur(6px);
        }
        .addr-panel h3 {
          margin-top: 0;
          margin-bottom: 10px;
          font-size: 18px;
          letter-spacing: -0.02em;
          color: #396175;
        }
        .addr-flow {
          display: grid;
          grid-template-columns: 1fr;
          gap: 16px;
        }
        .addr-card {
          border-radius: 16px;
          padding: 18px;
          border: 1px solid rgba(194, 130, 115, 0.18);
          border-left: 5px solid #C28273;
          background: linear-gradient(180deg, #ffffff, #fcf6f4);
        }
        .addr-card-title {
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #7a8f99;
          margin-bottom: 8px;
        }
        .addr-card-main {
          font-size: 22px;
          font-weight: 700;
          line-height: 1.3;
          margin-bottom: 10px;
          color: #2f5364;
        }
        .addr-card-note {
          font-size: 13px;
          line-height: 1.45;
          color: #4a7283;
          white-space: pre-wrap;
        }
        .addr-card-details {
          margin-top: 16px;
          padding-top: 14px;
          border-top: 1px solid rgba(194, 130, 115, 0.22);
          display: grid;
          gap: 12px;
        }
        .addr-detail-section {
          display: grid;
          gap: 8px;
        }
        .addr-detail-section h4 {
          margin: 0;
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #7a8f99;
        }
        .addr-structure-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
          gap: 12px;
        }
        .addr-structure-card {
          border-radius: 12px;
          border: 1px solid rgba(194, 130, 115, 0.16);
          background: #fff8f6;
          padding: 12px;
        }
        .addr-stage-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
          gap: 10px 14px;
        }
        .addr-stage-stat {
          border-radius: 12px;
          border: 1px solid rgba(194, 130, 115, 0.14);
          background: rgba(255, 255, 255, 0.85);
          padding: 10px 12px;
        }
        .addr-stage-label {
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #7a8f99;
          margin-bottom: 4px;
        }
        .addr-stage-value {
          font-size: 13px;
          line-height: 1.45;
          color: #2f5364;
          word-break: break-word;
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
          color: #245d49;
          background: #dff1ea;
        }
        .addr-pill-partial {
          color: #8b5d26;
          background: #f7e2cf;
        }
        .addr-pill-miss {
          color: #8a3d47;
          background: #f5dde0;
        }
        .addr-pill-info {
          color: #396175;
          background: #eaf1f4;
        }
        .addr-kv {
          display: grid;
          grid-template-columns: 130px 1fr;
          gap: 8px 12px;
          font-size: 13px;
          line-height: 1.4;
        }
        .addr-kv-key {
          color: #7a8f99;
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
          background: rgba(255, 255, 255, 0.92);
          border-radius: 12px;
          overflow: hidden;
        }
        .addr-table th {
          text-align: left;
          background: #f7ece7;
          color: #396175;
          padding: 9px 10px;
          border-bottom: 1px solid rgba(194, 130, 115, 0.18);
          white-space: nowrap;
        }
        .addr-table td {
          padding: 8px 10px;
          border-bottom: 1px solid rgba(194, 130, 115, 0.1);
          vertical-align: top;
        }
        .addr-empty {
          color: #7a8f99;
          font-style: italic;
          margin: 0;
        }
        .addr-code {
          font-family: \"Source Code Pro\", \"SFMono-Regular\", Menlo, Consolas, monospace;
          font-size: 12px;
        }
        .addr-error {
          border-radius: 14px;
          background: #f5dde0;
          color: #8a3d47;
          padding: 12px 14px;
          font-weight: 600;
        }
        .btn-default {
          background: #C28273;
          border-color: #C28273;
          color: #ffffff;
        }
        .btn-default:hover,
        .btn-default:focus {
          background: #b16f60;
          border-color: #b16f60;
          color: #ffffff;
        }
        .form-control:focus {
          border-color: #C28273;
          box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.075), 0 0 6px rgba(194, 130, 115, 0.28);
        }
        .form-control, .btn, .checkbox, .control-label, .nav-tabs {
          font-size: 13px;
        }
        @media (min-width: 768px) {
          .col-sm-3 {
            width: 24%;
          }
          .col-sm-9 {
            width: 76%;
          }
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
        "The page is organized as the parse-and-match flow, with parser output,",
        "normalized addr structure, and stage diagnostics shown inside each step",
        "against nad_example_data()$nad_addr."
      )
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::textInput(
          "address_input",
          "Address",
          value = voter_pool[[1]]
        ),
        shiny::actionButton("random_address", "Random voter address"),
        shiny::tags$hr(),
        shiny::checkboxInput(
          "clean",
          "Clean text before tagging",
          value = TRUE
        ),
        shiny::checkboxInput(
          "map_state",
          "Map states to abbreviations",
          value = TRUE
        ),
        shiny::checkboxInput(
          "map_posttype",
          "Map street posttypes",
          value = TRUE
        ),
        shiny::checkboxInput(
          "map_directional",
          "Map directionals",
          value = TRUE
        ),
        shiny::checkboxInput(
          "map_pretype",
          "Map street pretypes",
          value = TRUE
        ),
        shiny::tags$hr(),
        shiny::checkboxInput(
          "zip_variants",
          "Allow ZIP variants",
          value = TRUE
        ),
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
        width = 9,
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
      clean_preview <- if (clean) {
        clean_address_text(address_text())
      } else {
        address_text()
      }

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
            match_street_predirectional = isTRUE(
              input$match_street_predirectional
            ),
            match_street_posttype = isTRUE(input$match_street_posttype),
            match_street_pretype = isTRUE(input$match_street_pretype),
            match_street_postdirectional = isTRUE(
              input$match_street_postdirectional
            )
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

      shiny::div(
        class = "addr-panel",
        shiny::h3("Flow"),
        addr_explorer_flow_ui(x)
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
  out <- merge(
    tag_df,
    map_df,
    by.x = "parser_tag",
    by.y = "tag_label",
    all.x = TRUE
  )
  out <- out[order(out$tag_index), , drop = FALSE]

  parsed_df <- addr_explorer_one_row(parsed)
  raw_combined <- vapply(
    out$parser_tag,
    function(label) {
      addr_explorer_extract_tag(
        tags,
        label = label,
        collapse = if (
          label %in%
            c(
              "AddressNumberPrefix",
              "AddressNumber",
              "AddressNumberSuffix",
              "StateName",
              "ZipCode"
            )
        ) {
          ""
        } else {
          " "
        }
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

      if (
        sum(out$parser_tag == out$parser_tag[[i]]) > 1L &&
          raw_combined[[i]] != out$parser_value[[i]]
      ) {
        if (identical(raw_combined[[i]], mapped_value[[i]])) {
          return("Combined with other values sharing the same parser tag.")
        }
        return(paste(
          "Combined, then normalized to",
          addr_explorer_format_value(mapped_value[[i]]),
          "."
        ))
      }

      if (!identical(raw_combined[[i]], mapped_value[[i]])) {
        return(paste(
          "Normalized to",
          addr_explorer_format_value(mapped_value[[i]]),
          "."
        ))
      }

      "Copied into the addr field."
    },
    character(1)
  )

  data.frame(
    parser_tag = out$parser_tag,
    parser_value = out$parser_value,
    addr_field = ifelse(is.na(out$addr_field), "<unused>", out$addr_field),
    mapped_value = vapply(
      mapped_value,
      addr_explorer_format_value,
      character(1)
    ),
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

  zip_data <- if (zip_matched) {
    nad_index$by_zip[[matched_zipcode[[1]]]]
  } else {
    NULL
  }
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
    } else if (
      identical(as.character(parsed@street), as.character(street_match))
    ) {
      "exact"
    } else {
      "fuzzy_or_phonetic"
    },
    street_scope_n = street_scope_n,
    street_pool_n = street_pool_n,
    number_match = number_match,
    number_match_type = if (!number_matched) {
      "none"
    } else if (
      identical(as.character(parsed@number), as.character(number_match))
    ) {
      "exact"
    } else {
      "fuzzy"
    },
    final_match = final_match,
    final_stage = as.character(addr_match_stage(final_match)),
    reference_match = reference_match
  )
}

addr_explorer_pill <- function(text, class) {
  shiny::tags$span(class = paste("addr-pill", class), text)
}

addr_explorer_card <- function(title, pill, main, note = NULL, details = NULL) {
  shiny::tags$div(
    class = "addr-card",
    shiny::tags$div(class = "addr-card-title", title),
    pill,
    shiny::tags$div(class = "addr-card-main", main),
    shiny::tags$div(class = "addr-card-note", note),
    if (!is.null(details)) {
      shiny::tags$div(class = "addr-card-details", details)
    }
  )
}

addr_explorer_detail_section <- function(title, content) {
  shiny::tags$div(
    class = "addr-detail-section",
    shiny::tags$h4(title),
    content
  )
}

addr_explorer_stage_stats_ui <- function(values) {
  shiny::tags$div(
    class = "addr-stage-grid",
    lapply(names(values), function(label) {
      shiny::tags$div(
        class = "addr-stage-stat",
        shiny::tags$div(class = "addr-stage-label", label),
        shiny::tags$div(
          class = "addr-stage-value addr-code",
          htmltools::htmlEscape(addr_explorer_format_value(values[[label]]))
        )
      )
    })
  )
}

addr_explorer_structure_ui <- function(parsed) {
  shiny::tags$div(
    class = "addr-structure-grid",
    addr_explorer_detail_section(
      "Number",
      shiny::tags$div(
        class = "addr-structure-card",
        shiny::tags$div(
          class = "addr-kv",
          shiny::tags$div(class = "addr-kv-key", "prefix"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@number@prefix)
          ),
          shiny::tags$div(class = "addr-kv-key", "digits"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@number@digits)
          ),
          shiny::tags$div(class = "addr-kv-key", "suffix"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@number@suffix)
          )
        )
      )
    ),
    addr_explorer_detail_section(
      "Street",
      shiny::tags$div(
        class = "addr-structure-card",
        shiny::tags$div(
          class = "addr-kv",
          shiny::tags$div(class = "addr-kv-key", "predirectional"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@street@predirectional)
          ),
          shiny::tags$div(class = "addr-kv-key", "premodifier"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@street@premodifier)
          ),
          shiny::tags$div(class = "addr-kv-key", "pretype"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@street@pretype)
          ),
          shiny::tags$div(class = "addr-kv-key", "name"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@street@name)
          ),
          shiny::tags$div(class = "addr-kv-key", "posttype"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@street@posttype)
          ),
          shiny::tags$div(class = "addr-kv-key", "postdirectional"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@street@postdirectional)
          )
        )
      )
    ),
    addr_explorer_detail_section(
      "Place",
      shiny::tags$div(
        class = "addr-structure-card",
        shiny::tags$div(
          class = "addr-kv",
          shiny::tags$div(class = "addr-kv-key", "name"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@place@name)
          ),
          shiny::tags$div(class = "addr-kv-key", "state"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@place@state)
          ),
          shiny::tags$div(class = "addr-kv-key", "zipcode"),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(parsed@place@zipcode)
          )
        )
      )
    )
  )
}

addr_explorer_flow_ui <- function(x) {
  diag <- x$diag

  clean_note <- if (identical(x$raw_input, x$clean_preview)) {
    paste("Input:", x$raw_input)
  } else {
    paste("Input:", x$raw_input, "\nCleaned:", x$clean_preview)
  }

  input_card <- addr_explorer_card(
    "Input",
    addr_explorer_pill("source", "addr-pill-info"),
    addr_explorer_format_value(x$raw_input),
    clean_note,
    details = addr_explorer_detail_section(
      "Parser tags",
      addr_explorer_table_ui(x$tag_table)
    )
  )

  normalized_card <- addr_explorer_card(
    "as_addr()",
    addr_explorer_pill("normalized", "addr-pill-info"),
    addr_explorer_format_value(as.character(x$parsed)),
    "The normalized addr object drives every later matching stage.",
    details = shiny::tagList(
      addr_explorer_detail_section(
        "addr structure",
        addr_explorer_structure_ui(x$parsed)
      ),
      addr_explorer_detail_section(
        "Normalized fields",
        addr_explorer_table_ui(x$parsed_df)
      )
    )
  )

  zip_card <- if (identical(diag$zip_match_type, "none")) {
    addr_explorer_card(
      "ZIP stage",
      addr_explorer_pill("no match", "addr-pill-miss"),
      "No ZIP candidate",
      paste(
        "Parsed ZIP:",
        addr_explorer_format_value(x$parsed@place@zipcode),
        "\nNo reference ZIP matched."
      ),
      details = addr_explorer_detail_section(
        "Stage diagnostics",
        addr_explorer_stage_stats_ui(c(
          "status" = "no match",
          "parsed ZIP" = as.character(x$parsed@place@zipcode),
          "matched ZIP" = NA_character_,
          "candidate pool" = "0"
        ))
      )
    )
  } else {
    addr_explorer_card(
      "ZIP stage",
      addr_explorer_pill(
        diag$zip_match_type,
        if (identical(diag$zip_match_type, "exact")) {
          "addr-pill-match"
        } else {
          "addr-pill-partial"
        }
      ),
      addr_explorer_format_value(diag$matched_zipcode),
      paste(
        "Parsed ZIP:",
        addr_explorer_format_value(x$parsed@place@zipcode),
        "\nUnique NAD addresses in ZIP:",
        prettyNum(diag$zip_pool_n, big.mark = ",")
      ),
      details = addr_explorer_detail_section(
        "Stage diagnostics",
        addr_explorer_stage_stats_ui(c(
          "status" = diag$zip_match_type,
          "parsed ZIP" = as.character(x$parsed@place@zipcode),
          "matched ZIP" = as.character(diag$matched_zipcode),
          "candidate pool" = prettyNum(diag$zip_pool_n, big.mark = ",")
        ))
      )
    )
  }

  street_card <- if (identical(diag$zip_match_type, "none")) {
    addr_explorer_card(
      "Street stage",
      addr_explorer_pill("skipped", "addr-pill-info"),
      "ZIP match required first",
      "Street matching was not attempted because the ZIP stage failed.",
      details = addr_explorer_detail_section(
        "Stage diagnostics",
        addr_explorer_stage_stats_ui(c(
          "status" = "skipped",
          "parsed street" = as.character(x$parsed@street),
          "matched street" = NA_character_,
          "street scope" = "0",
          "candidate pool" = "0"
        ))
      )
    )
  } else if (identical(diag$street_match_type, "none")) {
    addr_explorer_card(
      "Street stage",
      addr_explorer_pill("no match", "addr-pill-miss"),
      "No street candidate",
      paste(
        "Parsed street:",
        addr_explorer_format_value(as.character(x$parsed@street)),
        "\nStreet scope:",
        prettyNum(diag$street_scope_n, big.mark = ","),
        "unique streets"
      ),
      details = addr_explorer_detail_section(
        "Stage diagnostics",
        addr_explorer_stage_stats_ui(c(
          "status" = "no match",
          "parsed street" = as.character(x$parsed@street),
          "matched street" = NA_character_,
          "street scope" = paste(
            prettyNum(diag$street_scope_n, big.mark = ","),
            "unique streets"
          ),
          "candidate pool" = "0"
        ))
      )
    )
  } else {
    addr_explorer_card(
      "Street stage",
      addr_explorer_pill(
        gsub("_", "/", diag$street_match_type),
        if (identical(diag$street_match_type, "exact")) {
          "addr-pill-match"
        } else {
          "addr-pill-partial"
        }
      ),
      addr_explorer_format_value(as.character(diag$street_match)),
      paste(
        "Parsed street:",
        addr_explorer_format_value(as.character(x$parsed@street)),
        "\nAddresses sharing the matched street:",
        prettyNum(diag$street_pool_n, big.mark = ",")
      ),
      details = addr_explorer_detail_section(
        "Stage diagnostics",
        addr_explorer_stage_stats_ui(c(
          "status" = gsub("_", "/", diag$street_match_type),
          "parsed street" = as.character(x$parsed@street),
          "matched street" = as.character(diag$street_match),
          "street scope" = paste(
            prettyNum(diag$street_scope_n, big.mark = ","),
            "unique streets"
          ),
          "candidate pool" = prettyNum(diag$street_pool_n, big.mark = ",")
        ))
      )
    )
  }

  number_card <- if (identical(diag$street_match_type, "none")) {
    addr_explorer_card(
      "Number stage",
      addr_explorer_pill("skipped", "addr-pill-info"),
      "Street match required first",
      "House-number matching was not attempted because the street stage failed.",
      details = addr_explorer_detail_section(
        "Stage diagnostics",
        addr_explorer_stage_stats_ui(c(
          "status" = "skipped",
          "parsed number" = as.character(x$parsed@number),
          "matched number" = NA_character_,
          "final stage" = diag$final_stage,
          "final match" = as.character(diag$final_match)
        ))
      )
    )
  } else if (identical(diag$number_match_type, "none")) {
    addr_explorer_card(
      "Number stage",
      addr_explorer_pill("no match", "addr-pill-miss"),
      "No number candidate",
      paste(
        "Parsed number:",
        addr_explorer_format_value(as.character(x$parsed@number)),
        "\nCandidate addresses on matched street:",
        prettyNum(diag$street_pool_n, big.mark = ",")
      ),
      details = shiny::tagList(
        addr_explorer_detail_section(
          "Stage diagnostics",
          addr_explorer_stage_stats_ui(c(
            "status" = "no match",
            "parsed number" = as.character(x$parsed@number),
            "matched number" = NA_character_,
            "candidate pool" = prettyNum(diag$street_pool_n, big.mark = ","),
            "final stage" = diag$final_stage
          ))
        ),
        addr_explorer_detail_section(
          "Best available match",
          addr_explorer_stage_stats_ui(c(
            "final match" = as.character(diag$final_match)
          ))
        )
      )
    )
  } else {
    addr_explorer_card(
      "Number stage",
      addr_explorer_pill(
        diag$number_match_type,
        if (identical(diag$number_match_type, "exact")) {
          "addr-pill-match"
        } else {
          "addr-pill-partial"
        }
      ),
      addr_explorer_format_value(as.character(diag$number_match)),
      paste(
        "Parsed number:",
        addr_explorer_format_value(as.character(x$parsed@number)),
        "\nFinal matched address:",
        addr_explorer_format_value(as.character(diag$final_match))
      ),
      details = shiny::tagList(
        addr_explorer_detail_section(
          "Stage diagnostics",
          addr_explorer_stage_stats_ui(c(
            "status" = diag$number_match_type,
            "parsed number" = as.character(x$parsed@number),
            "matched number" = as.character(diag$number_match),
            "candidate pool" = prettyNum(diag$street_pool_n, big.mark = ","),
            "final stage" = diag$final_stage
          ))
        ),
        addr_explorer_detail_section(
          "Matched address",
          addr_explorer_stage_stats_ui(c(
            "final match" = as.character(diag$final_match)
          ))
        )
      )
    )
  }

  shiny::tags$div(
    class = "addr-flow",
    input_card,
    normalized_card,
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
