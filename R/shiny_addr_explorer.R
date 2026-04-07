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
  stopifnot(
    "launch.browser must be TRUE or FALSE" = is.logical(launch.browser) &&
      length(launch.browser) == 1L &&
      !is.na(launch.browser)
  )
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
  nad_index <- nad_example_data(match_prepared = TRUE)

  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
        body {
          background: #ffffff;
          color: #2f3a44;
          font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif;
        }
        .container-fluid {
          max-width: 1400px;
          padding: 0 20px 24px 20px;
        }
        .addr-title {
          margin: 18px 0 6px 0;
          font-size: 30px;
          font-weight: 700;
          color: #2f3a44;
        }
        .addr-subtitle {
          margin-bottom: 18px;
          max-width: 980px;
          color: #5a6772;
          font-size: 14px;
          line-height: 1.5;
        }
        .well {
          background: #f7f7f8;
          border: 1px solid #dddddf;
          border-radius: 8px;
          padding: 16px;
        }
        .addr-panel {
          background: #ffffff;
          border: 1px solid #dddddf;
          border-radius: 8px;
          padding: 16px;
          margin-bottom: 16px;
        }
        .addr-panel h3 {
          margin-top: 0;
          margin-bottom: 12px;
          font-size: 18px;
          color: #2f3a44;
        }
        .addr-kv {
          display: grid;
          grid-template-columns: 140px 1fr;
          gap: 8px 12px;
          font-size: 13px;
          line-height: 1.4;
        }
        .addr-kv-key {
          color: #5a6772;
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
          background: #f1f3f5;
          color: #2f3a44;
          padding: 9px 10px;
          border: 1px solid #d9dde2;
          white-space: nowrap;
        }
        .addr-table td {
          padding: 8px 10px;
          border: 1px solid #e3e6ea;
          vertical-align: top;
        }
        .addr-empty {
          color: #5a6772;
          font-style: italic;
          margin: 0;
        }
        .addr-code {
          font-family: \"Source Code Pro\", \"SFMono-Regular\", Menlo, Consolas, monospace;
          font-size: 12px;
        }
        .addr-error {
          border-radius: 8px;
          background: #f8d7da;
          color: #7c2732;
          padding: 12px 14px;
          font-weight: 600;
        }
        .btn-default {
          background: #e9ecef;
          border-color: #ced4da;
          color: #2f3a44;
        }
        .btn-default:hover,
        .btn-default:focus {
          background: #dde2e6;
          border-color: #c5cbd3;
          color: #2f3a44;
        }
        .form-control:focus {
          border-color: #6c8aa0;
          box-shadow: none;
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
        "This app is intentionally basic: it shows parser output, normalized",
        "addr fields, and the matching stages against nad_example_data()$nad_addr."
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
            stage_table = addr_explorer_stage_table(
              parsed = parsed,
              diag = diag
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

      shiny::tagList(
        shiny::div(
          class = "addr-panel",
          shiny::h3("Current input"),
          addr_explorer_kv_ui(c(
            "raw input" = x$raw_input,
            "cleaned input" = x$clean_preview
          ))
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("Parser tags"),
          addr_explorer_table_ui(x$tag_table)
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("Normalized addr"),
          addr_explorer_kv_ui(c(
            "addr" = as.character(x$parsed),
            "street" = as.character(x$parsed@street),
            "number" = as.character(x$parsed@number),
            "place" = paste(
              addr_explorer_format_value(x$parsed@place@name),
              addr_explorer_format_value(x$parsed@place@state),
              addr_explorer_format_value(x$parsed@place@zipcode)
            )
          )),
          shiny::tags$div(style = "margin-top: 12px;"),
          addr_explorer_table_ui(x$parsed_df)
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("Match stages"),
          addr_explorer_table_ui(x$stage_table)
        ),
        shiny::div(
          class = "addr-panel",
          shiny::h3("Final match"),
          addr_explorer_kv_ui(c(
            "final stage" = x$diag$final_stage,
            "final match" = as.character(x$diag$final_match),
            "matched ZIP" = as.character(x$diag$matched_zipcode),
            "matched street" = as.character(x$diag$street_match),
            "matched number" = as.character(x$diag$number_match)
          ))
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

addr_explorer_kv_ui <- function(values) {
  shiny::tags$div(
    class = "addr-kv",
    do.call(
      shiny::tagList,
      lapply(names(values), function(label) {
        list(
          shiny::tags$div(class = "addr-kv-key", label),
          shiny::tags$div(
            class = "addr-kv-value addr-code",
            addr_explorer_format_value(values[[label]])
          )
        )
      })
    )
  )
}

addr_explorer_stage_table <- function(parsed, diag) {
  data.frame(
    stage = c("zip", "street", "number", "final"),
    status = c(
      if (identical(diag$zip_match_type, "none")) {
        "no match"
      } else {
        diag$zip_match_type
      },
      if (identical(diag$zip_match_type, "none")) {
        "skipped"
      } else if (identical(diag$street_match_type, "none")) {
        "no match"
      } else {
        diag$street_match_type
      },
      if (identical(diag$street_match_type, "none")) {
        "skipped"
      } else if (identical(diag$number_match_type, "none")) {
        "no match"
      } else {
        diag$number_match_type
      },
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
          addr_explorer_format_value(value[[1]])
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
