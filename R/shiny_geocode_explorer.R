#' Launch the address geocoding explorer
#'
#' Opens a minimal Shiny app that geocodes one typed address with
#' [geocode()] and maps the result with leaflet.
#'
#' @param launch.browser logical; passed to [shiny::runApp()]
#' @returns Invisibly returns the result of [shiny::runApp()]
#' @export
#' @examples
#' \dontrun{
#'   run_geocode_explorer()
#' }
run_geocode_explorer <- function(launch.browser = interactive()) {
  stopifnot(
    "launch.browser must be TRUE or FALSE" = is.logical(launch.browser) &&
      length(launch.browser) == 1L &&
      !is.na(launch.browser)
  )
  check_installed("shiny", "to run the geocoding explorer app")
  check_installed("leaflet", "to render the geocoding explorer map")
  check_installed("wk", "to extract geocoded coordinates")
  invisible(
    shiny::runApp(
      geocode_explorer_app(),
      launch.browser = launch.browser
    )
  )
}

geocode_explorer_app <- function() {
  check_installed("shiny", "to run the geocoding explorer app")
  check_installed("leaflet", "to render the geocoding explorer map")
  check_installed("wk", "to extract geocoded coordinates")

  example_address <- voter_addresses()[[1]]

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
          margin: 18px 0 16px 0;
          font-size: 30px;
          font-weight: 700;
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
          font-size: 16px;
        }
        .addr-table {
          width: 100%;
          border-collapse: collapse;
          table-layout: fixed;
          font-size: 13px;
          line-height: 1.4;
        }
        .addr-table th,
        .addr-table td {
          border: 1px solid #e3e6ea;
          padding: 7px 9px;
          vertical-align: top;
        }
        .addr-table th {
          width: 45%;
          background: #f1f3f5;
          color: #40505f;
          font-weight: 600;
          text-align: left;
        }
        .addr-table td {
          font-family: \"Source Code Pro\", \"SFMono-Regular\", Menlo, Consolas, monospace;
          overflow-wrap: anywhere;
        }
        .btn-primary {
          width: 100%;
          background: #2f6f8f;
          border-color: #285f7b;
        }
        .btn-primary:hover,
        .btn-primary:focus {
          background: #285f7b;
          border-color: #214f66;
        }
        .addr-error {
          border-radius: 8px;
          background: #f8d7da;
          color: #7c2732;
          padding: 12px 14px;
          font-weight: 600;
        }
        .form-control:focus {
          border-color: #6c8aa0;
          box-shadow: none;
        }
        .form-control, .control-label {
          font-size: 13px;
        }
        "
      ))
    ),
    shiny::div(class = "addr-title", "Address geocoder"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::textInput(
          "address_input",
          "Address",
          value = example_address
        ),
        shiny::actionButton(
          "geocode_address",
          "Geocode",
          class = "btn-primary"
        ),
        shiny::tags$hr(),
        shiny::uiOutput("geocode_status")
      ),
      shiny::mainPanel(
        width = 8,
        leaflet::leafletOutput("geocode_map", height = 650)
      )
    )
  )

  server <- function(input, output, session) {
    geocoded <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$geocode_address, {
      address_text <- trimws(input$address_input)
      if (!nzchar(address_text)) {
        geocoded(list(error = "Enter an address to geocode."))
        return()
      }

      geocoded(
        tryCatch(
          {
            parsed <- as_addr(address_text)
            gcd <- geocode(parsed, progress = FALSE)
            coords <- wk::wk_coords(gcd$matched_geography)

            list(
              raw_input = address_text,
              parsed = parsed,
              geocode = gcd,
              coords = coords
            )
          },
          error = function(e) {
            list(error = conditionMessage(e))
          }
        )
      )
    })

    output$geocode_map <- leaflet::renderLeaflet({
      x <- geocoded()
      map <- leaflet::leaflet() |>
        leaflet::addTiles()

      if (is.null(x)) {
        return(
          map |>
            leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)
        )
      }

      if (!is.null(x$error)) {
        return(
          map |>
            leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)
        )
      }

      coords <- x$coords
      if (nrow(coords) == 0L) {
        return(
          map |>
            leaflet::setView(lng = -98.5795, lat = 39.8283, zoom = 4)
        )
      }

      map |>
        leaflet::addCircleMarkers(
          data = coords,
          lng = ~x,
          lat = ~y,
          label = ~feature_id,
          radius = 8,
          fillOpacity = 0.8
        ) |>
        leaflet::setView(lng = coords$x[[1]], lat = coords$y[[1]], zoom = 17)
    })

    output$geocode_status <- shiny::renderUI({
      x <- geocoded()

      if (is.null(x)) {
        return(NULL)
      }

      if (!is.null(x$error)) {
        return(shiny::div(class = "addr-error", x$error))
      }

      coords <- x$coords
      longitude <- if (nrow(coords) == 0L) NA_character_ else coords$x[[1]]
      latitude <- if (nrow(coords) == 0L) NA_character_ else coords$y[[1]]

      shiny::tagList(
        geocode_explorer_table_ui(
          "Parsed addr parts",
          geocode_explorer_addr_part_rows(x$parsed)
        ),
        geocode_explorer_table_ui(
          "Geocode result",
          data.frame(
            field = c(
              "parsed addr",
              "matched ZIP",
              "matched street",
              "longitude",
              "latitude",
              "s2_cell"
            ),
            value = c(
              as.character(x$parsed),
              x$geocode$matched_zipcode[[1]],
              as.character(x$geocode$matched_street[1]),
              longitude,
              latitude,
              as.character(x$geocode$s2_cell[1])
            ),
            stringsAsFactors = FALSE
          )
        )
      )
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}

geocode_explorer_addr_part_rows <- function(x) {
  df <- as.data.frame(x[1], stringsAsFactors = FALSE)
  data.frame(
    field = names(df),
    value = unname(as.character(df[1, ])),
    stringsAsFactors = FALSE
  )
}

geocode_explorer_table_ui <- function(title, df) {
  shiny::tags$div(
    class = "addr-panel",
    shiny::h3(title),
    shiny::tags$table(
      class = "addr-table",
      shiny::tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          shiny::tags$tr(
            shiny::tags$th(df$field[[i]]),
            shiny::tags$td(geocode_explorer_format_value(df$value[[i]]))
          )
        })
      )
    )
  )
}

geocode_explorer_format_value <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x[[1]])) {
    return("NA")
  }
  if (!nzchar(x[[1]])) {
    return("<empty>")
  }
  x[[1]]
}
