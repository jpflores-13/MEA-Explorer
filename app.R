# MEA Explorer — Upload stage.
#
# Lets a researcher upload one or more Axion/AxIS CSV exports, then extract
# a single measurement (Mean Firing Rate (Hz) or Number of Bursts) for a
# chosen set of wells across all of them. This is deliberately the Upload
# stage only (see MEA-Explorer-Design-Handbook.md for the full five-stage
# workflow) — Verify/Plate Map/Explore/Export are not built yet.
#
# Per CLAUDE.md: Shiny code calls into R/ functions rather than computing
# inside render*() blocks; reactives hold the *result* of an R/ call.

library(shiny)
library(bslib)

invisible(sapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))

METRIC_CHOICES <- c(
  "Mean Firing Rate (Hz)" = "mean_firing_rate_hz",
  "Number of Bursts" = "number_of_bursts"
)

theme <- bs_theme(
  version = 5,
  bg = "#F8F8F5",
  fg = "#1B1D1C",
  primary = "#0B6E63",
  base_font = font_face(family = "system-ui", src = "local('system-ui')"),
  # font_link() (a <link> the browser fetches itself), not font_google()
  # (which downloads/caches the font at app-startup time via curl/libcurl —
  # unavailable in a webR/shinylive WASM build, and unnecessary work in a
  # normal R session too).
  code_font = font_link(
    family = "IBM Plex Mono",
    href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono&display=swap"
  )
)

ui <- page_sidebar(
  title = "MEA Explorer — Upload",
  theme = theme,
  sidebar = sidebar(
    width = 320,
    fileInput(
      "files", "Axion/AxIS CSV exports",
      multiple = TRUE, accept = ".csv",
      buttonLabel = "Browse", placeholder = "No files selected"
    ),
    radioButtons("metric", "Measurement", choices = METRIC_CHOICES),
    selectizeInput(
      "wells", "Wells", choices = character(0), multiple = TRUE,
      options = list(placeholder = "Upload files to choose wells")
    ),
    downloadButton("download", "Export selection (.csv)", class = "btn-outline-primary w-100 mt-2"),
    radioButtons(
      "plot_format", "Figure format",
      choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
      selected = "png", inline = TRUE
    ),
    downloadButton("download_plot", "Export figure", class = "btn-outline-primary w-100")
  ),
  uiOutput("errors"),
  uiOutput("caption"),
  plotOutput("plot", height = "360px"),
  tableOutput("table")
)

server <- function(input, output, session) {
  parsed <- reactive({
    req(input$files)
    read_axion_mea_report(input$files$datapath, input$files$name)
  })

  observeEvent(parsed(), {
    wells <- sort(unique(parsed()$data$well))
    selected <- intersect(isolate(input$wells), wells)
    if (length(selected) == 0) selected <- wells
    updateSelectizeInput(session, "wells", choices = wells, selected = selected)
  })

  filtered <- reactive({
    data <- parsed()$data
    req(nrow(data) > 0, input$wells, input$metric)
    data |>
      dplyr::filter(well %in% input$wells) |>
      dplyr::select(source_file, well, value = dplyr::all_of(input$metric)) |>
      dplyr::arrange(source_file, well)
  })

  output$errors <- renderUI({
    req(input$files)
    errors <- parsed()$errors
    if (nrow(errors) == 0) return(NULL)

    div(
      class = "alert alert-warning",
      tags$strong(sprintf(
        "%d of %d file%s could not be parsed:",
        nrow(errors), nrow(input$files),
        if (nrow(input$files) == 1) "" else "s"
      )),
      tags$ul(
        lapply(seq_len(nrow(errors)), function(i) {
          tags$li(tags$code(errors$source_file[i]), " — ", errors$message[i])
        })
      )
    )
  })

  output$caption <- renderUI({
    req(input$files)
    if (nrow(parsed()$data) == 0) {
      return(p(class = "text-muted", "No wells were parsed from the uploaded file(s)."))
    }
    req(input$wells)
    n_files <- length(unique(filtered()$source_file))
    p(
      class = "text-muted",
      sprintf(
        "n = %d well%s · %d file%s · %s",
        nrow(filtered()), if (nrow(filtered()) == 1) "" else "s",
        n_files, if (n_files == 1) "" else "s",
        names(METRIC_CHOICES)[METRIC_CHOICES == input$metric]
      )
    )
  })

  output$table <- renderTable(
    {
      req(input$files)
      out <- filtered()
      names(out) <- c("Source file", "Well", names(METRIC_CHOICES)[METRIC_CHOICES == input$metric])
      out
    },
    striped = TRUE, hover = TRUE, digits = 6
  )

  output$download <- downloadHandler(
    filename = function() "mea_explorer_selection.csv",
    content = function(file) {
      out <- filtered()
      names(out) <- c("source_file", "well", input$metric)
      readr::write_csv(out, file)
    }
  )

  # plot_well_values() (R/plotting.R) never needs metadata columns, so it
  # works directly on what Upload has: whichever wells/metric are selected.
  plot_obj <- reactive({
    req(nrow(filtered()) > 0)
    metric_label <- names(METRIC_CHOICES)[METRIC_CHOICES == input$metric]

    p <- plot_well_values(filtered(), "value") +
      ggplot2::labs(x = "Well", y = metric_label) +
      ggplot2::theme_minimal(base_size = 13)

    # Distinguish same-named wells from different files rather than
    # conflating two different wells' worth of data at one x position.
    if (length(unique(filtered()$source_file)) > 1) {
      p <- p + ggplot2::facet_wrap(~source_file)
    }
    p
  })

  output$plot <- renderPlot(plot_obj())

  output$download_plot <- downloadHandler(
    filename = function() sprintf("mea_explorer_figure.%s", input$plot_format),
    content = function(file) {
      # ggsave()'s svg device needs svglite; named explicitly (rather than
      # left to ggsave's internal requireNamespace) so it's both a clear
      # error and a dependency renv's static scanner can find and snapshot.
      if (input$plot_format == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
        stop(mea_condition(
          "MEA Explorer needs the 'svglite' package installed to export SVG figures. Install it with install.packages(\"svglite\"), or choose PNG or PDF instead.",
          "missing_svg_dependency"
        ))
      }

      ggplot2::ggsave(
        file, plot = plot_obj(),
        width = 7, height = 5, units = "in", dpi = 300,
        device = input$plot_format
      )
    }
  )
}

shinyApp(ui, server)
