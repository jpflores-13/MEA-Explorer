# MEA Explorer — Upload stage.
#
# Lets a researcher upload one or more Axion/AxIS CSV exports, then:
#   - "Wells" tab: extract a single measurement for a chosen set of wells
#     across all uploaded files.
#   - "Time course" tab: assign a timepoint (days) and an optional
#     group/condition to each uploaded file, then view a mean +/- SEM
#     curve over time (individual wells shown underneath), and export it
#     as a figure or as a wide, Prism-ready table.
# This is deliberately the Upload stage only (see
# MEA-Explorer-Design-Handbook.md for the full five-stage workflow) —
# a full per-well Plate Map is not built yet; see CLAUDE.md's "Longitudinal
# data model" section for what that means for the Time course tab.
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

# A downloadHandler needs a stable, syntactically-safe key per uploaded
# file to name its dynamic treatment_*/timepoint_* inputs.
file_input_key <- function(source_file) {
  paste0("f", vapply(source_file, function(x) {
    paste(as.integer(charToRaw(x)), collapse = "_")
  }, character(1)))
}

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
    )
  ),
  uiOutput("errors"),
  navset_tab(
    nav_panel(
      "Wells",
      uiOutput("caption"),
      plotOutput("plot", height = "360px"),
      tableOutput("table"),
      layout_columns(
        downloadButton("download", "Export selection (.csv)", class = "btn-outline-primary w-100"),
        div(
          radioButtons(
            "plot_format", "Figure format",
            choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
            selected = "png", inline = TRUE
          ),
          downloadButton("download_plot", "Export figure", class = "btn-outline-primary w-100")
        )
      )
    ),
    nav_panel(
      "Time course",
      p(
        class = "text-muted mt-2",
        "Timepoint (days in culture) is pre-filled from each file's Recording Name when MEA Explorer recognizes a day marker (e.g. \"d40\") — check it and correct it if it's wrong. Notes are free text for anything worth remembering about that run (e.g. \"media change\", \"possible contamination\")."
      ),
      uiOutput("metadata_inputs"),
      uiOutput("timecourse_caption"),
      plotOutput("timecourse_plot", height = "360px"),
      layout_columns(
        downloadButton("download_prism", "Export data (Prism format, .csv)", class = "btn-outline-primary w-100"),
        div(
          radioButtons(
            "timecourse_format", "Figure format",
            choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
            selected = "png", inline = TRUE
          ),
          downloadButton("download_timecourse_plot", "Export figure", class = "btn-outline-primary w-100")
        )
      )
    )
  )
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

  # ggsave()'s svg device needs svglite; checked explicitly (rather than
  # left to ggsave's internal requireNamespace) so it's both a clear error
  # and a dependency renv's static scanner can find and snapshot.
  require_svglite_if_needed <- function(format) {
    if (format == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
      stop(mea_condition(
        "MEA Explorer needs the 'svglite' package installed to export SVG figures. Install it with install.packages(\"svglite\"), or choose PNG or PDF instead.",
        "missing_svg_dependency"
      ))
    }
  }

  output$download_plot <- downloadHandler(
    filename = function() sprintf("mea_explorer_figure.%s", input$plot_format),
    content = function(file) {
      require_svglite_if_needed(input$plot_format)
      ggplot2::ggsave(
        file, plot = plot_obj(),
        width = 7, height = 5, units = "in", dpi = 300,
        device = input$plot_format
      )
    }
  )

  # --- Time course tab -------------------------------------------------
  #
  # Longitudinal design (see CLAUDE.md): one uploaded file = one recording
  # day for the whole plate, so timepoint is assigned per file. Treatment
  # (e.g. "ASD" vs "Control") is assigned per file too for now — correct
  # when each condition is recorded as its own set of files/plates. A
  # single file mixing conditions across different wells needs a real
  # per-well Plate Map, not built yet.

  output$metadata_inputs <- renderUI({
    req(nrow(parsed()$data) > 0)

    # Match each successfully-parsed file back to its upload datapath, so
    # the timepoint can be pre-filled from that file's own Recording Name
    # header (never from the upload filename) — see
    # infer_timepoint_days()/CLAUDE.md's "Longitudinal data model".
    ok_files <- unique(parsed()$data$source_file)
    files_df <- input$files[input$files$name %in% ok_files, , drop = FALSE]
    files_df <- files_df[order(files_df$name), ]

    tagList(lapply(seq_len(nrow(files_df)), function(i) {
      f <- files_df$name[[i]]
      key <- file_input_key(f)
      inferred_day <- infer_timepoint_days(read_recording_name(files_df$datapath[[i]]))
      layout_columns(
        col_widths = c(3, 2, 2, 5),
        div(class = "text-truncate pt-2", title = f, f),
        textInput(paste0("treatment_", key), NULL, placeholder = "Condition (optional)"),
        numericInput(paste0("timepoint_", key), NULL, value = inferred_day),
        textInput(paste0("notes_", key), NULL, placeholder = "Notes (optional)")
      )
    }))
  })

  data_with_metadata <- reactive({
    data <- parsed()$data
    req(nrow(data) > 0)
    files <- sort(unique(data$source_file))

    scaffold <- initialize_metadata(data)
    for (f in files) {
      key <- file_input_key(f)
      treatment_val <- input[[paste0("treatment_", key)]]
      timepoint_val <- input[[paste0("timepoint_", key)]]
      notes_val <- input[[paste0("notes_", key)]]
      rows <- scaffold$source_file == f
      scaffold$treatment[rows] <- if (!is.null(treatment_val) && nzchar(treatment_val)) treatment_val else NA_character_
      scaffold$timepoint[rows] <- if (!is.null(timepoint_val) && !is.na(timepoint_val)) as.character(timepoint_val) else NA_character_
      scaffold$notes[rows] <- if (!is.null(notes_val) && nzchar(notes_val)) notes_val else NA_character_
    }

    apply_metadata(data, scaffold)
  })

  timecourse_data <- reactive({
    data <- data_with_metadata()
    req(input$wells)
    dplyr::filter(data, well %in% input$wells)
  })

  has_timepoint <- reactive({
    req(input$files)
    any(!is.na(timecourse_data()$timepoint))
  })

  has_group <- reactive({
    any(!is.na(timecourse_data()$treatment) & nzchar(timecourse_data()$treatment))
  })

  output$timecourse_caption <- renderUI({
    req(input$files)
    if (!has_timepoint()) {
      return(p(class = "text-muted", "Assign a timepoint (days) to at least one file above to see the time course."))
    }
    data <- timecourse_data()
    n_wells <- length(unique(paste(data$source_file, data$well)))
    p(
      class = "text-muted",
      sprintf(
        "n = %d well-recording%s · %s",
        n_wells, if (n_wells == 1) "" else "s",
        names(METRIC_CHOICES)[METRIC_CHOICES == input$metric]
      )
    )
  })

  timecourse_plot_obj <- reactive({
    req(has_timepoint())
    metric_label <- names(METRIC_CHOICES)[METRIC_CHOICES == input$metric]

    plot_time_course_summary(
      timecourse_data(), input$metric,
      group = if (has_group()) "treatment" else NULL
    ) +
      ggplot2::labs(x = "Days in culture", y = metric_label, color = "Condition") +
      ggplot2::theme_minimal(base_size = 13)
  })

  output$timecourse_plot <- renderPlot({
    req(has_timepoint())
    timecourse_plot_obj()
  })

  output$download_timecourse_plot <- downloadHandler(
    filename = function() sprintf("mea_explorer_timecourse.%s", input$timecourse_format),
    content = function(file) {
      require_svglite_if_needed(input$timecourse_format)
      ggplot2::ggsave(
        file, plot = timecourse_plot_obj(),
        width = 7, height = 5, units = "in", dpi = 300,
        device = input$timecourse_format
      )
    }
  )

  output$download_prism <- downloadHandler(
    filename = function() "mea_explorer_timecourse_prism.csv",
    content = function(file) {
      data <- timecourse_data() |>
        dplyr::rename(value = dplyr::all_of(input$metric))
      out <- pivot_wide_for_prism(data, "value", group = if (has_group()) "treatment" else NULL)
      readr::write_csv(out, file)
    }
  )
}

shinyApp(ui, server)
