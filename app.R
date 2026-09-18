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

# First color is #FF54A9 — not a guess: it's the exact "Well Coloring"
# hex Axion/AxIS itself assigns and Prism inherits (see any export's
# header), so this matches what clients already see in Prism. The next
# few stay clearly distinct for a handful of real conditions.
CONDITION_PALETTE <- c("#FF54A9", "#111111", "#0B6E63", "#E69F00", "#3C79B5", "#7B4EA3")

# Real filenames are inconsistent enough (typos, stray leading
# underscores/digits, reordered tokens) that infer_condition_label() can
# turn what's really one condition into several near-duplicate labels —
# so the number of "conditions" a researcher ends up with isn't bounded
# by CONDITION_PALETTE's length. Never let running out of fixed colors
# error out the plot: extend with evenly-spaced hues past the fixed set.
condition_colors <- function(n) {
  if (n <= length(CONDITION_PALETTE)) return(CONDITION_PALETTE[seq_len(n)])
  n_extra <- n - length(CONDITION_PALETTE)
  extra <- grDevices::hcl(h = seq(15, 375, length.out = n_extra + 1)[seq_len(n_extra)], c = 100, l = 55)
  c(CONDITION_PALETTE, extra)
}

#' A light- or dark-background theme layer for exported figures
#'
#' Applied on top of each plot's own theme_minimal()/bold-text styling,
#' so it only needs to touch background fills and (for dark) flip text/
#' gridline/axis-line color to stay legible — face="bold" etc. set
#' earlier is inherited through, not reset, since this never sets those
#' properties directly.
#' @keywords internal
plot_background_theme <- function(dark) {
  if (!dark) {
    return(ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA)
    ))
  }

  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "black", color = NA),
    panel.background = ggplot2::element_rect(fill = "black", color = NA),
    panel.grid = ggplot2::element_line(color = "grey25"),
    legend.background = ggplot2::element_rect(fill = "black", color = NA),
    legend.key = ggplot2::element_rect(fill = "black", color = NA),
    text = ggplot2::element_text(color = "white"),
    axis.line = ggplot2::element_line(color = "white"),
    axis.ticks = ggplot2::element_line(color = "white")
  )
}

#' Force point/line/errorbar geoms to white when they have no color
#' aesthetic mapped, so they don't render invisibly-black on a dark
#' background
#'
#' theme() can't fix this — a geom with no `color` in its (or the plot's)
#' mapping falls back to ggplot2's own default (black), which a dark
#' background swallows completely. Layers that DO map color (e.g. by
#' condition) are left untouched, since their own color scale already
#' handles visibility.
#' @keywords internal
force_geom_color_if_dark <- function(p, dark) {
  if (!dark) return(p)
  has_global_color <- !is.null(p$mapping$colour)

  for (i in seq_along(p$layers)) {
    layer <- p$layers[[i]]
    if (has_global_color || !is.null(layer$mapping$colour)) next
    if (inherits(layer$geom, c("GeomPoint", "GeomLine", "GeomErrorbar"))) {
      p$layers[[i]]$aes_params$colour <- "white"
    }
  }
  p
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
    p(
      class = "text-muted small mb-1",
      "Browsing again adds more files rather than replacing your selection."
    ),
    uiOutput("file_list"),
    radioButtons("metric", "Measurement", choices = METRIC_CHOICES),
    div(
      class = "d-flex justify-content-between align-items-center",
      tags$label("Wells", class = "control-label", `for` = "wells"),
      div(
        actionLink("wells_select_all", "Select all", class = "small me-2"),
        actionLink("wells_select_none", "Clear", class = "small")
      )
    ),
    checkboxGroupInput("wells", NULL, choices = character(0), inline = TRUE)
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
          radioButtons(
            "plot_background", "Background",
            choices = c("White" = "light", "Black" = "dark"),
            selected = "light", inline = TRUE
          ),
          downloadButton("download_plot", "Export figure", class = "btn-outline-primary w-100")
        )
      )
    ),
    nav_panel(
      "Time course",
      p(
        class = "text-muted mt-2",
        "Condition and timepoint (days in culture) are pre-filled from each file's Recording Name (e.g. \"KOLF EGC BUMP d40\" → condition \"KOLF EGC BUMP\", day 40) — check them and correct if wrong. Notes are free text for anything worth remembering about that run."
      ),
      uiOutput("metadata_inputs"),
      uiOutput("condition_filter"),
      uiOutput("timecourse_caption"),
      plotOutput("timecourse_plot", height = "360px"),
      layout_columns(
        downloadButton("download_prism", "Export replicates (Prism, .csv)", class = "btn-outline-primary w-100"),
        downloadButton("download_prism_summary", "Export Mean/SD/N (Prism, .csv)", class = "btn-outline-primary w-100"),
        div(
          radioButtons(
            "timecourse_format", "Figure format",
            choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
            selected = "png", inline = TRUE
          ),
          radioButtons(
            "timecourse_background", "Background",
            choices = c("White" = "light", "Black" = "dark"),
            selected = "light", inline = TRUE
          ),
          downloadButton("download_timecourse_plot", "Export figure", class = "btn-outline-primary w-100")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # A native <input type=file> replaces its whole selection every time the
  # researcher browses again, so the working file set is tracked here
  # instead: each new browse action is merged in (a re-picked name replaces
  # its old entry), and individual files can be removed without redoing
  # the OS file picker for the rest.
  uploaded_files <- reactiveVal(tibble::tibble(
    name = character(0), datapath = character(0)
  ))

  observeEvent(input$files, {
    new_files <- tibble::as_tibble(input$files[, c("name", "datapath")])
    kept <- dplyr::filter(uploaded_files(), !name %in% new_files$name)
    uploaded_files(dplyr::bind_rows(kept, new_files))
  })

  observeEvent(input$remove_file, {
    uploaded_files(dplyr::filter(uploaded_files(), name != input$remove_file))
  })

  output$file_list <- renderUI({
    files <- uploaded_files()
    if (nrow(files) == 0) return(NULL)

    tagList(
      p(class = "small text-muted mb-1", sprintf(
        "%d file%s included:", nrow(files), if (nrow(files) == 1) "" else "s"
      )),
      tags$ul(
        class = "list-unstyled small mb-2",
        lapply(sort(files$name), function(name) {
          tags$li(
            class = "d-flex justify-content-between align-items-center",
            tags$span(class = "text-truncate", title = name, name),
            tags$a(
              href = "#", class = "text-danger ms-2 text-decoration-none",
              title = "Remove this file", `data-name` = name,
              onclick = "Shiny.setInputValue('remove_file', this.getAttribute('data-name'), {priority: 'event'}); return false;",
              "✕"
            )
          )
        })
      )
    )
  })

  parsed <- reactive({
    req(nrow(uploaded_files()) > 0)
    read_axion_mea_report(uploaded_files()$datapath, uploaded_files()$name)
  })

  observeEvent(parsed(), {
    wells <- sort(unique(parsed()$data$well))
    selected <- intersect(isolate(input$wells), wells)
    if (length(selected) == 0) selected <- wells
    updateCheckboxGroupInput(session, "wells", choices = wells, selected = selected, inline = TRUE)
  })

  observeEvent(input$wells_select_all, {
    wells <- sort(unique(parsed()$data$well))
    updateCheckboxGroupInput(session, "wells", choices = wells, selected = wells, inline = TRUE)
  })

  observeEvent(input$wells_select_none, {
    wells <- sort(unique(parsed()$data$well))
    updateCheckboxGroupInput(session, "wells", choices = wells, selected = character(0), inline = TRUE)
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
    req(nrow(uploaded_files()) > 0)
    errors <- parsed()$errors
    if (nrow(errors) == 0) return(NULL)

    div(
      class = "alert alert-warning",
      tags$strong(sprintf(
        "%d of %d file%s could not be parsed:",
        nrow(errors), nrow(uploaded_files()),
        if (nrow(uploaded_files()) == 1) "" else "s"
      )),
      tags$ul(
        lapply(seq_len(nrow(errors)), function(i) {
          tags$li(tags$code(errors$source_file[i]), " — ", errors$message[i])
        })
      )
    )
  })

  output$caption <- renderUI({
    req(nrow(uploaded_files()) > 0)
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
      req(nrow(uploaded_files()) > 0)
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
      ggplot2::theme_minimal(base_size = 13) +
      plot_background_theme(dark = identical(input$plot_background, "dark"))

    # Distinguish same-named wells from different files rather than
    # conflating two different wells' worth of data at one x position.
    if (length(unique(filtered()$source_file)) > 1) {
      p <- p + ggplot2::facet_wrap(~source_file)
    }
    force_geom_color_if_dark(p, dark = identical(input$plot_background, "dark"))
  })

  output$plot <- renderPlot(
    plot_obj(),
    bg = "transparent"
  )

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
      bg <- if (identical(input$plot_background, "dark")) "black" else "white"
      ggplot2::ggsave(
        file, plot = plot_obj(),
        width = 7, height = 5, units = "in", dpi = 300,
        device = input$plot_format, bg = bg
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
    files_df <- uploaded_files()[uploaded_files()$name %in% ok_files, , drop = FALSE]
    files_df <- files_df[order(files_df$name), ]

    tagList(lapply(seq_len(nrow(files_df)), function(i) {
      f <- files_df$name[[i]]
      key <- file_input_key(f)
      recording_name <- read_recording_name(files_df$datapath[[i]])
      inferred_day <- infer_timepoint_days(recording_name)
      inferred_condition <- infer_condition_label(recording_name)
      if (is.na(inferred_condition)) inferred_condition <- ""
      layout_columns(
        col_widths = c(3, 2, 2, 5),
        div(class = "text-truncate pt-2", title = f, f),
        textInput(paste0("treatment_", key), NULL, value = inferred_condition, placeholder = "Condition (optional)"),
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

  # Distinct conditions currently present, for the "which data" filter
  # below. Rows with a blank/unassigned condition are always kept in
  # timecourse_data() regardless of this filter, since there's no
  # checkbox representing "unlabeled" — never silently drop them.
  available_conditions <- reactive({
    conditions <- data_with_metadata()$treatment
    sort(unique(conditions[!is.na(conditions) & nzchar(conditions)]))
  })

  output$condition_filter <- renderUI({
    conditions <- available_conditions()
    if (length(conditions) < 2) return(NULL)

    selected <- intersect(isolate(input$conditions), conditions)
    if (length(selected) == 0) selected <- conditions

    tagList(
      div(
        class = "d-flex justify-content-between align-items-center mt-1",
        tags$label("Include conditions", class = "control-label mb-0", `for` = "conditions"),
        div(
          actionLink("normalize_conditions", "Normalize conditions", class = "small me-2"),
          actionLink("conditions_select_all", "Select all", class = "small me-2"),
          actionLink("conditions_select_none", "Clear", class = "small")
        )
      ),
      checkboxGroupInput("conditions", NULL, choices = conditions, selected = selected, inline = TRUE)
    )
  })

  observeEvent(input$conditions_select_all, {
    conditions <- available_conditions()
    updateCheckboxGroupInput(session, "conditions", choices = conditions, selected = conditions, inline = TRUE)
  })

  observeEvent(input$conditions_select_none, {
    conditions <- available_conditions()
    updateCheckboxGroupInput(session, "conditions", choices = conditions, selected = character(0), inline = TRUE)
  })

  # One condition label per file (not per well/row), for frequency-aware
  # grouping — the most common spelling in a group becomes the suggested
  # canonical one. See group_condition_labels()/normalize_condition_key()
  # (R/upload.R) for what does and doesn't get grouped automatically.
  condition_labels_per_file <- reactive({
    dplyr::distinct(data_with_metadata(), source_file, treatment)$treatment
  })

  condition_groups <- reactive({
    group_condition_labels(condition_labels_per_file())
  })

  # Snapshot the groups shown in the modal so a later Apply click acts on
  # exactly what the researcher reviewed, even if the underlying data
  # changes in between.
  pending_normalize_groups <- reactiveVal(list())

  observeEvent(input$normalize_conditions, {
    groups <- condition_groups()
    pending_normalize_groups(groups)

    showModal(modalDialog(
      title = "Normalize condition labels",
      if (length(groups) == 0) {
        p("No near-duplicate condition labels found — nothing to normalize.")
      } else {
        tagList(
          p(
            class = "text-muted",
            "These look like the same condition once case, spacing, punctuation, and word order are ignored. Review the canonical spelling below (edit if needed), then apply."
          ),
          lapply(seq_along(groups), function(i) {
            g <- groups[[i]]
            div(
              class = "mb-3 pb-3 border-bottom",
              checkboxInput(
                paste0("normalize_include_", i),
                label = paste(g$labels, collapse = "  •  "),
                value = TRUE
              ),
              textInput(paste0("normalize_canonical_", i), "Merge into:", value = g$suggested)
            )
          })
        )
      },
      footer = tagList(
        modalButton("Cancel"),
        if (length(groups) > 0) actionButton("normalize_apply", "Apply", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$normalize_apply, {
    groups <- pending_normalize_groups()
    file_treatment <- dplyr::distinct(data_with_metadata(), source_file, treatment)

    for (i in seq_along(groups)) {
      if (!isTRUE(input[[paste0("normalize_include_", i)]])) next
      canonical <- input[[paste0("normalize_canonical_", i)]]
      if (is.null(canonical) || !nzchar(canonical)) next

      affected_files <- file_treatment$source_file[file_treatment$treatment %in% groups[[i]]$labels]
      for (f in affected_files) {
        updateTextInput(session, paste0("treatment_", file_input_key(f)), value = canonical)
      }
    }

    removeModal()
  })

  timecourse_data <- reactive({
    data <- data_with_metadata()
    req(input$wells)
    data <- dplyr::filter(data, well %in% input$wells)

    conditions <- available_conditions()
    if (length(conditions) >= 2) {
      req(input$conditions)
      data <- dplyr::filter(data, is.na(treatment) | treatment %in% input$conditions)
    }
    data
  })

  has_timepoint <- reactive({
    req(nrow(uploaded_files()) > 0)
    any(!is.na(timecourse_data()$timepoint))
  })

  has_group <- reactive({
    any(!is.na(timecourse_data()$treatment) & nzchar(timecourse_data()$treatment))
  })

  output$timecourse_caption <- renderUI({
    req(nrow(uploaded_files()) > 0)
    if (!has_timepoint()) {
      return(p(class = "text-muted", "Assign a timepoint (days) to at least one file above to see the time course."))
    }
    data <- timecourse_data()
    n_wells <- length(unique(paste(data$source_file, data$well)))
    tagList(
      p(
        class = "text-muted mb-0",
        sprintf(
          "n = %d well-recording%s · %s",
          n_wells, if (n_wells == 1) "" else "s",
          names(METRIC_CHOICES)[METRIC_CHOICES == input$metric]
        )
      ),
      p(
        class = "text-muted small",
        "Faint dots = individual wells · bold dot/line = mean ± SEM across included wells."
      )
    )
  })

  timecourse_plot_obj <- reactive({
    req(has_timepoint())
    metric_label <- names(METRIC_CHOICES)[METRIC_CHOICES == input$metric]

    p <- plot_time_course_summary(
      timecourse_data(), input$metric,
      group = if (has_group()) "treatment" else NULL
    ) +
      ggplot2::labs(
        title = paste("Avg", metric_label), x = "Days in culture",
        y = metric_label, color = "Condition"
      ) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(face = "bold"),
        legend.title = ggplot2::element_text(face = "bold")
      ) +
      plot_background_theme(dark = identical(input$timecourse_background, "dark"))

    if (has_group()) {
      n_conditions <- length(unique(timecourse_data()$treatment[!is.na(timecourse_data()$treatment)]))
      p <- p + ggplot2::scale_color_manual(values = condition_colors(n_conditions))
    }
    force_geom_color_if_dark(p, dark = identical(input$timecourse_background, "dark"))
  })

  output$timecourse_plot <- renderPlot(
    {
      req(has_timepoint())
      timecourse_plot_obj()
    },
    bg = "transparent"
  )

  output$download_timecourse_plot <- downloadHandler(
    filename = function() sprintf("mea_explorer_timecourse.%s", input$timecourse_format),
    content = function(file) {
      require_svglite_if_needed(input$timecourse_format)
      bg <- if (identical(input$timecourse_background, "dark")) "black" else "white"
      ggplot2::ggsave(
        file, plot = timecourse_plot_obj(),
        width = 7, height = 5, units = "in", dpi = 300,
        device = input$timecourse_format, bg = bg
      )
    }
  )

  output$download_prism <- downloadHandler(
    filename = function() "mea_explorer_timecourse_prism_replicates.csv",
    content = function(file) {
      data <- timecourse_data() |>
        dplyr::rename(value = dplyr::all_of(input$metric))
      out <- pivot_wide_for_prism(data, "value", group = if (has_group()) "treatment" else NULL)
      readr::write_csv(out, file)
    }
  )

  # For Prism's "Enter and plot error values calculated elsewhere ->
  # Mean, SD, N" table format, which wants precomputed statistics per
  # timepoint rather than one column per replicate.
  output$download_prism_summary <- downloadHandler(
    filename = function() "mea_explorer_timecourse_prism_summary.csv",
    content = function(file) {
      data <- timecourse_data() |>
        dplyr::rename(value = dplyr::all_of(input$metric))
      out <- pivot_summary_for_prism(data, "value", group = if (has_group()) "treatment" else NULL)
      readr::write_csv(out, file)
    }
  )
}

shinyApp(ui, server)
