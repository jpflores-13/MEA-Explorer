# Plotting functions for tidy MEA data (see DATA_SPEC.md for the schema).
#
# Per CLAUDE.md, MEA Explorer never defaults to a bar/summary-only plot when
# the underlying observations can reasonably be shown. Every function below
# except plot_plate_heatmap() (inherently one value per well) plots
# individual well-level observations rather than only a mean/SEM summary.
# Metadata columns (treatment, timepoint, biological_replicate) may be
# entirely absent or present-but-NA; functions that don't strictly need a
# given metadata column degrade gracefully rather than erroring.

#' Plot per-well observations for a metric
#'
#' One point per row. Wells are ordered as they appear in `data` (not
#' re-sorted alphabetically). Points are colored by `treatment` when that
#' column is present and carries at least one non-`NA` value; otherwise no
#' color mapping is applied.
#'
#' @param data Tidy MEA data with `well` and `metric` columns.
#' @param metric Name of the measurement column to plot, e.g.
#'   "mean_firing_rate_hz".
#' @return A ggplot object.
#' @export
plot_well_values <- function(data, metric) {
  require_columns(data, c("well", metric), context = "data")

  plot_data <- data
  plot_data$well <- factor(plot_data$well, levels = unique(plot_data$well))

  has_treatment <- "treatment" %in% colnames(plot_data) &&
    any(!is.na(plot_data$treatment))

  mapping <- if (has_treatment) {
    ggplot2::aes(x = well, y = .data[[metric]], color = treatment)
  } else {
    ggplot2::aes(x = well, y = .data[[metric]])
  }

  ggplot2::ggplot(plot_data, mapping) +
    ggplot2::geom_point()
}

#' Plot the distribution of a metric across an experimental group
#'
#' Shows both the group-level distribution (boxplot) and the individual
#' well-level observations (jittered points) layered on top, per the
#' project rule against bar/summary-only plots.
#'
#' @param data Tidy MEA data.
#' @param metric Name of the measurement column to plot.
#' @param group Name of the grouping column, e.g. "treatment".
#' @return A ggplot object.
#' @export
plot_group_distribution <- function(data, metric, group) {
  require_columns(data, c(group, metric), context = "data")

  plot_data <- data
  plot_data[[group]] <- factor(plot_data[[group]])

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[group]], y = .data[[metric]])) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, height = 0)
}

#' Plot a metric across group and biological replicate, without averaging
#'
#' Every observation is plotted individually, colored by `replicate_col`
#' and jittered/dodged within each `group` so replicates stay
#' distinguishable. Replicates are never aggregated.
#'
#' @param data Tidy MEA data.
#' @param metric Name of the measurement column to plot.
#' @param group Name of the grouping column, e.g. "treatment".
#' @param replicate_col Name of the biological replicate column.
#' @return A ggplot object.
#' @export
plot_replicate_structure <- function(data, metric, group, replicate_col = "biological_replicate") {
  require_columns(data, c(group, replicate_col, metric), context = "data")

  plot_data <- data
  plot_data[[group]] <- factor(plot_data[[group]])
  plot_data[[replicate_col]] <- factor(plot_data[[replicate_col]])

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[group]], y = .data[[metric]], color = .data[[replicate_col]])
  ) +
    ggplot2::geom_point(position = ggplot2::position_jitterdodge())
}

#' Plot a metric over time, one trajectory per well
#'
#' Individual observations and per-well trajectories are plotted (never a
#' single averaged line per group), so within-group variability across
#' timepoints stays visible. Lines are grouped by the finest available
#' experimental unit: (well, source_file, replicate_col) when
#' `replicate_col` is given, otherwise (well, source_file).
#'
#' @param data Tidy MEA data with a `timepoint` column that has at least
#'   one assigned (non-`NA`) value.
#' @param metric Name of the measurement column to plot.
#' @param group Optional name of a column to color trajectories by, e.g.
#'   "treatment".
#' @param replicate_col Optional name of a biological replicate column;
#'   when given it refines the line grouping and is also mapped to
#'   linetype.
#' @return A ggplot object.
#' @export
plot_time_course <- function(data, metric, group = NULL, replicate_col = NULL) {
  required <- c("well", "source_file", metric)
  if (!is.null(group)) required <- c(required, group)
  if (!is.null(replicate_col)) required <- c(required, replicate_col)
  require_columns(data, required, context = "data")

  if (!"timepoint" %in% colnames(data) || !any(!is.na(data$timepoint))) {
    stop(mea_condition(
      "MEA Explorer could not plot a time course because no timepoint metadata has been assigned yet.",
      "missing_timepoint"
    ))
  }

  plot_data <- data
  plot_data$.mea_unit <- if (!is.null(replicate_col)) {
    interaction(plot_data$well, plot_data$source_file, plot_data[[replicate_col]])
  } else {
    interaction(plot_data$well, plot_data$source_file)
  }

  mapping <- ggplot2::aes(
    x = .data[["timepoint"]],
    y = .data[[metric]],
    group = .mea_unit,
    color = if (!is.null(group)) .data[[group]] else NULL,
    linetype = if (!is.null(replicate_col)) .data[[replicate_col]] else NULL
  )

  ggplot2::ggplot(plot_data, mapping) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
}

#' Plot a mean +/- SEM time course, with individual wells shown underneath
#'
#' Summarizes `metric` by (`timepoint`, `group`) with [summarize_mea()] and
#' draws one line per group connecting the per-timepoint mean, with SEM
#' error bars — the standard longitudinal-MEA figure (e.g. for GraphPad
#' Prism). Per the project rule against summary-only plots, every
#' well-level observation the error bar summarizes is still plotted
#' underneath, as small semi-transparent points, so the spread stays
#' checkable against the raw data.
#'
#' @param data Tidy MEA data with a `timepoint` column that has at least
#'   one assigned (non-`NA`) value, and (if `group` is given) that column.
#' @param metric Name of the measurement column to plot.
#' @param group Optional name of a column to group/color by, e.g.
#'   "treatment" (e.g. "ASD" vs "Control"). When `NULL`, all rows are
#'   treated as a single group/series.
#' @return A ggplot object.
#' @export
plot_time_course_summary <- function(data, metric, group = NULL) {
  required <- c("timepoint", metric)
  if (!is.null(group)) required <- c(required, group)
  require_columns(data, required, context = "data")

  plot_data <- data
  plot_data$timepoint <- suppressWarnings(as.numeric(plot_data$timepoint))

  newly_na <- is.na(plot_data$timepoint) & !is.na(data$timepoint)
  if (any(newly_na)) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer could not interpret timepoint value(s) %s as numbers (days).",
        paste(sprintf("'%s'", unique(data$timepoint[newly_na])), collapse = ", ")
      ),
      "invalid_timepoint"
    ))
  }

  if (!any(!is.na(plot_data$timepoint))) {
    stop(mea_condition(
      "MEA Explorer could not plot a time course because no timepoint metadata has been assigned yet.",
      "missing_timepoint"
    ))
  }

  plot_data <- plot_data[!is.na(plot_data$timepoint), , drop = FALSE]

  group_cols <- c("timepoint", if (!is.null(group)) group)
  summary_data <- summarize_mea(plot_data, metric, group_cols = group_cols)
  summary_data <- summary_data[!is.na(summary_data$mean), , drop = FALSE]

  point_aes <- if (!is.null(group)) {
    ggplot2::aes(x = timepoint, y = .data[[metric]], color = .data[[group]])
  } else {
    ggplot2::aes(x = timepoint, y = .data[[metric]])
  }
  line_aes <- if (!is.null(group)) {
    ggplot2::aes(x = timepoint, y = mean, color = .data[[group]], group = .data[[group]])
  } else {
    ggplot2::aes(x = timepoint, y = mean, group = 1)
  }
  errorbar_aes <- if (!is.null(group)) {
    ggplot2::aes(x = timepoint, ymin = mean - sem, ymax = mean + sem, color = .data[[group]])
  } else {
    ggplot2::aes(x = timepoint, ymin = mean - sem, ymax = mean + sem)
  }
  mean_point_aes <- if (!is.null(group)) {
    ggplot2::aes(x = timepoint, y = mean, color = .data[[group]])
  } else {
    ggplot2::aes(x = timepoint, y = mean)
  }

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = plot_data, mapping = point_aes,
      alpha = 0.25, size = 1.5,
      position = ggplot2::position_jitter(width = 0.3, height = 0)
    ) +
    ggplot2::geom_errorbar(data = summary_data, mapping = errorbar_aes, width = 0) +
    ggplot2::geom_line(data = summary_data, mapping = line_aes) +
    ggplot2::geom_point(data = summary_data, mapping = mean_point_aes, size = 2.5)
}

#' Plot a per-well heatmap of a metric across the plate layout
#'
#' Requires exactly one row per well; MEA Explorer never silently averages
#' or otherwise aggregates rows to make this true, since that decision
#' belongs to the caller (e.g. filter to one file/timepoint first). This is
#' the one plot in this file that shows a single value per well rather than
#' individual observations, since a plate heatmap is inherently one tile
#' per well.
#'
#' @param data Tidy MEA data with one row per well.
#' @param metric Name of the measurement column to plot.
#' @return A ggplot object.
#' @export
plot_plate_heatmap <- function(data, metric) {
  require_columns(data, c("well", metric), context = "data")

  parsed <- stringr::str_match(data$well, "^([A-Za-z]+)([0-9]+)$")
  bad <- which(is.na(parsed[, 1]))
  if (length(bad) > 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer could not parse well identifier '%s' as a plate position (expected a format like 'A1').",
        data$well[[bad[[1]]]]
      ),
      "invalid_well_id"
    ))
  }

  duplicated_wells <- data |>
    dplyr::count(well) |>
    dplyr::filter(n > 1)

  if (nrow(duplicated_wells) > 0) {
    stop(mea_condition(
      "MEA Explorer cannot draw a plate heatmap because some wells have more than one row (e.g. from multiple files or timepoints). Filter the data to one row per well first.",
      "ambiguous_heatmap_well"
    ))
  }

  plot_data <- data
  plot_data$row_letter <- factor(parsed[, 2], levels = rev(sort(unique(parsed[, 2]))))
  col_number <- as.integer(parsed[, 3])
  plot_data$col_number <- factor(col_number, levels = sort(unique(col_number)))

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = col_number, y = row_letter, fill = .data[[metric]])
  ) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "Column", y = "Row")

  if (requireNamespace("viridisLite", quietly = TRUE)) {
    p + ggplot2::scale_fill_viridis_c()
  } else {
    p + ggplot2::scale_fill_gradient()
  }
}
