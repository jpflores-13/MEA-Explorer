# Reshaping tidy MEA data for external tools that expect a wide layout
# (e.g. GraphPad Prism's grouped XY tables: one row per X value, either one
# column per replicate or a precomputed Mean/SD/N per group) rather than
# the long/tidy shape the rest of R/ works in.

#' Coerce `timepoint` to numeric and drop unassigned rows
#'
#' Shared by both `pivot_*_for_prism()` functions: validates that every
#' non-`NA` `timepoint` value is actually numeric (raising an informative
#' error otherwise), then drops rows with no timepoint assigned yet.
#'
#' @keywords internal
prepare_prism_timepoints <- function(data) {
  timepoint_numeric <- suppressWarnings(as.numeric(data$timepoint))
  newly_na <- is.na(timepoint_numeric) & !is.na(data$timepoint)
  if (any(newly_na)) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer could not interpret timepoint value(s) %s as numbers (days).",
        paste(sprintf("'%s'", unique(data$timepoint[newly_na])), collapse = ", ")
      ),
      "invalid_timepoint"
    ))
  }

  keep <- !is.na(timepoint_numeric)
  data <- data[keep, , drop = FALSE]
  data$timepoint <- timepoint_numeric[keep]

  if (nrow(data) == 0) {
    stop(mea_condition(
      "MEA Explorer could not build a Prism-style table because no timepoint metadata has been assigned yet.",
      "missing_timepoint"
    ))
  }

  data
}

#' Reshape tidy MEA data into a wide table for pasting into GraphPad Prism
#'
#' Prism's grouped XY tables expect one row per X value (here, timepoint)
#' and one column per replicate. This pivots the long/tidy `metric` column
#' into that shape: one row per timepoint, one column per well — or, when
#' more than one `group` value is present in `data`, one column per
#' (group, well) pair, since Prism's grouped-table columns need to stay
#' distinguishable by condition (e.g. "ASD - A1" vs "Control - A1").
#'
#' @param data Tidy MEA data with `well`, `timepoint`, and `metric` columns
#'   (and `group`, if given).
#' @param metric Name of the measurement column to pivot.
#' @param group Optional name of a column identifying the experimental
#'   group/condition (e.g. "treatment"). Only used to disambiguate column
#'   names when more than one group is present in `data`.
#' @return A tibble: one `timepoint` column (numeric, sorted ascending),
#'   plus one column per well (or per group/well pair). A (well, timepoint)
#'   combination with no observation is `NA`, never dropped or zero-filled.
#' @export
pivot_wide_for_prism <- function(data, metric, group = NULL) {
  required <- c("well", "timepoint", metric)
  if (!is.null(group)) required <- c(required, group)
  require_columns(data, required, context = "data")

  data <- prepare_prism_timepoints(data)

  multiple_groups <- !is.null(group) && length(unique(data[[group]])) > 1
  column_key <- if (multiple_groups) paste(data[[group]], data$well, sep = " - ") else data$well

  key_counts <- stats::aggregate(
    list(n = data$timepoint),
    by = list(key = column_key, timepoint = data$timepoint),
    FUN = length
  )
  duplicated <- key_counts[key_counts$n > 1, , drop = FALSE]
  if (nrow(duplicated) > 0) {
    stop(mea_condition(
      "MEA Explorer found more than one value for the same well and timepoint and cannot lay them out as separate Prism columns. Filter to one file per timepoint first.",
      "ambiguous_prism_pivot"
    ))
  }

  timepoints <- sort(unique(data$timepoint))
  wide <- tibble::tibble(timepoint = timepoints)
  for (key in unique(column_key)) {
    rows <- column_key == key
    wide[[key]] <- data[[metric]][rows][match(timepoints, data$timepoint[rows])]
  }
  wide
}

#' Reshape summary statistics into a wide table for Prism's "Mean, SD, N"
#' error-value format
#'
#' Prism's grouped XY tables can take error values already computed
#' elsewhere (its "Enter and plot error values calculated elsewhere ->
#' Mean, SD, N" option) instead of deriving them from replicate columns.
#' This summarizes `metric` by (timepoint[, group]) with [summarize_mea()]
#' — the same statistics [plot_time_course_summary()] plots — and lays
#' out `mean`/`sd`/`n` as columns: one row per timepoint, one Mean/SD/N
#' trio per group when more than one group is present in `data`.
#'
#' @param data Tidy MEA data with `well`, `timepoint`, and `metric`
#'   columns (and `group`, if given).
#' @param metric Name of the measurement column to summarize.
#' @param group Optional name of a column identifying the experimental
#'   group/condition. Only used to label columns when more than one
#'   group is present in `data`.
#' @return A tibble: one `timepoint` column (numeric, sorted ascending),
#'   plus `mean`/`sd`/`n` columns — or `<group> mean`/`<group> sd`/
#'   `<group> n` per group when multiple groups are present. A group with
#'   no observations at a given timepoint gets `NA` mean/sd and `n = 0`.
#' @export
pivot_summary_for_prism <- function(data, metric, group = NULL) {
  required <- c("well", "timepoint", metric)
  if (!is.null(group)) required <- c(required, group)
  require_columns(data, required, context = "data")

  data <- prepare_prism_timepoints(data)

  multiple_groups <- !is.null(group) && length(unique(data[[group]])) > 1
  group_cols <- c("timepoint", if (multiple_groups) group)
  summary_data <- summarize_mea(data, metric, group_cols = group_cols)

  if (!multiple_groups) {
    out <- summary_data[order(summary_data$timepoint), c("timepoint", "mean", "sd", "n")]
    return(tibble::as_tibble(out))
  }

  timepoints <- sort(unique(summary_data$timepoint))
  wide <- tibble::tibble(timepoint = timepoints)
  for (g in sort(unique(summary_data[[group]]))) {
    rows <- summary_data[[group]] == g
    sub <- summary_data[rows, ]
    idx <- match(timepoints, sub$timepoint)
    wide[[paste(g, "mean")]] <- sub$mean[idx]
    wide[[paste(g, "sd")]] <- sub$sd[idx]
    wide[[paste(g, "n")]] <- ifelse(is.na(idx), 0L, sub$n[idx])
  }
  wide
}
