# Reshaping tidy MEA data for external tools that expect a wide layout
# (e.g. GraphPad Prism's grouped XY tables: one row per X value, one column
# per replicate) rather than the long/tidy shape the rest of R/ works in.

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
  timepoint_numeric <- timepoint_numeric[keep]

  if (length(timepoint_numeric) == 0) {
    stop(mea_condition(
      "MEA Explorer could not build a Prism-style table because no timepoint metadata has been assigned yet.",
      "missing_timepoint"
    ))
  }

  multiple_groups <- !is.null(group) && length(unique(data[[group]])) > 1
  column_key <- if (multiple_groups) paste(data[[group]], data$well, sep = " - ") else data$well

  key_counts <- stats::aggregate(
    list(n = timepoint_numeric),
    by = list(key = column_key, timepoint = timepoint_numeric),
    FUN = length
  )
  duplicated <- key_counts[key_counts$n > 1, , drop = FALSE]
  if (nrow(duplicated) > 0) {
    stop(mea_condition(
      "MEA Explorer found more than one value for the same well and timepoint and cannot lay them out as separate Prism columns. Filter to one file per timepoint first.",
      "ambiguous_prism_pivot"
    ))
  }

  timepoints <- sort(unique(timepoint_numeric))
  wide <- tibble::tibble(timepoint = timepoints)
  for (key in unique(column_key)) {
    rows <- column_key == key
    wide[[key]] <- data[[metric]][rows][match(timepoints, timepoint_numeric[rows])]
  }
  wide
}
