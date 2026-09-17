# Summary statistics over a single MEA metric.
#
# Grouping is always explicit: summarize_mea() never guesses which columns
# to group by, so it never silently aggregates observations the caller
# didn't ask to combine. Missing (NA) values in the metric are excluded
# from the statistics but counted, never dropped or imputed; a group whose
# metric is entirely NA reports NA statistics, never 0 or NaN.

#' Summarize the non-missing values of a single metric
#' @keywords internal
summarize_metric_values <- function(values) {
  non_missing <- values[!is.na(values)]
  n <- length(non_missing)

  # mean()/sd()/median() of a zero-length vector return NaN, not NA, in
  # base R; guard explicitly so an all-missing group reports NA_real_.
  tibble::tibble(
    n = n,
    n_missing = length(values) - n,
    mean = if (n == 0) NA_real_ else mean(non_missing),
    sd = if (n == 0) NA_real_ else stats::sd(non_missing),
    sem = if (n == 0) NA_real_ else stats::sd(non_missing) / sqrt(n),
    median = if (n == 0) NA_real_ else stats::median(non_missing)
  )
}

#' Summarize an MEA metric, optionally grouped
#'
#' @param data Tidy MEA data (one row per well, optionally with metadata
#'   columns from apply_metadata()).
#' @param metric Column name (string) of the metric to summarize, e.g.
#'   "mean_firing_rate_hz" or "number_of_bursts".
#' @param group_cols Character vector of column names to group by (e.g.
#'   c("treatment"), c("treatment", "timepoint")). Default: character(0),
#'   which summarizes over all rows as a single group. This is NEVER
#'   guessed/auto-detected from the data — the caller must specify it
#'   explicitly (this is a hard project rule: never silently aggregate
#'   observations in a way the caller didn't ask for).
#' @return A tibble with the `group_cols` columns (if any), then `n`
#'   (count of non-missing `metric` observations used), `n_missing`
#'   (count of NA `metric` observations in that group), `mean`, `sd`,
#'   `sem` (standard error of the mean = sd / sqrt(n)), `median`.
#' @export
summarize_mea <- function(data, metric, group_cols = character(0)) {
  require_columns(data, c(metric, group_cols), context = "data")

  if (!is.numeric(data[[metric]])) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer cannot summarize '%s' because it is not a numeric column.",
        metric
      ),
      "non_numeric_metric"
    ))
  }

  if (length(group_cols) == 0) {
    return(summarize_metric_values(data[[metric]]))
  }

  grouped <- dplyr::group_by(data, dplyr::across(dplyr::all_of(group_cols)))
  dplyr::group_modify(grouped, function(rows, keys) {
    summarize_metric_values(rows[[metric]])
  }) |>
    dplyr::ungroup()
}
