# Parser for Axion/AxIS CSV exports containing a "Well Averages" table.
#
# See DATA_SPEC.md for the supported file format and the exact rules used
# to locate the table. See R/validation.R for the internal helpers that do
# the actual section/row matching and error reporting.

#' Read the Well Averages table from a single Axion/AxIS CSV export
#'
#' Extracts Mean Firing Rate (Hz) and Number of Bursts for each well,
#' anchored to the "Well Averages" section of the file (never a global
#' search of the whole CSV, since other tables can repeat the same metric
#' labels).
#'
#' @param path Path to an Axion/AxIS CSV export.
#' @return A tibble with one row per well:
#'   \describe{
#'     \item{source_file}{Base name of `path`, preserved for provenance.}
#'     \item{well}{Well identifier, e.g. "A1".}
#'     \item{mean_firing_rate_hz}{Mean Firing Rate (Hz); `NA` if missing.}
#'     \item{number_of_bursts}{Number of Bursts; `NA` if missing.}
#'   }
#' @export
read_axion_well_averages <- function(path) {
  if (!file.exists(path)) {
    stop(mea_condition(
      sprintf("MEA Explorer could not find the file '%s'.", path),
      "file_not_found",
      path
    ))
  }

  lines <- readr::read_lines(path)
  header <- find_well_averages_header(lines, path)
  section_lines <- extract_section_lines(lines, header$row_index)

  mean_firing_rate_hz <- extract_measurement(
    section_lines, header$wells, "Mean Firing Rate (Hz)", path
  )
  number_of_bursts <- extract_measurement(
    section_lines, header$wells, "Number of Bursts", path
  )

  tibble::tibble(
    source_file = basename(path),
    well = header$wells,
    mean_firing_rate_hz = mean_firing_rate_hz,
    number_of_bursts = number_of_bursts
  )
}

#' Read and combine Well Averages tables from multiple Axion/AxIS CSV exports
#'
#' Each file is parsed independently with [read_axion_well_averages()]; the
#' `source_file` column preserves provenance so rows from different files
#' remain identifiable after combining. No relationship between files
#' (timepoint, replicate, treatment, etc.) is inferred from filenames.
#'
#' @param paths Character vector of paths to Axion/AxIS CSV exports.
#' @return A combined tibble; see [read_axion_well_averages()] for columns.
#' @export
read_axion_mea <- function(paths) {
  dplyr::bind_rows(lapply(paths, read_axion_well_averages))
}
