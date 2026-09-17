# Internal helpers for locating and validating the "Well Averages" table
# inside an Axion/AxIS CSV export, and for reporting informative errors
# when that table cannot be uniquely identified.
#
# An Axion export contains several tables (Treatment Averages, Well
# Averages, per-electrode Measurement) that can repeat the same metric
# label (e.g. "Number of Bursts" appears in more than one table). Parsing
# must therefore be anchored to the "Well Averages" header row and bounded
# to the block of rows that immediately follows it, rather than searching
# the file globally for a metric name.

#' Construct a classed MEA parsing/validation error
#'
#' @param message Human-readable message shown to the researcher.
#' @param subclass Short machine-readable error subtype, e.g.
#'   "missing_well_averages_section".
#' @param path File path the error pertains to (for programmatic use;
#'   not necessarily repeated in `message`).
#' @keywords internal
mea_condition <- function(message, subclass, path = NULL) {
  structure(
    class = c(paste0("mea_", subclass), "mea_parse_error", "error", "condition"),
    list(message = message, call = NULL, path = path)
  )
}

#' Assert that a data frame has the given columns
#'
#' @param data A data frame.
#' @param columns Character vector of required column names.
#' @param context Short description of `data` used in the error message,
#'   e.g. "data" or "metadata".
#' @keywords internal
require_columns <- function(data, columns, context) {
  missing <- setdiff(columns, colnames(data))
  if (length(missing) > 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer expected column(s) %s in the %s but did not find them.",
        paste(sprintf("'%s'", missing), collapse = ", "), context
      ),
      "missing_columns"
    ))
  }
}

#' Get the first comma-delimited field of each line
#' @keywords internal
first_field <- function(lines) {
  split <- strsplit(trimws(lines), ",", fixed = TRUE)
  vapply(split, function(x) if (length(x) == 0) "" else trimws(x[[1]]), character(1))
}

#' Locate the "Well Averages" header row and the well identifiers it declares
#'
#' @param lines Character vector of raw CSV lines (one per row).
#' @param path Source file path, used only for error messages.
#' @return A list with `row_index` (position of the header row in `lines`)
#'   and `wells` (character vector of well identifiers, e.g. "A1", "A2").
#' @keywords internal
find_well_averages_header <- function(lines, path) {
  matches <- which(first_field(lines) == "Well Averages")

  if (length(matches) == 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer could not find a 'Well Averages' section in '%s'. Check that this is a valid Axion Well Averages export.",
        basename(path)
      ),
      "missing_well_averages_section",
      path
    ))
  }

  if (length(matches) > 1) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer found multiple 'Well Averages' sections in '%s' and cannot determine which one to use.",
        basename(path)
      ),
      "ambiguous_well_averages_section",
      path
    ))
  }

  header_index <- matches[[1]]
  fields <- stringr::str_split(lines[[header_index]], ",")[[1]]
  wells <- trimws(fields[-1])
  wells <- wells[nzchar(wells)]

  if (length(wells) == 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer found a 'Well Averages' section in '%s' but no well identifiers were listed alongside it.",
        basename(path)
      ),
      "missing_well_identifiers",
      path
    ))
  }

  list(row_index = header_index, wells = wells)
}

#' Extract the block of rows belonging to the Well Averages table
#'
#' The table runs from the row after the header until the next blank line
#' (or end of file), which is where Axion exports separate this table from
#' whatever follows (e.g. a per-electrode Measurement table).
#'
#' @param lines Character vector of raw CSV lines.
#' @param header_index Row index of the "Well Averages" header line.
#' @return Character vector of the data rows belonging to this table
#'   (may be empty if the header is the last non-blank line).
#' @keywords internal
extract_section_lines <- function(lines, header_index) {
  n <- length(lines)
  if (header_index >= n) {
    return(character(0))
  }

  rest <- lines[(header_index + 1):n]
  blank <- which(nchar(trimws(rest)) == 0)
  end <- if (length(blank) > 0) blank[[1]] - 1 else length(rest)

  if (end < 1) {
    return(character(0))
  }

  rest[seq_len(end)]
}

#' Extract one named measurement row from the Well Averages table
#'
#' @param section_lines Rows belonging to the Well Averages table, as
#'   returned by [extract_section_lines()].
#' @param wells Character vector of well identifiers (defines the expected
#'   number and order of values).
#' @param measurement_name Exact label to match in the first field of a row,
#'   e.g. "Mean Firing Rate (Hz)".
#' @param path Source file path, used only for error messages.
#' @return Numeric vector aligned to `wells`, with missing or malformed
#'   values as `NA` (never coerced to zero).
#' @keywords internal
extract_measurement <- function(section_lines, wells, measurement_name, path) {
  matches <- which(first_field(section_lines) == measurement_name)

  if (length(matches) == 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer could not find a '%s' row in the Well Averages section of '%s'.",
        measurement_name, basename(path)
      ),
      "missing_measurement",
      path
    ))
  }

  if (length(matches) > 1) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer found multiple '%s' rows in the Well Averages section of '%s' and cannot determine which one to use.",
        measurement_name, basename(path)
      ),
      "ambiguous_measurement",
      path
    ))
  }

  fields <- stringr::str_split(section_lines[[matches[[1]]]], ",")[[1]]
  values <- fields[-1]

  # Axion rows may have fewer trailing fields than there are wells when
  # trailing values are blank; pad with NA rather than misaligning wells.
  length(values) <- length(wells)
  values <- trimws(values)
  values[is.na(values) | !nzchar(values)] <- NA_character_

  suppressWarnings(as.numeric(values))
}
