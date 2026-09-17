# Batch parsing for interactive upload flows (e.g. Shiny's fileInput()).
#
# read_axion_mea() (R/import_axion.R) is the right tool when the caller
# already knows every file is well-formed. Upload flows can't assume that:
# a researcher may select ten files where one is the wrong export type.
# read_axion_mea_report() parses each file independently so one bad file
# never blocks the others, and returns *why* a file failed instead of
# letting a raw error abort the batch.

#' Read and combine Well Averages tables from multiple uploaded files,
#' without letting one unparseable file abort the batch
#'
#' Upload handlers (e.g. Shiny's `fileInput()`) typically store each
#' uploaded file at a temporary path (`datapath`) that has nothing to do
#' with the name the researcher recognizes (`name`). This function parses
#' each file at its `paths` location but records provenance under
#' `display_names`, so `source_file` in the output stays meaningful.
#'
#' @param paths Character vector of file paths to read (e.g. `datapath`
#'   from a Shiny `fileInput()`).
#' @param display_names Character vector, same length as `paths`, used as
#'   the `source_file` value instead of `basename(paths)`. Defaults to
#'   `basename(paths)`.
#' @return A list with two tibbles:
#'   \describe{
#'     \item{data}{Combined rows from every file that parsed successfully;
#'       same columns as [read_axion_mea()].}
#'     \item{errors}{One row per file that failed to parse:
#'       `source_file` (the display name) and `message` (the parse
#'       error's message, already safe to show a researcher directly —
#'       see R/validation.R).}
#'   }
#' @export
read_axion_mea_report <- function(paths, display_names = basename(paths)) {
  empty_data <- tibble::tibble(
    source_file = character(0), well = character(0),
    mean_firing_rate_hz = double(0), number_of_bursts = double(0)
  )
  empty_errors <- tibble::tibble(source_file = character(0), message = character(0))

  if (length(paths) == 0) {
    return(list(data = empty_data, errors = empty_errors))
  }

  if (length(paths) != length(display_names)) {
    stop(mea_condition(
      "MEA Explorer received a different number of file paths and display names.",
      "mismatched_upload_names"
    ))
  }

  results <- Map(
    function(path, name) {
      tryCatch(
        {
          parsed <- read_axion_well_averages(path)
          parsed$source_file <- name
          list(ok = TRUE, value = parsed)
        },
        mea_parse_error = function(e) {
          list(ok = FALSE, value = tibble::tibble(source_file = name, message = conditionMessage(e)))
        }
      )
    },
    paths, display_names
  )

  ok <- vapply(results, `[[`, logical(1), "ok")
  succeeded <- lapply(results[ok], `[[`, "value")
  failed <- lapply(results[!ok], `[[`, "value")

  list(
    data = if (length(succeeded) > 0) dplyr::bind_rows(succeeded) else empty_data,
    errors = if (length(failed) > 0) dplyr::bind_rows(failed) else empty_errors
  )
}

#' Read the "Recording Name" field from an Axion/AxIS CSV export's header
#'
#' @param path Path to an Axion/AxIS CSV export.
#' @return A trimmed character string, or `NA` if the file doesn't exist or
#'   has no `Recording Name` line in its first few header lines.
#' @export
read_recording_name <- function(path) {
  if (!file.exists(path)) return(NA_character_)

  lines <- readr::read_lines(path, n_max = 10)
  match_row <- which(startsWith(trimws(lines), "Recording Name"))
  if (length(match_row) == 0) return(NA_character_)

  line <- lines[[match_row[[1]]]]
  colon <- regexpr(":", line, fixed = TRUE)
  if (colon == -1) return(NA_character_)

  value <- trimws(substr(line, colon + 1, nchar(line)))
  if (!nzchar(value)) NA_character_ else value
}

#' Infer a days-in-culture timepoint from a recording name, if present
#'
#' Looks for a trailing "d40" / "D18" / "day 12"-style token in the
#' recording name — read from the file's own `Recording Name` header
#' field, never from the upload filename (MEA Explorer never infers
#' experimental relationships from filenames). Intended only to pre-fill
#' an editable timepoint field in the Upload UI: the inferred value is
#' always shown to the researcher and can be overridden, never applied
#' silently — see CLAUDE.md's "Longitudinal data model" section.
#'
#' @param recording_name A recording name string, e.g. from
#'   [read_recording_name()]. `NA` returns `NA`.
#' @return A single integer (days), or `NA` if no recognizable pattern is
#'   found.
#' @export
infer_timepoint_days <- function(recording_name) {
  if (is.na(recording_name)) return(NA_integer_)

  matches <- regmatches(
    recording_name,
    gregexpr("(?<![A-Za-z0-9])[Dd](?:ay)?[ _]?([0-9]{1,4})(?![0-9])", recording_name, perl = TRUE)
  )[[1]]

  if (length(matches) == 0) return(NA_integer_)

  last_match <- matches[[length(matches)]]
  digits <- regmatches(last_match, regexpr("[0-9]+", last_match))
  as.integer(digits)
}
