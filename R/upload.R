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
