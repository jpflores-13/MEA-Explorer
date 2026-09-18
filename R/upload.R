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

#' Infer a condition/group label from a recording name, if present
#'
#' Axion recording names in practice follow `<date> <tokens...> <day
#' marker>` (e.g. "20260505 KOLF EGC BUMP d40"), where `<tokens...>` — a
#' genotype, cell type, protocol, etc. — is NOT always in a fixed order
#' (a real dataset had both "KOLF EGC BUMP" and "KOLF BUMP EGC") and isn't
#' drawn from a known closed vocabulary. Rather than guess at parsing
#' those tokens individually, this strips the leading date and the
#' trailing day marker (the same token [infer_timepoint_days()] finds)
#' and returns whatever remains, whitespace-normalized, as a single
#' label. Read from the file's own `Recording Name` header field, never
#' the upload filename. Intended only to pre-fill an editable Condition
#' field in the Upload UI — always shown and overridable, never applied
#' silently — see CLAUDE.md's "Longitudinal data model" section.
#'
#' @param recording_name A recording name string, e.g. from
#'   [read_recording_name()]. `NA` returns `NA`.
#' @return A single character label, or `NA` if nothing recognizable
#'   remains after stripping the date/day tokens.
#' @export
infer_condition_label <- function(recording_name) {
  if (is.na(recording_name)) return(NA_character_)

  x <- sub("^\\s*[0-9]{6,8}\\s*", "", recording_name)
  x <- sub("(?<![A-Za-z0-9])[Dd](?:ay)?[ _]?[0-9]{1,4}\\s*$", "", x, perl = TRUE)
  x <- gsub("\\s+", " ", trimws(x))

  if (!nzchar(x)) NA_character_ else x
}

#' Normalize a condition label for grouping near-duplicates
#'
#' Used only to detect whether two condition labels are "the same modulo
#' formatting" — never as a label to display or apply. Strips a leading
#' non-alphanumeric character (e.g. a stray "_" left over from a
#' filename date separator), case-folds, collapses whitespace, and sorts
#' tokens so word order doesn't matter (e.g. "KOLF EGC BUMP" and "KOLF
#' BUMP EGC" normalize to the same key).
#'
#' Deliberately does NOT fix typos or merge tokens: "BUMPM" stays a
#' different token from "BUMP", and "KOLFEGC" (no space) stays different
#' from "KOLF EGC" — those need a human to confirm, since correcting them
#' automatically means guessing intent, which MEA Explorer never does
#' silently (see CLAUDE.md).
#'
#' @param label A condition/treatment label string.
#' @return A normalized character key, not meant to be shown to a
#'   researcher.
#' @keywords internal
normalize_condition_key <- function(label) {
  x <- toupper(trimws(label))
  x <- sub("^[^A-Z0-9]+", "", x)
  x <- gsub("\\s+", " ", trimws(x))
  tokens <- sort(strsplit(x, " ", fixed = TRUE)[[1]])
  paste(tokens, collapse = " ")
}

#' Group condition labels that are the same modulo formatting
#'
#' @param labels Character vector of raw condition labels, one per file
#'   (repeats expected/used to determine the most common spelling).
#' @return A list of groups, each `list(labels = <distinct raw labels,
#'   most-frequent first>, suggested = <the most frequent one>)`. Only
#'   groups with more than one distinct raw label are returned — an
#'   already-unique label needs no merging and is omitted.
#' @keywords internal
group_condition_labels <- function(labels) {
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0) return(list())

  freq <- table(labels)
  distinct_labels <- names(freq)
  keys <- vapply(distinct_labels, normalize_condition_key, character(1))
  by_key <- split(distinct_labels, keys)
  by_key <- by_key[lengths(by_key) > 1]

  lapply(by_key, function(group_labels) {
    ordered <- group_labels[order(-freq[group_labels])]
    list(labels = unname(ordered), suggested = unname(ordered[[1]]))
  })
}
