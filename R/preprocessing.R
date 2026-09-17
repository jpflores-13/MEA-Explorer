# Experimental metadata assignment.
#
# The raw Axion Treatment/ID row is frequently blank in practice, so MEA
# Explorer lets researchers assign experimental metadata (experiment_id,
# treatment, timepoint, biological_replicate, notes) to wells *after*
# import, rather than trying to infer it from the source file. This is
# deliberately a plain data operation: build a `metadata` tibble keyed by
# (source_file, well) and apply it with `apply_metadata()`. A Shiny
# plate-mapper is just a UI for building that table — it should not need
# to duplicate any of this logic.
#
# `notes` is free text (e.g. "media change day 20", "possible
# contamination") — unlike the other metadata columns it's never grouped,
# summarized, or plotted on; it exists purely for a researcher to record
# and later read back.

METADATA_COLUMNS <- c("experiment_id", "treatment", "timepoint", "biological_replicate", "notes")

#' Build an empty metadata scaffold for a parsed MEA dataset
#'
#' One row per unique (source_file, well) pair present in `data`, with all
#' metadata fields set to `NA`. Intended as the starting point for a
#' plate-mapper UI or manual metadata assignment — nothing is guessed.
#'
#' @param data Tidy data as returned by [read_axion_well_averages()] /
#'   [read_axion_mea()]. Must have `source_file` and `well` columns.
#' @return A tibble: source_file, well, experiment_id, treatment,
#'   timepoint, biological_replicate, notes (all metadata columns `NA`).
#' @export
initialize_metadata <- function(data) {
  require_columns(data, c("source_file", "well"), context = "data")

  metadata <- dplyr::distinct(data, source_file, well)
  for (col in METADATA_COLUMNS) {
    metadata[[col]] <- NA_character_
  }
  metadata
}

#' Attach experimental metadata to a parsed MEA dataset
#'
#' Left-joins a researcher-provided `metadata` table onto `data` by
#' (source_file, well). Wells with no matching metadata row keep `NA`
#' metadata — nothing is inferred or defaulted. Metadata rows that do not
#' correspond to any well in `data` are rejected rather than silently
#' dropped, since that usually indicates a typo'd well ID or source file
#' name.
#'
#' @param data Tidy data as returned by [read_axion_mea()]. Must not
#'   already contain metadata columns (`experiment_id`, `treatment`,
#'   `timepoint`, `biological_replicate`, `notes`) — apply metadata once,
#'   to freshly parsed data. To revise an assignment, edit the `metadata`
#'   table (the source of truth) and re-apply it to the original parsed
#'   data, rather than joining on top of an already-joined result.
#' @param metadata A tibble with `source_file`, `well`, and any subset of
#'   `experiment_id`, `treatment`, `timepoint`, `biological_replicate`,
#'   `notes`. Missing metadata columns are added as `NA`. At most one row
#'   per (source_file, well) pair.
#' @return `data` with metadata columns joined in, in canonical column
#'   order: source_file, experiment_id, well, treatment, timepoint,
#'   biological_replicate, notes, followed by whatever measurement columns
#'   were already in `data` (e.g. mean_firing_rate_hz, number_of_bursts).
#' @export
apply_metadata <- function(data, metadata) {
  require_columns(data, c("source_file", "well"), context = "data")
  require_columns(metadata, c("source_file", "well"), context = "metadata")

  already_present <- intersect(METADATA_COLUMNS, colnames(data))
  if (length(already_present) > 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer found existing metadata column(s) (%s) in the data passed to apply_metadata(). Apply metadata to the freshly parsed data, then re-apply your updated metadata table, rather than joining metadata twice.",
        paste(sprintf("'%s'", already_present), collapse = ", ")
      ),
      "metadata_already_applied"
    ))
  }

  duplicated_keys <- metadata |>
    dplyr::count(source_file, well) |>
    dplyr::filter(n > 1)

  if (nrow(duplicated_keys) > 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer found more than one metadata row for well(s) %s and cannot determine which to use.",
        paste(sprintf("'%s' in '%s'", duplicated_keys$well, duplicated_keys$source_file), collapse = ", ")
      ),
      "ambiguous_metadata"
    ))
  }

  known_wells <- dplyr::distinct(data, source_file, well)
  orphaned <- dplyr::anti_join(metadata, known_wells, by = c("source_file", "well"))

  if (nrow(orphaned) > 0) {
    stop(mea_condition(
      sprintf(
        "MEA Explorer found metadata for well(s) %s that do not appear in the parsed data.",
        paste(sprintf("'%s' in '%s'", orphaned$well, orphaned$source_file), collapse = ", ")
      ),
      "orphaned_metadata"
    ))
  }

  for (col in METADATA_COLUMNS) {
    if (!col %in% colnames(metadata)) {
      metadata[[col]] <- NA_character_
    }
  }

  joined <- dplyr::left_join(
    data,
    metadata[, c("source_file", "well", METADATA_COLUMNS)],
    by = c("source_file", "well")
  )

  measurement_columns <- setdiff(colnames(data), c("source_file", "well"))
  dplyr::select(
    joined,
    "source_file", "experiment_id", "well", "treatment", "timepoint",
    "biological_replicate", "notes",
    dplyr::all_of(measurement_columns)
  )
}
