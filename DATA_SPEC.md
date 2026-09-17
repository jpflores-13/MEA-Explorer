# DATA_SPEC.md — Supported Axion/AxIS CSV format (v0.1)

This describes the input format `R/import_axion.R` and `R/validation.R`
currently support. It documents what the parser assumes, what it never
does, and how to extend it later.

## Source format

An Axion/AxIS CSV export is not a single tidy table. It is a sequence of
loosely related tables separated by blank lines, for example (abbreviated):

```
Investigator: ...
Recording Name: ...

Maestro Pro Settings,
   Plate Type,CytoView MEA 12
...

Treatment Averages,
Mean Firing Rate (Hz) - Avg,0.097789
Number of Bursts - Avg,46.833333
...

Well Averages,A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4
Treatment/ID,,,,,,,,,,,,
Number of Spikes,67,1,2,1996,...
Mean Firing Rate (Hz),0.001747,2.6E-05,5.2E-05,0.052044,...
Number of Bursts,0,0,0,71,...
...

Measurement,A1_11,A1_12,...
Mean Firing Rate (Hz),0,0,...
Number of Bursts,0,0,...
```

Critically, **metric names are not unique across the file**:

- `Treatment Averages` has plate-wide averages, suffixed `- Avg`
  (e.g. `Mean Firing Rate (Hz) - Avg`) — a different metric label, and
  not per-well.
- `Well Averages` has the exact per-well values MEA Explorer v0.1 uses.
- `Measurement` (per-electrode) repeats the **exact same** row labels as
  `Well Averages` (`Mean Firing Rate (Hz)`, `Number of Bursts`, ...) but at
  electrode granularity, not well granularity.

Parsing this file by searching for the first row whose label matches
`Mean Firing Rate (Hz)` would be a data-integrity bug: depending on row
order it could silently return per-electrode or averaged values instead of
per-well values, with no error.

## Parsing rules (v0.1)

1. Find the row whose **first field, trimmed, is exactly** `Well Averages`.
   - Zero matches → error: no `Well Averages` section found.
   - More than one match → error: ambiguous, cannot pick one silently.
2. The remaining fields of that row, trimmed and with empty fields
   dropped, are the well identifiers (e.g. `A1`, `A2`, ..., `C4`), in
   column order.
3. The table's data rows are every line after the header row, up to (but
   not including) the next blank line, or end of file if none. This is
   what excludes the `Measurement` table below it.
4. Within that bounded block only, find the row whose first field is
   exactly `Mean Firing Rate (Hz)`, and the row whose first field is
   exactly `Number of Bursts`.
   - Zero or more-than-one match for either, within the section → error.
5. Each matched row's remaining fields are aligned positionally to the
   well identifiers from step 2. Missing trailing fields are padded with
   `NA`, not zero.
6. Field values are converted with `as.numeric()`. Blank fields and
   non-numeric text (e.g. `#ERROR!`, `N/A`) both become `NA`. **Values are
   never coerced to `0`.**

## Output (canonical tidy format, v0.1)

One row per well, one file per call to `read_axion_well_averages()`:

| column                | type      | notes                                   |
|-----------------------|-----------|------------------------------------------|
| `source_file`         | character | `basename()` of the input path           |
| `well`                | character | e.g. `"A1"`                               |
| `mean_firing_rate_hz` | double    | `NA` if missing or malformed              |
| `number_of_bursts`    | double    | `NA` if missing or malformed              |

`read_axion_mea(paths)` parses each file independently with
`read_axion_well_averages()` and row-binds the results. `source_file` is
the only provenance signal — no relationship between files (timepoint,
replicate, treatment) is inferred from filenames or file order.

## Experimental metadata (`R/preprocessing.R`)

The source `Treatment/ID` row is frequently blank in practice and is not
read by the parser. Metadata is instead assigned after import, as a
separate, explicit, user-driven step:

- `initialize_metadata(data)` returns a scaffold — one row per unique
  `(source_file, well)` pair, with `experiment_id`, `treatment`,
  `timepoint`, `biological_replicate`, and `notes` all `NA`. This is what
  a plate-mapper UI (or a manually edited table) starts from.
- `apply_metadata(data, metadata)` left-joins that table onto the parsed
  data by `(source_file, well)`. Unassigned wells stay `NA` — nothing is
  inferred or defaulted. It errors (rather than guessing) on: duplicate
  metadata rows for the same well, metadata referencing a well that
  doesn't exist in `data`, and re-applying to data that already carries
  metadata columns (to avoid silently stacking joins).
- `notes` is free text (e.g. "media change day 20", "possible
  contamination") — unlike the other metadata columns it's never grouped,
  summarized, or plotted on. It exists purely for a researcher to record
  and later read back.

Canonical extended output column order:

```
source_file, experiment_id, well, treatment, timepoint,
biological_replicate, notes, mean_firing_rate_hz, number_of_bursts
```

## Explicit non-goals of v0.1

- No metrics beyond Mean Firing Rate (Hz) and Number of Bursts.
- No use of the `Treatment Averages` or per-electrode `Measurement` tables.
- No inference of experimental relationships (treatment, timepoint,
  biological replicate) from filenames, upload order, or the source CSV.
- No aggregation, exclusion, or imputation of any kind.

## Error handling

Parsing failures raise a classed R condition (`mea_parse_error`, with a
more specific subclass such as `mea_missing_well_averages_section` or
`mea_ambiguous_measurement`) carrying a plain-language `message` intended
to be shown directly to a researcher — see `R/validation.R`.
