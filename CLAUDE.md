# CLAUDE.md — engineering & scientific guardrails for MEA Explorer

MEA Explorer is an open-source R/Shiny application for reproducible
analysis and visualization of multielectrode array (MEA) experiments. This
is a scientific tool, not a dashboard that merely looks correct.

## Priority order (in this order, always)

1. Data integrity
2. Scientific correctness
3. Reproducibility
4. Testability
5. Maintainability
6. User experience
7. Performance

## Current MVP (v0.1)

Supported input: Axion/AxIS CSV exports containing a `Well Averages`
section. Supported measurements — **only** these two, do not add more
without an explicit request:

1. Mean Firing Rate (Hz)
2. Number of Bursts

Workflow: UPLOAD → PARSE → VALIDATE → ASSIGN EXPERIMENTAL METADATA →
VISUALIZE → EXPORT. Advanced statistical testing is out of scope for the
MVP.

## Axion data format — read this before touching the parser

Axion CSVs contain multiple tables that repeat the same metric labels
(`Treatment Averages`, `Well Averages`, per-electrode `Measurement`).
**Never** search the whole file for a metric name and take the first
match. Parsing must be anchored to the row whose first field is exactly
`Well Averages`, bounded to the block of rows before the next blank line.
See [DATA_SPEC.md](DATA_SPEC.md) for the full rules and rationale.

## Non-negotiable data rules

- Never convert a missing value to zero. Missing stays `NA`.
- Never silently guess when a section/row can't be uniquely identified —
  raise an informative, classed error instead (see `R/validation.R`).
- Never discard file provenance. Every row traces back to a
  `source_file`.
- Never infer experimental relationships (timepoint, replicate, treatment)
  from filenames or upload order unless explicitly instructed.
- Never treat wells as independent biological replicates, and never treat
  electrode-level measurements as biological replicates. The hierarchy is
  biological replicate → plate → well → electrode; the current
  measurements are **well averages**.
- Never silently aggregate or exclude observations, and never silently
  impute.

## Architecture

Keep the analytical engine (`R/`) callable and testable without Shiny:

```r
data <- read_axion_mea(paths)
summary_data <- summarize_mea(data, metric = "mean_firing_rate_hz")
```

Shiny code should call into `R/` functions rather than compute inside
`render*()` blocks. Reactives should hold the *result* of an `R/`
function call, and render functions should only format/plot that result.

## Longitudinal data model (Time course tab)

MEA recordings in this project are longitudinal: the same plate/wells are
recorded repeatedly over many days, and Axion exports one file per
recording session. So **one uploaded file = one recording day for the
whole plate** — timepoint (days in culture) is therefore assigned per
*file*, via `R/preprocessing.R`'s `initialize_metadata()`/
`apply_metadata()`, never inferred from the filename even when the
filename encodes it (e.g. `..._d40.csv`).

Experimental group/condition (e.g. "ASD" vs "Control") is, for now, also
assigned per file, using the same `treatment` metadata column. **This is
only correct when each condition is recorded as its own set of files/
plates.** A single file/plate that mixes conditions across different
wells needs a real per-well Plate Map (the design handbook's Plate Map
stage, not built yet) — per-file assignment would incorrectly label every
well in that file with the same condition. Don't extend the per-file
shortcut to that case; build proper per-well assignment instead when it's
needed.

## Summary (mean ± SEM) plots are an intentional, scoped exception

The "never a bar/summary-only plot" rule still holds for well-level and
group-comparison plots (`plot_well_values()`, `plot_group_distribution()`,
`plot_replicate_structure()`, `plot_time_course()`). But
`plot_time_course_summary()` (`R/plotting.R`) deliberately draws a mean ±
SEM curve per group over time — the standard longitudinal-MEA figure
(what a researcher would recreate in GraphPad Prism) — because clients
specifically need that shape for publication figures. It stays compliant
with the transparency principle a different way: every individual
well-level observation the error bar summarizes is *still* plotted
underneath, as small semi-transparent points, so the spread stays
checkable against the raw data. Don't drop those points to get a cleaner
match to a reference figure; if an exact match (no points) is ever
wanted, that's a product decision to make explicitly, not a default.

`R/export.R`'s `pivot_wide_for_prism()` reshapes tidy data into the wide,
one-row-per-timepoint / one-column-per-well layout Prism's grouped XY
tables expect, for direct copy-paste import. This is a display/export
concern only — it never changes what `read_axion_mea()` /
`summarize_mea()` compute, and a missing (well, timepoint) combination
stays `NA`, never dropped or zero-filled.

## Testing

The parser must have `testthat` tests covering (at minimum): valid data,
missing Mean Firing Rate, missing Number of Bursts, missing `Well
Averages` section, missing measurement values, malformed numeric values,
and multiple uploaded files. Verify exact expected output, not just "no
error". Scientific transformations must be testable independently of
Shiny.

## Error messages shown to researchers

State WHAT went wrong, WHERE if possible, and HOW to resolve it. Never
surface a raw R stack trace (e.g. "subscript out of bounds") through the
normal interface — translate it, e.g. "MEA Explorer could not find a
'Well Averages' section in this file."

## Development behavior

Before a major change: inspect the repo, identify relevant files, explain
the proposed implementation and its scientific/data assumptions, then
implement the smallest coherent change and run the tests. Don't rewrite
working code to impose a preferred architecture. Don't invent scientific
requirements — ask when genuinely ambiguous. Don't add metrics, statistics,
or UI beyond what's been asked for.
