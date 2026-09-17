# MEA Explorer

MEA Explorer is an open-source R/Shiny application for reproducible
analysis and visualization of multielectrode array (MEA) experiments.

**Status: v0.1, analytical engine.** Parsing, validation, metadata
assignment, summary statistics, and plotting all exist and are tested
independently of any UI. The Shiny application has not been built yet —
see [DESIGN_SPEC.md](DESIGN_SPEC.md) (when available) and the project
roadmap for what's next.

## What v0.1 does

Parses Axion/AxIS CSV exports that contain a `Well Averages` table and
extracts, per well:

- Mean Firing Rate (Hz)
- Number of Bursts

into a tidy `source_file, well, mean_firing_rate_hz, number_of_bursts`
table. See [DATA_SPEC.md](DATA_SPEC.md) for the exact format supported and
the assumptions the parser makes (and refuses to make).

## Using the analytical engine without Shiny

```r
invisible(sapply(list.files("R", full.names = TRUE), source))

data <- read_axion_well_averages("example_data/20260505_KOLF_EGC_BUMP_d40.csv")
data

# Multiple files, each remaining identifiable via source_file:
combined <- read_axion_mea(c("experiment_1.csv", "experiment_2.csv"))

# Assign experimental metadata after import (nothing is inferred):
scaffold <- initialize_metadata(data)
scaffold$treatment <- c(rep("control", 4), rep("treated", 8))
data_with_metadata <- apply_metadata(data, scaffold)

# Grouped summary statistics (grouping is always explicit, never guessed):
summarize_mea(data_with_metadata, "mean_firing_rate_hz", group_cols = "treatment")

# Plots return ggplot objects; individual observations are always shown,
# never only a mean +/- SEM bar:
plot_group_distribution(data_with_metadata, "mean_firing_rate_hz", "treatment")
plot_plate_heatmap(data, "number_of_bursts")
```

## Running the tests

```r
library(testthat)
invisible(sapply(list.files("R", full.names = TRUE), source))
test_dir("tests/testthat")
```

(Once `renv` is restored, this can also be run as
`testthat::test_dir("tests/testthat")` after `devtools::load_all()` in a
package-shaped layout, if the project is later converted into a package.)

## Repository layout

```
MEA-Explorer/
├── R/
│   ├── import_axion.R   # public read_axion_well_averages() / read_axion_mea()
│   ├── validation.R     # section/row matching + informative error conditions
│   ├── preprocessing.R  # experimental metadata assignment
│   ├── metrics.R        # summarize_mea(): explicit, grouped summary stats
│   └── plotting.R       # ggplot-returning plot_*() functions
├── tests/testthat/       # unit tests + synthetic fixture CSVs
├── example_data/         # a real (de-identified metadata) Axion export
├── DATA_SPEC.md          # supported CSV format, in detail
```
