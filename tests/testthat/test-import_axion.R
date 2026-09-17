fixture <- function(name) testthat::test_path("fixtures", name)

test_that("valid Well Averages section is parsed correctly", {
  result <- read_axion_well_averages(fixture("valid.csv"))

  expect_equal(colnames(result), c(
    "source_file", "well", "mean_firing_rate_hz", "number_of_bursts"
  ))
  expect_equal(nrow(result), 3)
  expect_equal(result$source_file, rep("valid.csv", 3))
  expect_equal(result$well, c("A1", "A2", "A3"))
  expect_equal(result$mean_firing_rate_hz, c(0.001747, 0.052044, 0.005788))
  expect_equal(result$number_of_bursts, c(0, 71, 0))
})

test_that("the Treatment Averages and per-electrode Measurement tables are ignored", {
  # valid.csv deliberately repeats "Mean Firing Rate (Hz)" and
  # "Number of Bursts" in a Treatment Averages ("- Avg") row and in a
  # per-electrode Measurement table with the exact same labels. Parsing
  # must be anchored to the Well Averages section, not the first match
  # found anywhere in the file.
  result <- read_axion_well_averages(fixture("valid.csv"))

  expect_false(any(result$mean_firing_rate_hz == 0.2))
  expect_false(any(result$number_of_bursts == 12))
})

test_that("a missing Mean Firing Rate (Hz) row raises an informative error", {
  expect_error(
    read_axion_well_averages(fixture("missing_mean_firing_rate.csv")),
    "Mean Firing Rate \\(Hz\\)",
    class = "mea_missing_measurement"
  )
})

test_that("a missing Number of Bursts row raises an informative error", {
  expect_error(
    read_axion_well_averages(fixture("missing_number_of_bursts.csv")),
    "Number of Bursts",
    class = "mea_missing_measurement"
  )
})

test_that("a missing Well Averages section raises an informative error", {
  expect_error(
    read_axion_well_averages(fixture("missing_well_averages_section.csv")),
    "Well Averages",
    class = "mea_missing_well_averages_section"
  )
})

test_that("missing measurement values are preserved as NA, never zero", {
  result <- read_axion_well_averages(fixture("missing_values.csv"))

  expect_equal(result$mean_firing_rate_hz, c(0.001747, NA, 0.005788))
  expect_equal(result$number_of_bursts, c(0, 0, NA))
  expect_false(any(is.nan(result$mean_firing_rate_hz)))
})

test_that("malformed numeric values become NA rather than erroring or zero", {
  result <- read_axion_well_averages(fixture("malformed_values.csv"))

  expect_equal(result$mean_firing_rate_hz, c(0.001747, NA, 0.005788))
  expect_equal(result$number_of_bursts, c(0, 71, NA))
})

test_that("multiple uploaded experiments combine with provenance preserved", {
  result <- read_axion_mea(c(fixture("experiment_1.csv"), fixture("experiment_2.csv")))

  expect_equal(nrow(result), 4)
  expect_equal(result$source_file, c(
    "experiment_1.csv", "experiment_1.csv", "experiment_2.csv", "experiment_2.csv"
  ))
  expect_equal(result$well, c("A1", "A2", "B1", "B2"))
  expect_equal(result$mean_firing_rate_hz, c(0.01, 0.02, 0.03, 0.04))
  expect_equal(result$number_of_bursts, c(1, 2, 3, 4))
})

test_that("a nonexistent file raises an informative error", {
  expect_error(
    read_axion_well_averages(fixture("does_not_exist.csv")),
    "could not find the file",
    class = "mea_file_not_found"
  )
})
