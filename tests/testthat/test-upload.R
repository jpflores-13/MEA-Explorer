fixture <- function(name) testthat::test_path("fixtures", name)

test_that("all files parse: data has every well, errors is empty", {
  result <- read_axion_mea_report(c(fixture("experiment_1.csv"), fixture("experiment_2.csv")))

  expect_equal(nrow(result$data), 4)
  expect_equal(result$data$source_file, c(
    "experiment_1.csv", "experiment_1.csv", "experiment_2.csv", "experiment_2.csv"
  ))
  expect_equal(result$data$well, c("A1", "A2", "B1", "B2"))
  expect_equal(nrow(result$errors), 0)
  expect_equal(colnames(result$errors), c("source_file", "message"))
})

test_that("one bad file does not block the others", {
  result <- read_axion_mea_report(c(
    fixture("valid.csv"),
    fixture("missing_well_averages_section.csv"),
    fixture("experiment_1.csv")
  ))

  expect_equal(sort(unique(result$data$source_file)), c("experiment_1.csv", "valid.csv"))
  expect_equal(nrow(result$data), 5)

  expect_equal(nrow(result$errors), 1)
  expect_equal(result$errors$source_file, "missing_well_averages_section.csv")
  expect_match(result$errors$message, "Well Averages")
})

test_that("every file fails: data is empty with the right columns, all errors reported", {
  result <- read_axion_mea_report(c(
    fixture("missing_well_averages_section.csv"),
    fixture("missing_mean_firing_rate.csv")
  ))

  expect_equal(nrow(result$data), 0)
  expect_equal(colnames(result$data), c(
    "source_file", "well", "mean_firing_rate_hz", "number_of_bursts"
  ))
  expect_equal(nrow(result$errors), 2)
  expect_equal(result$errors$source_file, c(
    "missing_well_averages_section.csv", "missing_mean_firing_rate.csv"
  ))
})

test_that("display_names overrides source_file provenance for both success and failure", {
  result <- read_axion_mea_report(
    c(fixture("experiment_1.csv"), fixture("missing_well_averages_section.csv")),
    display_names = c("Plate 3 - 2026-05-05.csv", "Plate 4 - bad export.csv")
  )

  expect_equal(result$data$source_file, rep("Plate 3 - 2026-05-05.csv", 2))
  expect_equal(result$errors$source_file, "Plate 4 - bad export.csv")
})

test_that("no files returns empty data and errors with the right columns", {
  result <- read_axion_mea_report(character(0))

  expect_equal(nrow(result$data), 0)
  expect_equal(colnames(result$data), c(
    "source_file", "well", "mean_firing_rate_hz", "number_of_bursts"
  ))
  expect_equal(nrow(result$errors), 0)
  expect_equal(colnames(result$errors), c("source_file", "message"))
})

test_that("mismatched paths and display_names raises an informative error", {
  expect_error(
    read_axion_mea_report(
      c(fixture("experiment_1.csv"), fixture("experiment_2.csv")),
      display_names = "only_one_name.csv"
    ),
    "different number",
    class = "mea_mismatched_upload_names"
  )
})
