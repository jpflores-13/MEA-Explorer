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

test_that("read_recording_name reads the header field, including with a UTF-8 BOM", {
  # This fixture is a real Axion export copied verbatim, including the BOM
  # AxIS wrote before "Investigator:" on the line above Recording Name.
  expect_equal(read_recording_name(fixture("recording_name_d12.csv")), "20260409 KOLF EGC D12")
  expect_equal(read_recording_name(fixture("valid.csv")), "Synthetic Valid Export")
})

test_that("read_recording_name returns NA rather than erroring when the file is missing", {
  expect_true(is.na(read_recording_name(fixture("does_not_exist.csv"))))
})

test_that("read_recording_name returns NA when there is no Recording Name line at all", {
  no_header <- tempfile(fileext = ".csv")
  writeLines(c("Well Averages,A1", "Mean Firing Rate (Hz),0.1"), no_header)
  expect_true(is.na(read_recording_name(no_header)))
})

test_that("infer_timepoint_days extracts a trailing d<N> token from real recording names", {
  expect_equal(infer_timepoint_days("20260409 KOLF EGC D12"), 12L)
  expect_equal(infer_timepoint_days("20260411 KOLF EGC BUMP D16"), 16L)
  expect_equal(infer_timepoint_days("20260413 KOLFEGC BUMP d18"), 18L)
  expect_equal(infer_timepoint_days("20260505 KOLF EGC BUMP d40"), 40L)
})

test_that("infer_timepoint_days handles day/underscore variants and returns NA when no token is present", {
  expect_equal(infer_timepoint_days("KOLF EGC day 21"), 21L)
  expect_equal(infer_timepoint_days("KOLF EGC day_21"), 21L)
  expect_true(is.na(infer_timepoint_days("Synthetic Valid Export")))
  expect_true(is.na(infer_timepoint_days(NA_character_)))
})

test_that("infer_timepoint_days does not match a 'd<digits>' substring embedded inside a larger word", {
  # "Grid42": the "d" immediately preceding "42" is itself preceded by a
  # letter ("i"), so this must not be read as day 42.
  expect_true(is.na(infer_timepoint_days("KOLF EGC Grid42 test")))
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
