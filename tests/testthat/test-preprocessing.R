sample_data <- tibble::tibble(
  source_file = c("exp1.csv", "exp1.csv", "exp2.csv", "exp2.csv"),
  well = c("A1", "A2", "A1", "A2"),
  mean_firing_rate_hz = c(0.1, 0.2, 0.3, 0.4),
  number_of_bursts = c(1, 2, 3, 4)
)

test_that("initialize_metadata scaffolds one NA row per well, nothing guessed", {
  scaffold <- initialize_metadata(sample_data)

  expect_equal(nrow(scaffold), 4)
  expect_equal(colnames(scaffold), c(
    "source_file", "well", "experiment_id", "treatment", "timepoint",
    "biological_replicate"
  ))
  expect_true(all(is.na(scaffold$experiment_id)))
  expect_true(all(is.na(scaffold$treatment)))
  expect_true(all(is.na(scaffold$timepoint)))
  expect_true(all(is.na(scaffold$biological_replicate)))
})

test_that("applying an all-NA scaffold leaves measurements untouched", {
  result <- apply_metadata(sample_data, initialize_metadata(sample_data))

  expect_equal(colnames(result), c(
    "source_file", "experiment_id", "well", "treatment", "timepoint",
    "biological_replicate", "mean_firing_rate_hz", "number_of_bursts"
  ))
  expect_equal(result$mean_firing_rate_hz, sample_data$mean_firing_rate_hz)
  expect_equal(result$number_of_bursts, sample_data$number_of_bursts)
  expect_true(all(is.na(result$treatment)))
})

test_that("metadata assigned to a subset of wells leaves the rest NA", {
  metadata <- tibble::tibble(
    source_file = "exp1.csv",
    well = "A1",
    treatment = "drug_x"
  )

  result <- apply_metadata(sample_data, metadata)

  expect_equal(
    result$treatment,
    c("drug_x", NA, NA, NA)
  )
  expect_true(all(is.na(result$experiment_id)))
})

test_that("metadata join respects source_file, not just well identifier", {
  # exp1/A1 and exp2/A1 must be distinguishable even though the well
  # identifier "A1" is shared across both source files.
  metadata <- tibble::tibble(
    source_file = c("exp1.csv", "exp2.csv"),
    well = c("A1", "A1"),
    experiment_id = c("run_1", "run_2")
  )

  result <- apply_metadata(sample_data, metadata)

  expect_equal(
    result$experiment_id[result$source_file == "exp1.csv" & result$well == "A1"],
    "run_1"
  )
  expect_equal(
    result$experiment_id[result$source_file == "exp2.csv" & result$well == "A1"],
    "run_2"
  )
})

test_that("a metadata column not supplied by the user is added as NA, not an error", {
  metadata <- tibble::tibble(
    source_file = "exp1.csv",
    well = "A1",
    treatment = "drug_x"
  )

  result <- apply_metadata(sample_data, metadata)

  expect_true("timepoint" %in% colnames(result))
  expect_true("biological_replicate" %in% colnames(result))
  expect_true("experiment_id" %in% colnames(result))
})

test_that("duplicate metadata rows for the same well are ambiguous, not silently merged", {
  metadata <- tibble::tibble(
    source_file = c("exp1.csv", "exp1.csv"),
    well = c("A1", "A1"),
    treatment = c("drug_x", "drug_y")
  )

  expect_error(
    apply_metadata(sample_data, metadata),
    "more than one metadata row",
    class = "mea_ambiguous_metadata"
  )
})

test_that("metadata referencing a well absent from the data is rejected", {
  metadata <- tibble::tibble(
    source_file = "exp1.csv",
    well = "Z9",
    treatment = "drug_x"
  )

  expect_error(
    apply_metadata(sample_data, metadata),
    "do not appear in the parsed data",
    class = "mea_orphaned_metadata"
  )
})

test_that("applying metadata twice to already-joined data is rejected", {
  once <- apply_metadata(sample_data, initialize_metadata(sample_data))

  expect_error(
    apply_metadata(once, initialize_metadata(sample_data)),
    "apply_metadata\\(\\)",
    class = "mea_metadata_already_applied"
  )
})
