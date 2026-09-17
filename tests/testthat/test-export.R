single_group_data <- tibble::tibble(
  well = rep(c("A1", "A2"), each = 3),
  timepoint = rep(c(20, 22, 24), times = 2),
  mean_firing_rate_hz = c(0.01, 0.02, 0.03, 0.05, 0.06, 0.07)
)

two_group_data <- tibble::tibble(
  well = rep(c("A1", "A2"), each = 2),
  treatment = rep(c("ASD", "Control"), each = 2),
  timepoint = rep(c(20, 22), times = 2),
  mean_firing_rate_hz = c(0.01, 0.02, 0.05, 0.06)
)

test_that("pivots one column per well when no group is given", {
  result <- pivot_wide_for_prism(single_group_data, "mean_firing_rate_hz")

  expect_equal(colnames(result), c("timepoint", "A1", "A2"))
  expect_equal(result$timepoint, c(20, 22, 24))
  expect_equal(result$A1, c(0.01, 0.02, 0.03))
  expect_equal(result$A2, c(0.05, 0.06, 0.07))
})

test_that("does not disambiguate column names when only one group value is present", {
  one_group <- dplyr::mutate(single_group_data, treatment = "control")
  result <- pivot_wide_for_prism(one_group, "mean_firing_rate_hz", group = "treatment")

  expect_equal(colnames(result), c("timepoint", "A1", "A2"))
})

test_that("disambiguates columns by group when more than one group is present", {
  result <- pivot_wide_for_prism(two_group_data, "mean_firing_rate_hz", group = "treatment")

  expect_equal(sort(colnames(result)), sort(c("timepoint", "ASD - A1", "Control - A2")))
  expect_equal(result$timepoint, c(20, 22))
  expect_equal(result[["ASD - A1"]], c(0.01, 0.02))
  expect_equal(result[["Control - A2"]], c(0.05, 0.06))
})

test_that("a missing (well, timepoint) combination is NA, not dropped or zeroed", {
  sparse <- single_group_data[-1, ] # drop A1's value at timepoint 20

  result <- pivot_wide_for_prism(sparse, "mean_firing_rate_hz")

  expect_equal(nrow(result), 3)
  expect_true(is.na(result$A1[result$timepoint == 20]))
})

test_that("a row with unassigned (NA) timepoint is excluded, not errored on", {
  # Drop only A1's timepoint=20 row; A2 still has a value at timepoint 20,
  # so timepoint 20 survives in the result with A1's cell now NA.
  with_na <- single_group_data
  with_na$timepoint[1] <- NA

  result <- pivot_wide_for_prism(with_na, "mean_firing_rate_hz")

  expect_equal(nrow(result), 3)
  expect_true(is.na(result$A1[result$timepoint == 20]))
  expect_equal(result$A2[result$timepoint == 20], 0.05)
})

test_that("errors when no timepoint is assigned at all", {
  all_na <- single_group_data
  all_na$timepoint <- NA_real_

  expect_error(
    pivot_wide_for_prism(all_na, "mean_firing_rate_hz"),
    "no timepoint metadata",
    class = "mea_missing_timepoint"
  )
})

test_that("errors on a non-numeric timepoint value", {
  bad <- single_group_data
  bad$timepoint <- as.character(bad$timepoint)
  bad$timepoint[1] <- "twenty"

  expect_error(
    pivot_wide_for_prism(bad, "mean_firing_rate_hz"),
    "could not interpret timepoint",
    class = "mea_invalid_timepoint"
  )
})

test_that("errors rather than silently colliding two values at the same (well, timepoint)", {
  dup <- dplyr::bind_rows(single_group_data, single_group_data[1, ])

  expect_error(
    pivot_wide_for_prism(dup, "mean_firing_rate_hz"),
    "more than one value",
    class = "mea_ambiguous_prism_pivot"
  )
})

test_that("errors when required columns are missing", {
  expect_error(
    pivot_wide_for_prism(dplyr::select(single_group_data, -timepoint), "mean_firing_rate_hz"),
    class = "mea_missing_columns"
  )
})
