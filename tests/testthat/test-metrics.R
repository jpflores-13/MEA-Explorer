sample_data <- tibble::tibble(
  well = c("A1", "A2", "A3", "A4", "A5", "A6", "A7"),
  treatment = c("drug_a", "drug_a", "drug_b", "drug_b", "drug_b", NA, NA),
  timepoint = c("t1", "t2", "t1", "t1", "t2", "t1", "t1"),
  mean_firing_rate_hz = c(0.1, 0.2, 0.3, 0.5, NA, 0.7, 0.9),
  number_of_bursts = c(1, 2, 3, 4, 5, 6, 7)
)

two_way_data <- tibble::tibble(
  well = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8"),
  treatment = c("drug_a", "drug_a", "drug_a", "drug_a", "drug_b", "drug_b", "drug_b", "drug_b"),
  timepoint = c("t1", "t1", "t2", "t2", "t1", "t1", "t2", "t2"),
  mean_firing_rate_hz = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
)

all_na_group_data <- tibble::tibble(
  well = c("C1", "C2", "C3", "C4"),
  treatment = c("ctrl", "ctrl", "treated", "treated"),
  mean_firing_rate_hz = c(NA_real_, NA_real_, 0.2, 0.4)
)

test_that("no grouping summarizes the whole dataset as a single group", {
  result <- summarize_mea(sample_data, "mean_firing_rate_hz")

  present <- c(0.1, 0.2, 0.3, 0.5, 0.7, 0.9)

  expect_equal(colnames(result), c("n", "n_missing", "mean", "sd", "sem", "median"))
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 6)
  expect_equal(result$n_missing, 1)
  expect_equal(result$mean, mean(present))
  expect_equal(result$sd, sd(present))
  expect_equal(result$sem, sd(present) / sqrt(6))
  expect_equal(result$median, median(present))
})

test_that("grouping by one column computes correct per-group values", {
  result <- summarize_mea(sample_data, "mean_firing_rate_hz", group_cols = "treatment")

  expect_equal(colnames(result), c(
    "treatment", "n", "n_missing", "mean", "sd", "sem", "median"
  ))
  expect_equal(nrow(result), 3)

  drug_a <- result[result$treatment %in% "drug_a", ]
  expect_equal(drug_a$n, 2)
  expect_equal(drug_a$n_missing, 0)
  expect_equal(drug_a$mean, mean(c(0.1, 0.2)))
  expect_equal(drug_a$sd, sd(c(0.1, 0.2)))
  expect_equal(drug_a$sem, sd(c(0.1, 0.2)) / sqrt(2))
  expect_equal(drug_a$median, median(c(0.1, 0.2)))

  drug_b <- result[result$treatment %in% "drug_b", ]
  expect_equal(drug_b$n, 2)
  expect_equal(drug_b$n_missing, 1)
  expect_equal(drug_b$mean, mean(c(0.3, 0.5)))
  expect_equal(drug_b$sd, sd(c(0.3, 0.5)))
  expect_equal(drug_b$sem, sd(c(0.3, 0.5)) / sqrt(2))
  expect_equal(drug_b$median, median(c(0.3, 0.5)))
})

test_that("grouping by two columns produces correct multi-key groups", {
  result <- summarize_mea(
    two_way_data, "mean_firing_rate_hz",
    group_cols = c("treatment", "timepoint")
  )

  expect_equal(colnames(result), c(
    "treatment", "timepoint", "n", "n_missing", "mean", "sd", "sem", "median"
  ))
  expect_equal(nrow(result), 4)

  cell <- function(trt, tp) result[result$treatment == trt & result$timepoint == tp, ]

  a_t1 <- cell("drug_a", "t1")
  expect_equal(a_t1$n, 2)
  expect_equal(a_t1$mean, mean(c(0.1, 0.2)))

  a_t2 <- cell("drug_a", "t2")
  expect_equal(a_t2$n, 2)
  expect_equal(a_t2$mean, mean(c(0.3, 0.4)))

  b_t1 <- cell("drug_b", "t1")
  expect_equal(b_t1$n, 2)
  expect_equal(b_t1$mean, mean(c(0.5, 0.6)))

  b_t2 <- cell("drug_b", "t2")
  expect_equal(b_t2$n, 2)
  expect_equal(b_t2$mean, mean(c(0.7, 0.8)))
})

test_that("an NA metric value is excluded from stats but counted as missing", {
  result <- summarize_mea(sample_data, "mean_firing_rate_hz", group_cols = "treatment")
  drug_b <- result[result$treatment %in% "drug_b", ]

  expect_equal(drug_b$n, 2)
  expect_equal(drug_b$n_missing, 1)
  expect_equal(drug_b$mean, mean(c(0.3, 0.5)))
})

test_that("a group whose metric is entirely NA reports NA_real_, never NaN or 0", {
  result <- summarize_mea(all_na_group_data, "mean_firing_rate_hz", group_cols = "treatment")
  ctrl <- result[result$treatment %in% "ctrl", ]

  expect_equal(ctrl$n, 0)
  expect_equal(ctrl$n_missing, 2)

  expect_true(is.na(ctrl$mean))
  expect_false(is.nan(ctrl$mean))
  expect_true(is.na(ctrl$sd))
  expect_false(is.nan(ctrl$sd))
  expect_true(is.na(ctrl$sem))
  expect_false(is.nan(ctrl$sem))
  expect_true(is.na(ctrl$median))
  expect_false(is.nan(ctrl$median))

  treated <- result[result$treatment %in% "treated", ]
  expect_equal(treated$n, 2)
  expect_equal(treated$mean, mean(c(0.2, 0.4)))
})

test_that("an NA group_cols value forms its own visible group, not dropped", {
  result <- summarize_mea(sample_data, "mean_firing_rate_hz", group_cols = "treatment")

  expect_equal(nrow(result), 3)
  na_group <- result[is.na(result$treatment), ]
  expect_equal(nrow(na_group), 1)
  expect_equal(na_group$n, 2)
  expect_equal(na_group$n_missing, 0)
  expect_equal(na_group$mean, mean(c(0.7, 0.9)))
})

test_that("group_cols order and names are preserved as leading columns", {
  result <- summarize_mea(
    two_way_data, "mean_firing_rate_hz",
    group_cols = c("timepoint", "treatment")
  )

  expect_equal(colnames(result)[1:2], c("timepoint", "treatment"))
})

test_that("a missing metric column is rejected via require_columns", {
  expect_error(
    summarize_mea(sample_data, "does_not_exist"),
    class = "mea_missing_columns"
  )
})

test_that("a missing group_cols column is rejected via require_columns", {
  expect_error(
    summarize_mea(sample_data, "mean_firing_rate_hz", group_cols = "not_a_column"),
    class = "mea_missing_columns"
  )
})

test_that("a non-numeric metric column is rejected with a dedicated error", {
  expect_error(
    summarize_mea(sample_data, "well"),
    "not a numeric column",
    class = "mea_non_numeric_metric"
  )
})
