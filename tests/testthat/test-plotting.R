well_data <- tibble::tibble(
  source_file = c("exp1.csv", "exp1.csv", "exp1.csv", "exp1.csv"),
  well = c("A1", "A2", "B1", "B2"),
  treatment = c("drug_x", "drug_x", "vehicle", "vehicle"),
  biological_replicate = c("rep1", "rep2", "rep1", "rep2"),
  mean_firing_rate_hz = c(0.1, 0.2, 0.3, 0.4),
  number_of_bursts = c(1, 2, 3, 4)
)

time_course_data <- tibble::tibble(
  source_file = rep(c("exp1.csv", "exp2.csv"), each = 4),
  well = rep(c("A1", "A2"), times = 4),
  treatment = rep(c("drug_x", "vehicle"), each = 4),
  biological_replicate = rep(c("rep1", "rep2"), times = 4),
  timepoint = rep(c(0, 1), each = 2, times = 2),
  mean_firing_rate_hz = c(0.1, 0.2, 0.15, 0.25, 0.3, 0.4, 0.35, 0.45),
  number_of_bursts = c(1, 2, 3, 4, 5, 6, 7, 8)
)

test_that("plot_well_values returns a ggplot with one point per row", {
  result <- plot_well_values(well_data, "mean_firing_rate_hz")

  expect_s3_class(result, "ggplot")
  built <- ggplot2::layer_data(result)
  expect_equal(nrow(built), nrow(well_data))
  expect_equal(sort(built$y), sort(well_data$mean_firing_rate_hz))
})

test_that("plot_well_values colors by treatment when present", {
  result <- plot_well_values(well_data, "mean_firing_rate_hz")
  expect_true("colour" %in% names(result$mapping) || "colour" %in% names(result$layers[[1]]$mapping))
})

test_that("plot_well_values degrades gracefully with no metadata columns at all", {
  bare_data <- tibble::tibble(
    source_file = c("exp1.csv", "exp1.csv"),
    well = c("A1", "A2"),
    mean_firing_rate_hz = c(0.1, 0.2),
    number_of_bursts = c(1, 2)
  )

  result <- plot_well_values(bare_data, "mean_firing_rate_hz")

  expect_s3_class(result, "ggplot")
  built <- ggplot2::layer_data(result)
  expect_equal(nrow(built), 2)
})

test_that("plot_well_values keeps the data's well order rather than resorting", {
  reordered <- well_data[c(4, 3, 2, 1), ] # deliberately not alphabetical: B2, B1, A2, A1

  result <- plot_well_values(reordered, "mean_firing_rate_hz")

  expect_equal(levels(result$data$well), reordered$well)
})

test_that("plot_well_values errors on a missing metric column", {
  expect_error(
    plot_well_values(well_data, "not_a_column"),
    class = "mea_missing_columns"
  )
})

test_that("plot_group_distribution returns a boxplot layered with jittered points", {
  result <- plot_group_distribution(well_data, "mean_firing_rate_hz", "treatment")

  expect_s3_class(result, "ggplot")
  expect_true(length(result$layers) >= 2)
  layer_classes <- vapply(result$layers, function(l) class(l$geom)[[1]], character(1))
  expect_true(any(grepl("Boxplot", layer_classes)))
  expect_true(any(grepl("Point", layer_classes)))
})

test_that("plot_group_distribution errors when the group column is missing", {
  expect_error(
    plot_group_distribution(well_data, "mean_firing_rate_hz", "not_a_column"),
    class = "mea_missing_columns"
  )
})

test_that("plot_replicate_structure plots every observation without averaging", {
  result <- plot_replicate_structure(well_data, "mean_firing_rate_hz", "treatment")

  expect_s3_class(result, "ggplot")
  built <- ggplot2::layer_data(result)
  expect_equal(nrow(built), nrow(well_data))
  expect_equal(sort(built$y), sort(well_data$mean_firing_rate_hz))
})

test_that("plot_replicate_structure errors when replicate_col is missing", {
  no_replicate <- dplyr::select(well_data, -biological_replicate)

  expect_error(
    plot_replicate_structure(no_replicate, "mean_firing_rate_hz", "treatment"),
    class = "mea_missing_columns"
  )
})

test_that("plot_time_course plots individual points and per-well trajectories", {
  result <- plot_time_course(time_course_data, "mean_firing_rate_hz", group = "treatment")

  expect_s3_class(result, "ggplot")
  point_layer <- which(vapply(result$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
  expect_true(length(point_layer) >= 1)
  built_points <- ggplot2::layer_data(result, point_layer[[1]])
  expect_equal(nrow(built_points), nrow(time_course_data))
})

test_that("plot_time_course errors when there is no timepoint column", {
  no_timepoint <- dplyr::select(time_course_data, -timepoint)

  expect_error(
    plot_time_course(no_timepoint, "mean_firing_rate_hz"),
    "no timepoint metadata",
    class = "mea_missing_timepoint"
  )
})

test_that("plot_time_course errors when timepoint is entirely NA", {
  all_na_timepoint <- time_course_data
  all_na_timepoint$timepoint <- NA_real_

  expect_error(
    plot_time_course(all_na_timepoint, "mean_firing_rate_hz"),
    "no timepoint metadata",
    class = "mea_missing_timepoint"
  )
})

test_that("plot_time_course_summary draws one mean point per (timepoint, group)", {
  result <- plot_time_course_summary(time_course_data, "mean_firing_rate_hz", group = "treatment")

  expect_s3_class(result, "ggplot")
  point_layers <- which(vapply(result$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
  expect_equal(length(point_layers), 2) # individual wells + per-group means

  raw_points <- ggplot2::layer_data(result, point_layers[[1]])
  expect_equal(nrow(raw_points), nrow(time_course_data))

  mean_points <- ggplot2::layer_data(result, point_layers[[2]])
  expect_equal(nrow(mean_points), 4) # 2 timepoints x 2 treatments
})

test_that("plot_time_course_summary's mean matches a manual calculation", {
  result <- plot_time_course_summary(time_course_data, "mean_firing_rate_hz", group = "treatment")

  point_layers <- which(vapply(result$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
  mean_points <- ggplot2::layer_data(result, point_layers[[2]])

  expected_mean <- mean(time_course_data$mean_firing_rate_hz[
    time_course_data$timepoint == 0 & time_course_data$treatment == "drug_x"
  ])
  expect_true(any(abs(mean_points$y - expected_mean) < 1e-9))
})

test_that("plot_time_course_summary draws one series with no group given", {
  no_group <- dplyr::select(time_course_data, -treatment)
  result <- plot_time_course_summary(no_group, "mean_firing_rate_hz")

  expect_s3_class(result, "ggplot")
  point_layers <- which(vapply(result$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))
  mean_points <- ggplot2::layer_data(result, point_layers[[2]])
  expect_equal(nrow(mean_points), 2) # 2 timepoints, 1 series
})

test_that("plot_time_course_summary errors when there is no timepoint column", {
  no_timepoint <- dplyr::select(time_course_data, -timepoint)

  expect_error(
    plot_time_course_summary(no_timepoint, "mean_firing_rate_hz"),
    class = "mea_missing_columns"
  )
})

test_that("plot_time_course_summary errors when timepoint is entirely NA", {
  all_na_timepoint <- time_course_data
  all_na_timepoint$timepoint <- NA_real_

  expect_error(
    plot_time_course_summary(all_na_timepoint, "mean_firing_rate_hz"),
    "no timepoint metadata",
    class = "mea_missing_timepoint"
  )
})

test_that("plot_time_course_summary errors on a non-numeric timepoint value", {
  bad_timepoint <- time_course_data
  bad_timepoint$timepoint <- as.character(bad_timepoint$timepoint)
  bad_timepoint$timepoint[1] <- "forty"

  expect_error(
    plot_time_course_summary(bad_timepoint, "mean_firing_rate_hz"),
    "could not interpret timepoint",
    class = "mea_invalid_timepoint"
  )
})

test_that("plot_plate_heatmap draws one tile per well", {
  plate_data <- tibble::tibble(
    well = c("A1", "A2", "B1", "B2"),
    mean_firing_rate_hz = c(0.1, 0.2, 0.3, 0.4)
  )

  result <- plot_plate_heatmap(plate_data, "mean_firing_rate_hz")

  expect_s3_class(result, "ggplot")
  built <- ggplot2::layer_data(result)
  expect_equal(nrow(built), nrow(plate_data))
})

test_that("plot_plate_heatmap orders rows top-to-bottom and columns left-to-right", {
  plate_data <- tibble::tibble(
    well = c("B2", "A1", "A2", "B1"),
    mean_firing_rate_hz = c(0.4, 0.1, 0.2, 0.3)
  )

  result <- plot_plate_heatmap(plate_data, "mean_firing_rate_hz")

  expect_equal(levels(result$data$row_letter), c("B", "A"))
  expect_equal(levels(result$data$col_number), c("1", "2"))
})

test_that("plot_plate_heatmap rejects a malformed well identifier", {
  bad_data <- tibble::tibble(
    well = c("A1", "1A"),
    mean_firing_rate_hz = c(0.1, 0.2)
  )

  expect_error(
    plot_plate_heatmap(bad_data, "mean_firing_rate_hz"),
    "could not parse well identifier",
    class = "mea_invalid_well_id"
  )
})

test_that("plot_plate_heatmap rejects a well id with no numeric suffix", {
  bad_data <- tibble::tibble(
    well = c("A1", "Z"),
    mean_firing_rate_hz = c(0.1, 0.2)
  )

  expect_error(
    plot_plate_heatmap(bad_data, "mean_firing_rate_hz"),
    class = "mea_invalid_well_id"
  )
})

test_that("plot_plate_heatmap rejects duplicated wells rather than averaging", {
  dup_data <- tibble::tibble(
    well = c("A1", "A1", "A2"),
    mean_firing_rate_hz = c(0.1, 0.2, 0.3)
  )

  expect_error(
    plot_plate_heatmap(dup_data, "mean_firing_rate_hz"),
    "more than one row",
    class = "mea_ambiguous_heatmap_well"
  )
})
