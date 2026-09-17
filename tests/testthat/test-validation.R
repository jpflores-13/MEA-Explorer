with_temp_csv <- function(lines, code) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  on.exit(unlink(path))
  code(path)
}

test_that("multiple Well Averages sections are treated as ambiguous, not first-match", {
  with_temp_csv(
    c(
      "Well Averages,A1,A2",
      "Mean Firing Rate (Hz),0.1,0.2",
      "Number of Bursts,1,2",
      "",
      "Well Averages,A1,A2",
      "Mean Firing Rate (Hz),0.3,0.4",
      "Number of Bursts,3,4"
    ),
    function(path) {
      expect_error(
        read_axion_well_averages(path),
        "multiple 'Well Averages' sections",
        class = "mea_ambiguous_well_averages_section"
      )
    }
  )
})

test_that("a duplicated measurement row within one section is ambiguous", {
  with_temp_csv(
    c(
      "Well Averages,A1,A2",
      "Mean Firing Rate (Hz),0.1,0.2",
      "Mean Firing Rate (Hz),0.3,0.4",
      "Number of Bursts,1,2"
    ),
    function(path) {
      expect_error(
        read_axion_well_averages(path),
        "multiple 'Mean Firing Rate \\(Hz\\)' rows",
        class = "mea_ambiguous_measurement"
      )
    }
  )
})

test_that("a Well Averages header with no well identifiers is rejected", {
  with_temp_csv(
    c(
      "Well Averages,",
      "Mean Firing Rate (Hz),",
      "Number of Bursts,"
    ),
    function(path) {
      expect_error(
        read_axion_well_averages(path),
        "no well identifiers",
        class = "mea_missing_well_identifiers"
      )
    }
  )
})

test_that("extract_section_lines stops at the first blank line", {
  lines <- c(
    "Well Averages,A1",
    "Mean Firing Rate (Hz),0.1",
    "",
    "Measurement,A1_11",
    "Mean Firing Rate (Hz),9.9"
  )
  section <- extract_section_lines(lines, header_index = 1)

  expect_equal(section, "Mean Firing Rate (Hz),0.1")
})

test_that("extract_section_lines handles the header being the last line", {
  expect_equal(extract_section_lines(c("Well Averages,A1"), header_index = 1), character(0))
})
