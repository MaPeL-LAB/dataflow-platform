testthat::test_that("CSV input produces separate dictionary and metadata products", {
  temp_input <- tempfile(fileext = ".csv")
  temp_output <- tempfile(pattern = "dictionary-output-")
  utils::write.csv(
    data.frame(
      name = paste0("Name ", 1:11),
      group = rep(c("A", "B"), length.out = 11),
      amount = seq(10, 110, by = 10),
      stringsAsFactors = FALSE
    ),
    temp_input,
    row.names = FALSE
  )

  result <- run_data_dictionary_pipeline(
    inputs = temp_input,
    output_dir = temp_output,
    default_config_path = default_config_path,
    overrides = list(
      metadata = list(
        project_name = "Local smoke study",
        run_comment = "Initial local dictionary run"
      ),
      output = list(write_excel = FALSE, overwrite = TRUE)
    )
  )

  testthat::expect_true(file.exists(file.path(temp_output, "data_dictionary.json")))
  testthat::expect_true(file.exists(file.path(temp_output, "metadata.json")))
  testthat::expect_true(file.exists(file.path(temp_output, "metadata_report.html")))
  testthat::expect_true(file.exists(file.path(temp_output, "open_science_metadata_report.html")))
  testthat::expect_true(file.exists(file.path(temp_output, "csv", "data_dictionary.csv")))
  testthat::expect_true(file.exists(file.path(temp_output, "csv", "variable_metadata.csv")))
  testthat::expect_true(file.exists(file.path(temp_output, "csv", "variable_dictionary.csv")))
  testthat::expect_equal(result$results$summary$datasets, 1)
  testthat::expect_equal(result$results$summary$variables, 3)
  testthat::expect_equal(nrow(result$results$data_dictionary), 3)
  testthat::expect_true(ncol(result$results$variable_metadata) > ncol(result$results$data_dictionary))
  testthat::expect_equal(result$results$run_metadata$project_name, "Local smoke study")
  testthat::expect_equal(result$results$run_metadata$run_comment, "Initial local dictionary run")
  report <- paste(readLines(file.path(temp_output, "metadata_report.html"), warn = FALSE), collapse = "\n")
  testthat::expect_match(report, "Local smoke study", fixed = TRUE)
  testthat::expect_match(report, "Initial local dictionary run", fixed = TRUE)

  public_report <- paste(
    readLines(file.path(temp_output, "open_science_metadata_report.html"), warn = FALSE),
    collapse = "\n"
  )
  testthat::expect_match(public_report, "Open Science Metadata Report", fixed = TRUE)
  testthat::expect_match(public_report, "Print / Save as PDF", fixed = TRUE)
  testthat::expect_match(public_report, "@media print", fixed = TRUE)
  testthat::expect_match(public_report, "Local smoke study", fixed = TRUE)
  testthat::expect_false(grepl(normalizePath(temp_input, winslash = "/", mustWork = TRUE), public_report, fixed = TRUE))
  testthat::expect_false(grepl("Initial local dictionary run", public_report, fixed = TRUE))
  testthat::expect_false(grepl("Representative Values", public_report, fixed = TRUE))
  testthat::expect_false(grepl(">NA<", public_report, fixed = TRUE))
})
