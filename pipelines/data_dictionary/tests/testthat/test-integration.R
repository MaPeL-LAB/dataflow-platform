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
    overrides = list(output = list(write_excel = FALSE, overwrite = TRUE))
  )

  testthat::expect_true(file.exists(file.path(temp_output, "data_dictionary.json")))
  testthat::expect_true(file.exists(file.path(temp_output, "metadata.json")))
  testthat::expect_true(file.exists(file.path(temp_output, "metadata_report.html")))
  testthat::expect_true(file.exists(file.path(temp_output, "csv", "data_dictionary.csv")))
  testthat::expect_true(file.exists(file.path(temp_output, "csv", "variable_metadata.csv")))
  testthat::expect_true(file.exists(file.path(temp_output, "csv", "variable_dictionary.csv")))
  testthat::expect_equal(result$results$summary$datasets, 1)
  testthat::expect_equal(result$results$summary$variables, 3)
  testthat::expect_equal(nrow(result$results$data_dictionary), 3)
  testthat::expect_true(ncol(result$results$variable_metadata) > ncol(result$results$data_dictionary))
})
