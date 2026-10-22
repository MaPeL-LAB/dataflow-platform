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

testthat::test_that("one RDS input remains unchanged and writes only requested privacy-safe outputs", {
  temp_root <- tempfile(pattern = "rds-output-isolation-")
  input_dir <- file.path(temp_root, "input")
  output_dir <- file.path(temp_root, "requested-output")
  dir.create(input_dir, recursive = TRUE)
  input_path <- file.path(input_dir, "synthetic_input.rds")
  sentinel <- "generated_private_row_value_"
  saveRDS(
    data.frame(
      record_id = sprintf("R%03d", seq_len(12L)),
      group = rep(c("A", "B"), 6L),
      measure = seq(0.125, 1.5, by = 0.125),
      free_text = paste0(sentinel, sprintf("%03d", seq_len(12L))),
      stringsAsFactors = FALSE
    ),
    input_path,
    version = 2
  )

  input_inventory_before <- list.files(input_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  input_hash_before <- unname(tools::md5sum(input_path))
  input_mtime_before <- file.info(input_path)$mtime

  result <- run_data_dictionary_pipeline(
    inputs = input_path,
    output_dir = output_dir,
    default_config_path = default_config_path,
    overrides = list(
      privacy = list(include_examples = FALSE),
      output = list(write_excel = FALSE, overwrite = FALSE)
    )
  )

  testthat::expect_equal(result$results$summary$datasets, 1L)
  testthat::expect_equal(result$results$dataset_metadata$source_format, "rds")
  testthat::expect_false(result$config$privacy$include_examples)
  testthat::expect_true(all(is.na(result$results$variable_metadata$examples)))
  testthat::expect_equal(unname(tools::md5sum(input_path)), input_hash_before)
  testthat::expect_equal(file.info(input_path)$mtime, input_mtime_before)
  testthat::expect_equal(
    list.files(input_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    input_inventory_before
  )

  normalized_output <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  normalized_artifacts <- normalizePath(result$artifacts, winslash = "/", mustWork = TRUE)
  testthat::expect_true(all(startsWith(normalized_artifacts, paste0(normalized_output, "/"))))

  text_artifacts <- normalized_artifacts[grepl("\\.(csv|html|json|ya?ml)$", normalized_artifacts, ignore.case = TRUE)]
  artifact_text <- paste(vapply(
    text_artifacts,
    function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    character(1L)
  ), collapse = "\n")
  testthat::expect_false(grepl(sentinel, artifact_text, fixed = TRUE))
})
