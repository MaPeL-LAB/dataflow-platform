source(file.path(testthat::test_path(), "..", "fixtures", "dataminima_fixture.R"))

read_handoff_json <- function(output, name) {
  jsonlite::fromJSON(file.path(output, "dataminima_handoff", name), simplifyVector = FALSE)
}

quality_issues_matching <- function(quality, issue_type, dataset_id = NULL, variable_name = NULL) {
  Filter(function(issue) {
    identical(issue$issue_type, issue_type) &&
      (is.null(dataset_id) || identical(issue$dataset_id, dataset_id)) &&
      (is.null(variable_name) || identical(issue$variable_name, variable_name))
  }, quality$issues)
}

testthat::test_that("DataMinima fixture produces an exact qualified-shape six-artifact bundle", {
  root <- tempfile("dataminima-fixture-")
  output <- tempfile("dataminima-output-")
  run_dataminima_fixture(root, output, default_config_path)

  bundle <- file.path(output, "dataminima_handoff.zip")
  expected <- sort(c(DATAMINIMA_PAYLOAD_ARTIFACTS, "artifact_manifest.csv"))
  testthat::expect_equal(sort(utils::unzip(bundle, list = TRUE)$Name), expected)

  dictionary <- read_handoff_json(output, "data_dictionary.json")
  profile <- read_handoff_json(output, "statistical_profile.json")
  generation <- jsonlite::fromJSON(file.path(output, "dataminima_handoff", "generation_profile.yml"), simplifyVector = FALSE)
  quality <- read_handoff_json(output, "quality_baseline.json")
  categories <- utils::read.csv(file.path(output, "dataminima_handoff", "categorical_levels.csv"), stringsAsFactors = FALSE)
  manifest <- utils::read.csv(file.path(output, "dataminima_handoff", "artifact_manifest.csv"), stringsAsFactors = FALSE)

  testthat::expect_equal(dictionary$source$product, "dataflow")
  testthat::expect_equal(dictionary$source$product_version, "0.2.0")
  testthat::expect_equal(dictionary$source$pipeline_version, "0.2.0")
  testthat::expect_equal(dictionary$source$run_id, "run_fixture_20260813")
  testthat::expect_match(dictionary$source$configuration_sha256, "^[a-f0-9]{64}$")
  testthat::expect_equal(profile$tables$people$fields$age$distribution$parameters$maximum, 999)
  testthat::expect_equal(profile$tables$events$fields$score$missingness$rate, 0.25)
  testthat::expect_equal(profile$tables$events$fields$score$missingness$conditional_on, list("phase"))
  testthat::expect_equal(profile$tables$events$relationships[[1L]]$foreign_key_coverage, 1)
  testthat::expect_equal(profile$tables$events$relationships[[1L]]$parent_unique_rate, 1)
  testthat::expect_setequal(
    vapply(profile$tables$events$dependencies, `[[`, character(1L), "method"),
    c("pearson", "cramers_v", "conditional_frequency")
  )
  testthat::expect_equal(profile$tables$events$temporal[[1L]]$interval_distribution$family, "empirical_intervals_days_v1")
  testthat::expect_equal(generation$review_status, "approved")
  testthat::expect_equal(generation$tables$events$temporal[[1L]]$interval_unit, "days")
  testthat::expect_false("secret_note" %in% names(generation$tables$people$fields))
  testthat::expect_false(any(categories$raw_value %in% c("rare_one", "rare_two")))
  testthat::expect_null(generation$tables$people$fields$risk_band$category_probabilities)
  testthat::expect_true(all(c("rare_levels_suppressed", "invalid_values", "aggregate_disclosure_review") %in%
    vapply(quality$issues, `[[`, character(1L), "issue_type")))
  testthat::expect_equal(nrow(manifest), 5L)
  testthat::expect_true(all(nchar(manifest$sha256) == 64L))
  testthat::expect_equal(length(unique(manifest$configuration_sha256)), 1L)

  bundled_text <- paste(vapply(
    DATAMINIMA_PAYLOAD_ARTIFACTS,
    function(name) paste(readLines(file.path(output, "dataminima_handoff", name), warn = FALSE), collapse = "\n"),
    character(1L)
  ), collapse = "\n")
  testthat::expect_false(grepl(normalizePath(root, winslash = "/", mustWork = TRUE), bundled_text, fixed = TRUE))
  testthat::expect_false(grepl("private_fixture_value_", bundled_text, fixed = TRUE))
  testthat::expect_false(grepl("source_row_marker_must_not_ship", bundled_text, fixed = TRUE))
  review <- jsonlite::fromJSON(file.path(output, "dataminima_disclosure_review.json"), simplifyVector = FALSE)
  testthat::expect_equal(review$decision, "approved")
})

testthat::test_that("quality baseline records every applicable DFL-PRO-006 finding", {
  root <- tempfile("dataminima-quality-fixture-")
  output <- tempfile("dataminima-quality-output-")
  run_dataminima_fixture(root, output, default_config_path)
  quality <- read_handoff_json(output, "quality_baseline.json")

  constants <- quality_issues_matching(quality, "constant", "quality_cases", "constant_marker")
  testthat::expect_length(constants, 1L)
  testthat::expect_equal(constants[[1L]]$metric, 1)

  duplicates <- quality_issues_matching(quality, "duplicate_rows", "quality_cases")
  testthat::expect_length(duplicates, 1L)
  testthat::expect_equal(duplicates[[1L]]$metric, 1)

  parsing <- quality_issues_matching(quality, "import_parsing_problems", "parser_cases")
  testthat::expect_length(parsing, 1L)
  testthat::expect_equal(parsing[[1L]]$metric, 1)

  documentation <- quality_issues_matching(
    quality, "missing_business_definition", "quality_cases", "constant_marker"
  )
  testthat::expect_length(documentation, 1L)

  rare_levels <- quality_issues_matching(quality, "rare_levels_suppressed", "people", "risk_band")
  testthat::expect_length(rare_levels, 1L)
  testthat::expect_equal(rare_levels[[1L]]$metric, 2)
  testthat::expect_equal(rare_levels[[1L]]$threshold, 5)

  invalid_values <- quality_issues_matching(quality, "invalid_values", "people", "age")
  testthat::expect_length(invalid_values, 1L)
  testthat::expect_equal(invalid_values[[1L]]$metric, 1)
  testthat::expect_equal(invalid_values[[1L]]$threshold, 0)
})

testthat::test_that("numeric statistical parameters obey DFL-SEC-002 aggregate precision", {
  root <- tempfile("dataminima-precision-fixture-")
  output <- tempfile("dataminima-precision-output-")
  overrides <- dataminima_fixture_overrides()
  overrides$dataminima_handoff$aggregate_privacy$precision_digits <- 3L
  run_dataminima_fixture(root, output, default_config_path, overrides = overrides)

  profile <- read_handoff_json(output, "statistical_profile.json")
  parameters <- profile$tables$people$fields$age$distribution$parameters
  raw_age <- c(21.123456789, 22:39, 999)
  testthat::expect_equal(parameters$mean, round(mean(raw_age), 3L))
  testthat::expect_equal(parameters$minimum, round(min(raw_age), 3L))
  testthat::expect_equal(parameters$maximum, round(max(raw_age), 3L))
  testthat::expect_equal(parameters$sd, round(stats::sd(raw_age), 3L))
  testthat::expect_false(identical(parameters$minimum, min(raw_age)))
})

testthat::test_that("DataMinima output is deterministic for fixed run metadata and configuration", {
  root <- tempfile("dataminima-repeat-fixture-")
  output_one <- tempfile("dataminima-repeat-one-")
  output_two <- tempfile("dataminima-repeat-two-")
  overrides <- dataminima_fixture_overrides()
  run_dataminima_fixture(root, output_one, default_config_path)
  run_dataminima_fixture(root, output_two, default_config_path)

  artifacts <- c(DATAMINIMA_PAYLOAD_ARTIFACTS, "artifact_manifest.csv")
  digests <- function(output) vapply(
    artifacts,
    function(name) handoff_sha256_file(file.path(output, "dataminima_handoff", name)),
    character(1L)
  )
  testthat::expect_equal(digests(output_one), digests(output_two))
  testthat::expect_equal(handoff_sha256_file(file.path(output_one, "dataminima_handoff.zip")),
                         handoff_sha256_file(file.path(output_two, "dataminima_handoff.zip")))

  changed <- overrides
  changed$dataminima_handoff$aggregate_privacy$precision_digits <- 5L
  config_one <- load_data_dictionary_config(default_config_path, overrides = overrides)
  config_two <- load_data_dictionary_config(default_config_path, overrides = changed)
  testthat::expect_false(identical(
    handoff_sha256_raw(charToRaw(handoff_canonical_configuration(config_one))),
    handoff_sha256_raw(charToRaw(handoff_canonical_configuration(config_two)))
  ))
})

testthat::test_that("approved handoffs fail closed without disclosure review evidence", {
  overrides <- dataminima_fixture_overrides()
  overrides$dataminima_handoff$aggregate_privacy$review <- list(
    reviewer = NULL, reviewed_at = NULL, decision = "pending",
    findings = list(), residual_limitations = list()
  )
  testthat::expect_error(
    load_data_dictionary_config(default_config_path, overrides = overrides),
    "requires an approved aggregate disclosure-risk review"
  )
})

testthat::test_that("unknown handoff configuration fields fail closed", {
  testthat::expect_error(
    load_data_dictionary_config(
      default_config_path,
      overrides = list(dataminima_handoff = list(unknown_contract_field = TRUE))
    ),
    "unknown field"
  )
})

testthat::test_that("actual DataMinima qualifier accepts producer fixture output when available", {
  workspace_root <- dirname(dirname(repo_root))
  dataminima_root <- Sys.getenv(
    "DATAMINIMA_ROOT",
    file.path(workspace_root, "research-ai-products", "synthetic-data-platform")
  )
  python <- if (.Platform$OS.type == "windows") {
    file.path(dataminima_root, ".venv", "Scripts", "python.exe")
  } else {
    file.path(dataminima_root, ".venv", "bin", "python")
  }
  testthat::skip_if_not(file.exists(python), "DataMinima qualifier environment is not available in this checkout.")
  testthat::skip_if_not(file.exists(file.path(dataminima_root, "src", "metadata_synthetic_data_platform", "dataflow_qualification.py")))

  root <- tempfile("dataminima-real-qualifier-fixture-")
  output <- tempfile("dataminima-real-qualifier-output-")
  run_dataminima_fixture(root, output, default_config_path)
  report_path <- file.path(output, "dataflow_qualification.json")
  command_output <- system2(
    python,
    c(
      "-m", "metadata_synthetic_data_platform.cli", "qualify-dataflow-bundle",
      "--bundle", shQuote(file.path(output, "dataminima_handoff.zip")),
      "--output", shQuote(report_path)
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("PYTHONPATH=", shQuote(file.path(dataminima_root, "src")))
  )
  testthat::expect_equal(attr(command_output, "status") %||% 0L, 0L, info = paste(command_output, collapse = "\n"))
  report <- jsonlite::fromJSON(report_path, simplifyVector = FALSE)
  testthat::expect_equal(report$overall_status, "passed")
  testthat::expect_equal(length(report$artifacts), 6L)
  testthat::expect_true(all(vapply(report$artifacts, `[[`, logical(1L), "byte_equivalent")))
  testthat::expect_true(all(vapply(report$artifacts, `[[`, logical(1L), "canonical_equivalent")))
})
