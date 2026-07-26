testthat::test_that("high-cardinality character variables are not categorical", {
  config <- test_config()
  data <- data.frame(
    person_name = sprintf("Person %02d", 1:12),
    status = rep(c("active", "inactive"), 6),
    score = seq_len(12),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  source_path <- tempfile(fileext = ".csv")
  file.create(source_path)
  record <- make_dataset_record(data, source_path, "csv", "people")
  record$dataset_id <- "people"
  record$file_metadata <- source_file_metadata(source_path, compute_md5 = FALSE, source_format = "csv")
  profile <- profile_dataset(record, config)

  name_row <- profile$variables[profile$variables$variable_name == "person_name", ]
  status_row <- profile$variables[profile$variables$variable_name == "status", ]

  testthat::expect_false(name_row$categorical_eligible)
  testthat::expect_equal(name_row$n_unique_non_missing, 12)
  testthat::expect_match(name_row$classification_reason, "exceeding the configured categorical limit of 10")
  testthat::expect_true(name_row$semantic_type %in% c("identifier", "high_cardinality_text", "free_text"))
  testthat::expect_equal(name_row$examples, "<masked>")
  testthat::expect_true(any(profile$issues$issue_type == "high_cardinality_character"))

  testthat::expect_true(status_row$categorical_eligible)
  testthat::expect_equal(status_row$semantic_type, "categorical")
})

testthat::test_that("explicit factors remain categorical above the character threshold", {
  config <- test_config()
  x <- factor(sprintf("L%02d", 1:20), levels = sprintf("L%02d", 1:20))
  stats <- profile_basic_stats(x, config)
  hints <- detect_role_hints(x, "explicit_factor", stats, config)
  classification <- classify_variable(x, "explicit_factor", stats, hints, config)
  testthat::expect_true(classification$categorical_eligible)
  testthat::expect_equal(classification$semantic_type, "categorical")
})

testthat::test_that("the character threshold is configurable", {
  config <- test_config(list(profiling = list(max_categorical_levels = 3)))
  x <- c("A", "B", "C", "D")
  stats <- profile_basic_stats(x, config)
  hints <- detect_role_hints(x, "group", stats, config)
  classification <- classify_variable(x, "group", stats, hints, config)
  testthat::expect_false(classification$categorical_eligible)
  testthat::expect_match(classification$classification_reason, "limit of 3")
})

testthat::test_that("the default threshold is inclusive at ten and excludes eleven", {
  config <- test_config()
  ten <- sprintf("V%02d", 1:10)
  eleven <- sprintf("V%02d", 1:11)

  ten_stats <- profile_basic_stats(ten, config)
  ten_class <- classify_variable(ten, "group", ten_stats, detect_role_hints(ten, "group", ten_stats, config), config)
  eleven_stats <- profile_basic_stats(eleven, config)
  eleven_class <- classify_variable(eleven, "group", eleven_stats, detect_role_hints(eleven, "group", eleven_stats, config), config)

  testthat::expect_true(ten_class$categorical_eligible)
  testthat::expect_false(eleven_class$categorical_eligible)
})


testthat::test_that("all-missing factors retain their explicit categorical type", {
  config <- test_config()
  x <- factor(c(NA_character_, NA_character_), levels = c("A", "B"))
  stats <- profile_basic_stats(x, config)
  hints <- detect_role_hints(x, "group", stats, config)
  classification <- classify_variable(x, "group", stats, hints, config)
  testthat::expect_true(classification$categorical_eligible)
  testthat::expect_equal(classification$semantic_type, "categorical")
  testthat::expect_equal(cardinality_class(stats, config), "all_missing")
})

testthat::test_that("all-missing plain character fields are not inferred as categorical", {
  config <- test_config()
  x <- c(NA_character_, "", "   ")
  stats <- profile_basic_stats(x, config)
  hints <- detect_role_hints(x, "comment", stats, config)
  classification <- classify_variable(x, "comment", stats, hints, config)
  testthat::expect_false(classification$categorical_eligible)
  testthat::expect_equal(classification$semantic_type, "text_all_missing")
})
