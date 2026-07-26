testthat::test_that("the compatibility dictionary keeps the analyst-facing core columns", {
  data <- data.frame(
    person_name = sprintf("Person %02d", 1:12),
    status = rep(c("active", "inactive"), 6),
    consent = rep(c(0, 1), 6),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(data$person_name, "label") <- "Participant name"
  attr(data$status, "label") <- "Current status"
  attr(data$consent, "label") <- "Consent response"
  attr(data$consent, "labels") <- c(No = 0, Yes = 1)
  class(data$consent) <- c("labelled", class(data$consent))

  dictionary <- func_dictionary(data, max_categorical_levels = 10)
  testthat::expect_equal(names(dictionary)[1:5], c("label", "variable", "type", "value_label", "n_missing"))
  testthat::expect_equal(attr(dictionary$label, "label"), "Variable Label")

  name_row <- dictionary[dictionary$variable == "person_name", , drop = FALSE]
  status_row <- dictionary[dictionary$variable == "status", , drop = FALSE]
  consent_row <- dictionary[dictionary$variable == "consent", , drop = FALSE]

  testthat::expect_false(name_row$categorical_eligible)
  testthat::expect_match(name_row$value_label, "Not categorical")
  testthat::expect_match(name_row$categorical_status, "exceed the configured limit of 10")
  testthat::expect_true(status_row$categorical_eligible)
  testthat::expect_match(status_row$value_label, "active")
  testthat::expect_match(status_row$value_domain_status, "observed values only")
  testthat::expect_match(consent_row$value_label, "0 = No")
  testthat::expect_match(consent_row$value_label, "1 = Yes")
  testthat::expect_equal(consent_row$value_domain_status, "source-defined code-label mapping")
})

testthat::test_that("logical fields expose the full type-defined domain", {
  data <- data.frame(approved = c(TRUE, TRUE, NA))
  dictionary <- func_dictionary(data)
  testthat::expect_match(dictionary$value_label[[1L]], "FALSE")
  testthat::expect_match(dictionary$value_label[[1L]], "TRUE")
  testthat::expect_equal(dictionary$value_domain_status[[1L]], "type-defined logical domain")
})

testthat::test_that("missing business definitions are reported rather than invented", {
  data <- data.frame(code = c("A", "B"), stringsAsFactors = FALSE)
  dictionary <- func_dictionary(data)
  config <- in_memory_dictionary_config()
  gaps <- build_documentation_gaps(dictionary, config)

  testthat::expect_true(is.na(dictionary$business_definition[[1L]]))
  testthat::expect_equal(dictionary$documentation_status[[1L]], "needs business definition")
  testthat::expect_true(any(gaps$gap_type == "missing_business_definition"))
})

testthat::test_that("explicit factors remain categorical and preview large level sets", {
  data <- data.frame(group = factor(sprintf("L%02d", 1:20), levels = sprintf("L%02d", 1:20)))
  dictionary <- func_dictionary(data, max_categorical_levels = 10)
  testthat::expect_true(dictionary$categorical_eligible[[1L]])
  testthat::expect_match(dictionary$value_label[[1L]], "20 total")
  testthat::expect_equal(dictionary$value_representation[[1L]], "defined factor levels")
})

testthat::test_that("source labels seed a definition candidate without claiming independent validation", {
  data <- data.frame(status = c("active", "inactive"), stringsAsFactors = FALSE)
  attr(data$status, "label") <- "Current account status"
  dictionary <- func_dictionary(data)

  testthat::expect_equal(dictionary$business_definition[[1L]], "Current account status")
  testthat::expect_match(dictionary$definition_source[[1L]], "not independently validated")
  testthat::expect_equal(
    dictionary$documentation_status[[1L]],
    "definition candidate available from source metadata"
  )
})
