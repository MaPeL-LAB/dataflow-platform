testthat::test_that("the data dictionary pipeline is registered", {
  registry <- list_registered_pipelines(repo_root)
  testthat::expect_true("data_dictionary" %in% registry$pipeline)
  resolved <- resolve_pipeline("data_dictionary", repo_root)
  testthat::expect_true(file.exists(resolved$entrypoint))
  testthat::expect_true(file.exists(resolved$default_config))
})
