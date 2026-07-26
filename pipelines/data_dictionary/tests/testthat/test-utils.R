testthat::test_that("format detection handles supported compression and auxiliary catalogs", {
  testthat::expect_equal(detect_source_format("study.csv.gz"), "csv")
  testthat::expect_equal(detect_source_format("study.dta.xz"), "dta")
  testthat::expect_equal(detect_source_format("study.sav.bz2"), "sav")
  testthat::expect_true(is.na(detect_source_format("study.xlsx.gz")))
  testthat::expect_equal(detect_auxiliary_source_format("formats.sas7bcat"), "sas7bcat")
  testthat::expect_equal(source_stem("study.dta.gz"), "study")
})

testthat::test_that("dataset identifiers are concise for workbook sheets", {
  records <- list(list(
    source_file = "/tmp/workbook.xlsx",
    source_sheet = "employees",
    source_object = NA_character_,
    dataset_name = "workbook__employees"
  ))
  identified <- assign_dataset_ids(records)
  testthat::expect_equal(identified[[1L]]$dataset_id, "workbook_employees")
})

testthat::test_that("the supported-format matrix retains every ingestion family", {
  formats <- supported_formats_table()
  testthat::expect_true(all(c(
    "csv", "xlsx", "dta", "sas7bdat", "xpt", "sav", "rds", "rdata",
    "json", "jsonl", "parquet", "feather", "arrow", "ipc", "pickle"
  ) %in% formats$format))
  testthat::expect_equal(formats$availability[formats$format == "pickle"], "optional; disabled unless explicitly enabled")
})

testthat::test_that("artifact groups preserve the dictionary-metadata distinction", {
  paths <- c(
    "csv/data_dictionary.csv",
    "csv/variable_dictionary.csv",
    "csv/categorical_levels.csv",
    "metadata.json",
    "metadata_report.html",
    "resolved_config.yml"
  )
  testthat::expect_equal(
    artifact_group_for_path(paths),
    c("dictionary", "dictionary", "dictionary + metadata", "metadata", "combined report", "control")
  )
})
