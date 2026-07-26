testthat::test_that("delimited text readers ingest CSV, TSV, and configured TXT", {
  config <- test_config(list(input = list(text_delimiter = "|")))
  data <- data.frame(id = 1:3, group = c("A", "B", "A"), stringsAsFactors = FALSE)

  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  txt <- tempfile(fileext = ".txt")
  utils::write.csv(data, csv, row.names = FALSE)
  utils::write.table(data, tsv, row.names = FALSE, sep = "\t", quote = FALSE)
  utils::write.table(data, txt, row.names = FALSE, sep = "|", quote = FALSE)

  for (path in c(csv, tsv, txt)) {
    records <- read_source_file(path, config)
    testthat::expect_length(records, 1L)
    testthat::expect_equal(nrow(records[[1L]]$data), 3L)
    testthat::expect_equal(names(records[[1L]]$data), c("id", "group"))
  }
})

testthat::test_that("Excel readers expand every workbook sheet by default", {
  testthat::skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      baseline = data.frame(id = 1:2, status = c("A", "B")),
      follow_up = data.frame(id = 1:3, score = c(10, 20, 30))
    ),
    path
  )

  records <- read_source_file(path, test_config())
  testthat::expect_equal(length(records), 2L)
  testthat::expect_equal(vapply(records, `[[`, character(1L), "source_sheet"), c("baseline", "follow_up"))
})

testthat::test_that("readxl package examples cover both XLS and XLSX dispatch", {
  testthat::skip_if_not_installed("readxl")
  for (example_name in c("datasets.xls", "datasets.xlsx")) {
    path <- readxl::readxl_example(example_name)
    testthat::skip_if(!nzchar(path) || !file.exists(path), paste("Missing readxl example", example_name))
    sheets <- readxl::excel_sheets(path)
    testthat::expect_gt(length(sheets), 0L)
    selected_sheet <- sheets[[1L]]
    records <- read_source_file(path, test_config(list(input = list(excel_sheets = selected_sheet))))
    testthat::expect_length(records, 1L)
    testthat::expect_identical(records[[1L]]$source_sheet, selected_sheet)
    testthat::expect_gt(nrow(records[[1L]]$data), 0L)
  }
})

testthat::test_that("haven package examples ingest Stata, SAS, and SPSS", {
  testthat::skip_if_not_installed("haven")
  examples <- c(dta = "iris.dta", sas7bdat = "iris.sas7bdat", sav = "iris.sav")

  for (format in names(examples)) {
    path <- system.file("examples", examples[[format]], package = "haven")
    if (!nzchar(path) || !file.exists(path)) next
    records <- read_source_file(path, test_config())
    testthat::expect_length(records, 1L)
    testthat::expect_gt(nrow(records[[1L]]$data), 0L)
  }
})

testthat::test_that("legacy Stata trailing corruption uses the base-R safety reader", {
  testthat::skip_if_not_installed("haven")
  path <- tempfile(fileext = ".dta")
  expected <- data.frame(
    id = c(1L, 2L, 3L),
    status = c("A", "B", "A"),
    amount = c(10.5, 20.5, 30.5),
    stringsAsFactors = FALSE
  )
  haven::write_dta(expected, path, version = 12)
  clean_layout <- inspect_stata_legacy_layout(path)
  testthat::expect_false(clean_layout$unexpected_trailing_payload)

  connection <- file(path, open = "r+b")
  seek(connection, where = clean_layout$data_end, origin = "start", rw = "write")
  truncate(connection)
  writeBin(as.raw(c(0L, 0L, 0L, 0L, 1L, 2L, 3L, 4L)), connection)
  close(connection)

  corrupt_layout <- inspect_stata_legacy_layout(path)
  testthat::expect_true(corrupt_layout$unexpected_trailing_payload)
  records <- read_source_file(path, test_config())
  testthat::expect_length(records, 1L)
  testthat::expect_equal(records[[1L]]$reader_function, "read_stata_legacy_recovery")
  testthat::expect_equal(as.integer(records[[1L]]$data$id), expected$id)
  testthat::expect_equal(as.character(records[[1L]]$data$status), expected$status)
  testthat::expect_equal(as.numeric(records[[1L]]$data$amount), expected$amount)
})

testthat::test_that("SAS transport input is ingested", {
  testthat::skip_if_not_installed("haven")
  path <- tempfile(fileext = ".xpt")
  haven::write_xpt(data.frame(id = 1:3, status = c("A", "B", "A")), path)
  records <- read_source_file(path, test_config())
  testthat::expect_length(records, 1L)
  testthat::expect_equal(nrow(records[[1L]]$data), 3L)
})

testthat::test_that("RDS and RData files expand multiple tabular objects", {
  first <- data.frame(id = 1:2, stringsAsFactors = FALSE)
  second <- data.frame(code = c("A", "B", "C"), stringsAsFactors = FALSE)

  rds <- tempfile(fileext = ".rds")
  saveRDS(list(first = first, second = second), rds)
  rds_records <- read_source_file(rds, test_config())
  testthat::expect_equal(length(rds_records), 2L)

  rdata <- tempfile(fileext = ".RData")
  save(first, second, file = rdata)
  rdata_records <- read_source_file(rdata, test_config())
  testthat::expect_equal(length(rdata_records), 2L)
  testthat::expect_setequal(vapply(rdata_records, `[[`, character(1L), "source_object"), c("first", "second"))
})

testthat::test_that("JSON and JSON Lines are normalized into tabular records", {
  testthat::skip_if_not_installed("jsonlite")
  data <- data.frame(id = 1:3, status = c("A", "B", "A"), stringsAsFactors = FALSE)

  json <- tempfile(fileext = ".json")
  jsonlite::write_json(data, json, dataframe = "rows", auto_unbox = TRUE)
  json_records <- read_source_file(json, test_config())
  testthat::expect_length(json_records, 1L)
  testthat::expect_equal(nrow(json_records[[1L]]$data), 3L)

  jsonl <- tempfile(fileext = ".jsonl")
  connection <- file(jsonl, open = "wt", encoding = "UTF-8")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  jsonlite::stream_out(data, con = connection, verbose = FALSE)
  close(connection)
  jsonl_records <- read_source_file(jsonl, test_config())
  testthat::expect_length(jsonl_records, 1L)
  testthat::expect_equal(nrow(jsonl_records[[1L]]$data), 3L)
})

testthat::test_that("Arrow, Parquet, Feather, and IPC readers ingest optional formats", {
  testthat::skip_if_not_installed("arrow")
  data <- data.frame(id = 1:3, status = c("A", "B", "A"), stringsAsFactors = FALSE)
  paths <- c(
    parquet = tempfile(fileext = ".parquet"),
    feather = tempfile(fileext = ".feather"),
    arrow = tempfile(fileext = ".arrow"),
    ipc = tempfile(fileext = ".ipc")
  )

  arrow::write_parquet(data, paths[["parquet"]])
  arrow::write_feather(data, paths[["feather"]])
  arrow::write_feather(data, paths[["arrow"]])
  arrow::write_ipc_stream(data, paths[["ipc"]])

  for (path in unname(paths)) {
    records <- read_source_file(path, test_config())
    testthat::expect_length(records, 1L)
    testthat::expect_equal(nrow(records[[1L]]$data), 3L)
  }
})

testthat::test_that("Python pickle requires opt-in and ingests a trusted temporary object", {
  path <- tempfile(fileext = ".pkl")
  file.create(path)
  testthat::expect_error(read_source_file(path, test_config()), "disabled")

  testthat::skip_if_not_installed("reticulate")
  testthat::skip_if_not(reticulate::py_available(initialize = TRUE), "No usable Python installation")
  python_object <- reticulate::r_to_py(
    list(id = as.list(1:3), status = as.list(c("A", "B", "A"))),
    convert = FALSE
  )
  reticulate::py_save_object(python_object, path)
  config <- test_config(list(input = list(allow_unsafe_pickle = TRUE)))
  records <- read_source_file(path, config)
  testthat::expect_length(records, 1L)
  testthat::expect_equal(nrow(records[[1L]]$data), 3L)
})

testthat::test_that("SAS catalog resolution supports sibling, case-insensitive, and relative paths", {
  directory <- tempfile(pattern = "sas-catalog-")
  dir.create(directory)
  data_path <- file.path(directory, "Study.SAS7BDAT")
  catalog_path <- file.path(directory, "study.SAS7BCAT")
  file.create(data_path, catalog_path)

  automatic <- resolve_sas_catalog(data_path, test_config())
  testthat::expect_equal(
    normalizePath(automatic, winslash = "/"),
    normalizePath(catalog_path, winslash = "/")
  )

  nested <- file.path(directory, "catalogs")
  dir.create(nested)
  configured_catalog <- file.path(nested, "formats.sas7bcat")
  file.create(configured_catalog)
  relative_config <- test_config(list(input = list(
    auto_sas_catalog = FALSE,
    sas_catalog = file.path("catalogs", "formats.sas7bcat")
  )))
  resolved <- resolve_sas_catalog(data_path, relative_config)
  testthat::expect_equal(
    normalizePath(resolved, winslash = "/"),
    normalizePath(configured_catalog, winslash = "/")
  )
})
