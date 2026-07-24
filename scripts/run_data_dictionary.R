#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[[1]])) y else x

parse_args <- function(x) {
  out <- list()
  i <- 1L
  while (i <= length(x)) {
    key <- sub("^--", "", x[[i]])
    value <- if (i < length(x) && !startsWith(x[[i + 1L]], "--")) x[[i + 1L]] else TRUE
    out[[gsub("-", "_", key)]] <- value
    i <- i + if (identical(value, TRUE)) 1L else 2L
  }
  out
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
input <- args$input %||% stop("Use --input <file-or-directory>")
output <- args$output %||% "outputs/data_dictionary"
max_levels <- as.integer(args$max_categorical_levels %||% 10L)
recursive <- tolower(as.character(args$recursive %||% "false")) %in% c("true", "1", "yes")

dir.create(output, recursive = TRUE, showWarnings = FALSE)

supported <- c("csv", "tsv", "txt", "xls", "xlsx", "dta", "sas7bdat", "xpt", "sav", "zsav", "por", "rds", "rda", "rdata", "json", "jsonl", "ndjson", "parquet", "feather")
files <- if (dir.exists(input)) list.files(input, recursive = recursive, full.names = TRUE) else input
files <- files[tolower(tools::file_ext(files)) %in% supported]
if (!length(files)) stop("No supported input files found.")

read_one <- function(path) {
  ext <- tolower(tools::file_ext(path))
  name <- tools::file_path_sans_ext(basename(path))
  wrap <- function(x, suffix = NULL) list(data = as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE), dataset = paste(c(name, suffix), collapse = "__"), source = normalizePath(path, winslash = "/"), format = ext)
  if (ext == "csv") return(list(wrap(readr::read_csv(path, show_col_types = FALSE, progress = FALSE))))
  if (ext %in% c("tsv", "txt")) return(list(wrap(readr::read_tsv(path, show_col_types = FALSE, progress = FALSE))))
  if (ext %in% c("xls", "xlsx")) return(lapply(readxl::excel_sheets(path), function(s) wrap(readxl::read_excel(path, sheet = s), s)))
  if (ext == "dta") return(list(wrap(haven::read_dta(path))))
  if (ext == "sas7bdat") return(list(wrap(haven::read_sas(path))))
  if (ext == "xpt") return(list(wrap(haven::read_xpt(path))))
  if (ext %in% c("sav", "zsav")) return(list(wrap(haven::read_sav(path, user_na = TRUE))))
  if (ext == "por") return(list(wrap(haven::read_por(path, user_na = TRUE))))
  if (ext == "rds") {
    x <- readRDS(path)
    if (is.data.frame(x)) return(list(wrap(x)))
    if (is.list(x)) return(Filter(Negate(is.null), lapply(names(x), function(n) if (is.data.frame(x[[n]])) wrap(x[[n]], n) else NULL)))
    stop("RDS does not contain a data frame or list of data frames")
  }
  if (ext %in% c("rda", "rdata")) {
    e <- new.env(parent = emptyenv()); load(path, envir = e)
    return(Filter(Negate(is.null), lapply(ls(e), function(n) if (is.data.frame(e[[n]])) wrap(e[[n]], n) else NULL)))
  }
  if (ext == "json") return(list(wrap(jsonlite::fromJSON(path, flatten = TRUE))))
  if (ext %in% c("jsonl", "ndjson")) return(list(wrap(jsonlite::stream_in(file(path), verbose = FALSE))))
  if (ext %in% c("parquet", "feather")) {
    if (!requireNamespace("arrow", quietly = TRUE)) stop("Install optional package 'arrow'")
    x <- if (ext == "parquet") arrow::read_parquet(path) else arrow::read_feather(path)
    return(list(wrap(x)))
  }
  stop("Unsupported format: ", ext)
}

records <- list(); errors <- list()
for (f in files) {
  got <- tryCatch(read_one(f), error = function(e) { errors[[length(errors) + 1L]] <<- data.frame(source_file = f, error = conditionMessage(e)); NULL })
  if (length(got)) records <- c(records, got)
}
if (!length(records)) stop("No datasets could be read.")

is_identifier_name <- function(n) grepl("(^id$|_id$|identifier|record|subject|participant|person_name|email|phone|address)", tolower(n))
is_sensitive_name <- function(n) grepl("hiv|diagnos|medical|health|race|ethnic|relig|income|password", tolower(n))
mask <- function(x) ifelse(is.na(x), NA_character_, "<masked>")

variable_rows <- list(); level_rows <- list(); issue_rows <- list(); dataset_rows <- list()
for (rec in records) {
  d <- rec$data
  dataset_rows[[length(dataset_rows) + 1L]] <- data.frame(dataset = rec$dataset, source_file = rec$source, source_format = rec$format, rows = nrow(d), columns = ncol(d), duplicated_rows = sum(duplicated(d)), stringsAsFactors = FALSE)
  for (nm in names(d)) {
    x <- d[[nm]]
    chars <- trimws(as.character(x))
    nonblank <- chars[!is.na(chars) & nzchar(chars)]
    unique_vals <- unique(nonblank)
    n_unique <- length(unique_vals)
    missing_n <- sum(is.na(x) | (is.character(x) & !nzchar(trimws(x))))
    explicit_cat <- is.factor(x) || inherits(x, "haven_labelled") || inherits(x, "labelled")
    categorical <- explicit_cat || (is.character(x) && n_unique > 0 && n_unique <= max_levels)
    unique_ratio <- if (length(nonblank)) n_unique / length(nonblank) else NA_real_
    id_hint <- is_identifier_name(nm) || (!is.numeric(x) && !is.logical(x) && is.finite(unique_ratio) && unique_ratio >= 0.95)
    free_text <- is.character(x) && length(nonblank) && median(nchar(nonblank)) >= 40
    semantic <- if (inherits(x, "Date")) "date" else if (inherits(x, c("POSIXct", "POSIXlt"))) "datetime" else if (is.logical(x)) "logical" else if (is.numeric(x)) "numeric" else if (categorical) "categorical" else if (id_hint) "identifier" else if (free_text) "free_text" else if (is.character(x)) "high_cardinality_text" else class(x)[1]
    reason <- if (explicit_cat) "Explicit factor or imported labelled variable." else if (is.character(x) && categorical) sprintf("Character variable has %d distinct nonblank values, within the configured limit of %d.", n_unique, max_levels) else if (is.character(x)) sprintf("Character variable has %d distinct nonblank values, exceeding the configured categorical limit of %d.", n_unique, max_levels) else sprintf("Classified from R class: %s.", paste(class(x), collapse = ", "))
    examples <- head(unique_vals, 5)
    ex <- if (id_hint || is_sensitive_name(nm)) "<masked>" else paste(examples, collapse = " | ")
    variable_rows[[length(variable_rows) + 1L]] <- data.frame(dataset = rec$dataset, variable_name = nm, variable_label = as.character(attr(x, "label") %||% NA_character_), r_class = paste(class(x), collapse = ", "), semantic_type = semantic, categorical_eligible = categorical, n = length(x), missing_n = missing_n, missing_pct = if (length(x)) missing_n / length(x) else NA_real_, n_unique_non_missing = n_unique, uniqueness_ratio = unique_ratio, min = if (is.numeric(x) && any(is.finite(x), na.rm = TRUE)) min(x[is.finite(x)], na.rm = TRUE) else NA, max = if (is.numeric(x) && any(is.finite(x), na.rm = TRUE)) max(x[is.finite(x)], na.rm = TRUE) else NA, mean = if (is.numeric(x) && any(is.finite(x), na.rm = TRUE)) mean(x[is.finite(x)], na.rm = TRUE) else NA, examples = ex, classification_reason = reason, stringsAsFactors = FALSE)
    if (categorical) for (v in unique_vals) level_rows[[length(level_rows) + 1L]] <- data.frame(dataset = rec$dataset, variable_name = nm, value = v, count = sum(chars == v, na.rm = TRUE), stringsAsFactors = FALSE)
    add_issue <- function(type, severity, detail) issue_rows[[length(issue_rows) + 1L]] <<- data.frame(dataset = rec$dataset, variable_name = nm, issue_type = type, severity = severity, detail = detail, stringsAsFactors = FALSE)
    if (is.character(x) && n_unique > max_levels) add_issue("high_cardinality_character", "info", reason)
    if (missing_n == length(x)) add_issue("all_missing", "warning", "All values are missing or blank.")
    if (n_unique == 1 && missing_n < length(x)) add_issue("constant", "warning", "Only one distinct nonmissing value.")
    if (length(x) && missing_n / length(x) >= 0.40) add_issue("high_missingness", "warning", "At least 40% of values are missing or blank.")
    if (id_hint) add_issue("potential_identifier", "warning", "Name/content pattern suggests an identifier; examples were masked.")
    if (is_sensitive_name(nm)) add_issue("potential_sensitive_field", "warning", "Variable name suggests sensitive information; examples were masked.")
  }
}

bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
variables <- bind(variable_rows); levels <- bind(level_rows); issues <- bind(issue_rows); datasets <- bind(dataset_rows); error_df <- bind(errors)
readr::write_csv(variables, file.path(output, "variable_dictionary.csv"), na = "")
readr::write_csv(datasets, file.path(output, "dataset_metadata.csv"), na = "")
readr::write_csv(levels, file.path(output, "categorical_levels.csv"), na = "")
readr::write_csv(issues, file.path(output, "quality_issues.csv"), na = "")
readr::write_csv(error_df, file.path(output, "errors.csv"), na = "")
jsonlite::write_json(list(datasets = datasets, variables = variables, categorical_levels = levels, quality_issues = issues, errors = error_df), file.path(output, "metadata.json"), dataframe = "rows", pretty = TRUE, na = "null")
openxlsx::write.xlsx(list(datasets = datasets, variables = variables, categorical_levels = levels, quality_issues = issues, errors = error_df), file.path(output, "metadata.xlsx"), overwrite = TRUE)
yaml::write_yaml(list(input = normalizePath(input, winslash = "/", mustWork = TRUE), max_categorical_levels = max_levels, recursive = recursive), file.path(output, "resolved_config.yml"))

html <- paste0("<!doctype html><html><head><meta charset='utf-8'><title>Data dictionary</title><style>body{font-family:Arial;margin:2rem}table{border-collapse:collapse;width:100%;font-size:.85rem}th,td{border:1px solid #ddd;padding:.4rem;text-align:left}th{background:#f3f3f3}</style></head><body><h1>Data Dictionary</h1><p>Datasets: ", nrow(datasets), " | Variables: ", nrow(variables), " | Quality issues: ", nrow(issues), "</p>", paste(capture.output(print(variables, row.names = FALSE)), collapse = "<br>"), "</body></html>")
writeLines(html, file.path(output, "metadata_report.html"))
message("Data dictionary written to: ", normalizePath(output, winslash = "/"))
