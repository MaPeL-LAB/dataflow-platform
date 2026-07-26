empty_inventory <- function() {
  data.frame(
    source_file = character(), source_basename = character(), source_format = character(),
    supported = logical(), status = character(), datasets_created = integer(),
    error_message = character(), stringsAsFactors = FALSE
  )
}

empty_errors <- function() {
  data.frame(
    stage = character(), source_file = character(), dataset_id = character(),
    error_class = character(), error_message = character(), stringsAsFactors = FALSE
  )
}

assign_dataset_ids <- function(records) {
  if (length(records) == 0L) return(records)
  bases <- vapply(records, function(record) {
    stem <- source_stem(record$source_file)
    sheet <- as.character(record$source_sheet %||% NA_character_)
    object <- as.character(record$source_object %||% NA_character_)
    dataset_name <- as.character(record$dataset_name %||% NA_character_)

    details <- c(stem, sheet, object)
    details <- details[!is.na(details) & nzchar(details)]
    if ((is.na(sheet) || !nzchar(sheet)) && !is.na(dataset_name) && nzchar(dataset_name) && !(dataset_name %in% details)) {
      details <- c(details, dataset_name)
    }
    slugify(paste(unique(details), collapse = "__"), fallback = "dataset")
  }, character(1L))
  ids <- make.unique(bases, sep = "__")
  for (i in seq_along(records)) records[[i]]$dataset_id <- ids[[i]]
  records
}

administrative_metadata_from_config <- function(config) {
  metadata <- config$metadata %||% list()
  tags <- metadata$tags %||% character()
  if (is.list(tags)) tags <- unlist(tags, use.names = FALSE)
  data.frame(
    project_name = as.character(metadata$project_name %||% NA_character_),
    project_description = as.character(metadata$project_description %||% NA_character_),
    author = as.character(metadata$author %||% NA_character_),
    data_owner = as.character(metadata$data_owner %||% NA_character_),
    data_steward = as.character(metadata$data_steward %||% NA_character_),
    source_system = as.character(metadata$source_system %||% NA_character_),
    access_classification = as.character(metadata$access_classification %||% NA_character_),
    access_permissions = as.character(metadata$access_permissions %||% NA_character_),
    license = as.character(metadata$license %||% NA_character_),
    tags = if (length(tags) == 0L) NA_character_ else paste(as.character(tags), collapse = "; "),
    stringsAsFactors = FALSE
  )
}

provenance_metadata_from_datasets <- function(dataset_metadata, imported_utc, config) {
  if (!is.data.frame(dataset_metadata) || nrow(dataset_metadata) == 0L) {
    return(data.frame(
      dataset_id = character(), source_file = character(), source_sheet = character(),
      source_object = character(), source_format = character(), reader_package = character(),
      reader_function = character(), source_file_md5 = character(), imported_utc = character(),
      pipeline_name = character(), pipeline_version = character(), n_rows = integer(),
      n_columns = integer(), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    dataset_id = dataset_metadata$dataset_id,
    source_file = dataset_metadata$source_file,
    source_sheet = dataset_metadata$source_sheet,
    source_object = dataset_metadata$source_object,
    source_format = dataset_metadata$source_format,
    reader_package = dataset_metadata$reader_package,
    reader_function = dataset_metadata$reader_function,
    source_file_md5 = dataset_metadata$file_md5,
    imported_utc = rep(imported_utc, nrow(dataset_metadata)),
    pipeline_name = rep(as.character(config$pipeline$name), nrow(dataset_metadata)),
    pipeline_version = rep(as.character(config$pipeline$version), nrow(dataset_metadata)),
    n_rows = dataset_metadata$n_rows,
    n_columns = dataset_metadata$n_columns,
    stringsAsFactors = FALSE
  )
}

metadata_coverage_table <- function(data_dictionary, administrative_metadata) {
  definitions_missing <- if (!is.data.frame(data_dictionary) || nrow(data_dictionary) == 0L) {
    0L
  } else {
    sum(!nonblank_text(data_dictionary$business_definition))
  }
  admin_values <- unlist(administrative_metadata, use.names = FALSE)
  admin_supplied <- any(nonblank_text(admin_values))

  data.frame(
    metadata_category = c("structural", "descriptive", "administrative", "provenance", "quality"),
    coverage_status = c(
      "captured",
      if (definitions_missing == 0L) "definition candidates captured from source labels" else "partial; documentation gaps reported",
      if (admin_supplied) "supplied through configuration" else "not supplied; fields retained as blank",
      "captured",
      "captured"
    ),
    primary_tables = c(
      "dataset_metadata; variable_metadata; supported_formats",
      "data_dictionary; categorical_levels; value_labels; documentation_gaps",
      "administrative_metadata",
      "file_metadata; provenance_metadata; run_metadata; resolved_config",
      "quality_issues; import_problems; errors"
    ),
    notes = c(
      "Describes files, datasets, columns, types, dimensions, formats, and relationships to source objects/sheets.",
      sprintf("Business definitions are never invented; %d field(s) currently require a curated plain-language definition.", definitions_missing),
      "Owner, steward, author, access, licensing, and source-system fields are accepted from configuration.",
      "Records where data came from, how it was read, by which pipeline version, and when.",
      "Records observed completeness, cardinality, parsing, duplication, privacy, and other quality signals."
    ),
    stringsAsFactors = FALSE
  )
}

output_table_catalog <- function() {
  data.frame(
    artifact_group = c(
      "dictionary", "dictionary + metadata", "dictionary + metadata", "dictionary", "dictionary + metadata",
      "metadata", "metadata", "metadata", "metadata", "metadata", "metadata", "metadata",
      "metadata", "metadata", "metadata", "metadata", "metadata", "metadata", "metadata"
    ),
    table_name = c(
      "data_dictionary", "categorical_levels", "value_labels", "dictionary_schema", "documentation_gaps",
      "run_metadata", "summary", "administrative_metadata", "metadata_coverage", "supported_formats",
      "table_catalog", "input_inventory", "file_metadata", "dataset_metadata", "provenance_metadata",
      "variable_metadata", "import_problems", "quality_issues", "errors"
    ),
    grain = c(
      "one row per variable", "one row per reported level", "one row per imported code-label pair",
      "one row per dictionary column", "one row per documentation/review gap", "one row per run",
      "one row per run", "one row per run", "one row per metadata category", "one row per supported format",
      "one row per generated table", "one row per discovered file", "one row per discovered file",
      "one row per imported dataset/sheet/object", "one row per imported dataset/sheet/object",
      "one row per variable", "one row per reader parsing problem", "one row per quality flag",
      "one row per captured pipeline exception"
    ),
    purpose = c(
      "Reviewable field-level reference that organizes the most decision-relevant metadata and records remaining curation gaps.",
      "Complete or capped categorical frequency detail.",
      "Complete source code-to-label mappings.",
      "Definitions for every data_dictionary column.",
      "Items requiring human curation or confirmation.",
      "Execution context and software environment.",
      "Run-level counts and outcomes.",
      "User-supplied governance and access context.",
      "Explains which metadata categories were captured and where.",
      "Machine-readable ingestion capability matrix.",
      "Catalog of generated tables, their grain, product membership, and purpose.",
      "Discovery and ingestion status for each file.",
      "File-system and format metadata.",
      "Shape, source, and dataset-level quality metadata.",
      "Source-to-dataset lineage and reader details.",
      "Detailed technical statistics for every variable.",
      "Delimited-text parsing diagnostics.",
      "Observed data-quality, privacy, and classification signals.",
      "Ingestion or profiling exceptions retained when continue-on-error is enabled."
    ),
    stringsAsFactors = FALSE
  )
}

run_data_dictionary_pipeline <- function(inputs, output_dir, default_config_path,
                                         user_config_path = NULL, overrides = list()) {
  started_at <- Sys.time()
  config <- load_data_dictionary_config(default_config_path, user_config_path, overrides)
  output_dir <- path.expand(output_dir)

  files <- discover_input_files(
    inputs,
    recursive = isTRUE(config$input$recursive),
    include_hidden = isTRUE(config$input$include_hidden)
  )
  if (length(files) == 0L) stop("No input files were discovered.", call. = FALSE)

  inventory <- list()
  errors <- list()
  records <- list()
  file_metadata_rows <- list()

  for (path in files) {
    auxiliary_format <- detect_auxiliary_source_format(path)
    format <- detect_source_format(path)
    metadata_format <- if (!is.na(auxiliary_format)) auxiliary_format else format
    file_meta <- source_file_metadata(path, config$input$compute_md5, metadata_format)
    file_metadata_rows[[length(file_metadata_rows) + 1L]] <- file_meta

    if (!is.na(auxiliary_format)) {
      inventory[[length(inventory) + 1L]] <- data.frame(
        source_file = path, source_basename = basename(path), source_format = auxiliary_format,
        supported = TRUE, status = "auxiliary", datasets_created = 0L,
        error_message = "Auxiliary SAS catalog; used automatically when a matching SAS data set is present.",
        stringsAsFactors = FALSE
      )
      next
    }

    if (is.na(format)) {
      inventory[[length(inventory) + 1L]] <- data.frame(
        source_file = path, source_basename = basename(path), source_format = NA_character_,
        supported = FALSE, status = "skipped_unsupported", datasets_created = 0L,
        error_message = "Unsupported file extension.", stringsAsFactors = FALSE
      )
      next
    }

    log_info("Reading ", basename(path), " [", format, "]")
    read_result <- tryCatch(
      read_source_file(path, config),
      error = function(e) e
    )

    if (inherits(read_result, "error")) {
      message <- conditionMessage(read_result)
      inventory[[length(inventory) + 1L]] <- data.frame(
        source_file = path, source_basename = basename(path), source_format = format,
        supported = TRUE, status = "failed", datasets_created = 0L,
        error_message = message, stringsAsFactors = FALSE
      )
      errors[[length(errors) + 1L]] <- data.frame(
        stage = "ingestion", source_file = path, dataset_id = NA_character_,
        error_class = class(read_result)[[1L]], error_message = message,
        stringsAsFactors = FALSE
      )
      log_error("Could not read ", basename(path), ": ", message)
      if (!isTRUE(config$input$continue_on_error)) stop(read_result)
      next
    }

    for (record in read_result) {
      record$file_metadata <- file_meta
      records[[length(records) + 1L]] <- record
    }
    inventory[[length(inventory) + 1L]] <- data.frame(
      source_file = path, source_basename = basename(path), source_format = format,
      supported = TRUE, status = "read", datasets_created = length(read_result),
      error_message = NA_character_, stringsAsFactors = FALSE
    )
  }

  records <- assign_dataset_ids(records)
  profiles <- list()
  for (record in records) {
    log_info("Profiling dataset ", record$dataset_id, " (", nrow(record$data), " rows x ", ncol(record$data), " columns)")
    profile <- tryCatch(profile_dataset(record, config), error = function(e) e)
    if (inherits(profile, "error")) {
      message <- conditionMessage(profile)
      errors[[length(errors) + 1L]] <- data.frame(
        stage = "profiling", source_file = record$source_file, dataset_id = record$dataset_id,
        error_class = class(profile)[[1L]], error_message = message,
        stringsAsFactors = FALSE
      )
      log_error("Could not profile ", record$dataset_id, ": ", message)
      if (!isTRUE(config$input$continue_on_error)) stop(profile)
      next
    }
    profiles[[length(profiles) + 1L]] <- profile
  }

  dataset_metadata <- bind_rows_fill(lapply(profiles, `[[`, "dataset"))
  variable_metadata <- bind_rows_fill(lapply(profiles, `[[`, "variables"))
  categorical_levels <- if (length(profiles) == 0L) empty_categorical_levels() else bind_rows_fill(lapply(profiles, `[[`, "categorical_levels"))
  value_labels <- if (length(profiles) == 0L) empty_value_labels() else bind_rows_fill(lapply(profiles, `[[`, "value_labels"))
  import_problems <- if (length(profiles) == 0L) empty_import_problems() else bind_rows_fill(lapply(profiles, `[[`, "import_problems"))
  quality_issues <- if (length(profiles) == 0L) empty_quality_issues() else bind_rows_fill(lapply(profiles, `[[`, "issues"))
  inventory_frame <- if (length(inventory) == 0L) empty_inventory() else bind_rows_fill(inventory)
  errors_frame <- if (length(errors) == 0L) empty_errors() else bind_rows_fill(errors)
  file_metadata <- bind_rows_fill(file_metadata_rows)

  if (nrow(file_metadata) > 0L && nrow(inventory_frame) > 0L) {
    inventory_index <- match(file_metadata$source_file, inventory_frame$source_file)
    file_metadata$supported <- inventory_frame$supported[inventory_index]
    file_metadata$ingestion_status <- inventory_frame$status[inventory_index]
    file_metadata$datasets_created <- inventory_frame$datasets_created[inventory_index]
  }

  data_dictionary <- build_data_dictionary(
    variable_metadata,
    dataset_metadata,
    categorical_levels,
    value_labels,
    config
  )
  documentation_gaps <- build_documentation_gaps(data_dictionary, config)
  dictionary_schema <- data_dictionary_schema()
  administrative_metadata <- administrative_metadata_from_config(config)
  completed_at <- Sys.time()
  completed_utc <- format(completed_at, tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ")
  provenance_metadata <- provenance_metadata_from_datasets(dataset_metadata, completed_utc, config)
  metadata_coverage <- metadata_coverage_table(data_dictionary, administrative_metadata)
  supported_formats <- supported_formats_table()
  table_catalog <- output_table_catalog()

  packages <- c("yaml", "readr", "readxl", "haven", "jsonlite", "openxlsx", "arrow", "reticulate")
  package_versions <- paste(
    paste0(packages, "=", vapply(packages, package_version_safe, character(1L))),
    collapse = "; "
  )

  run_metadata <- data.frame(
    pipeline_name = as.character(config$pipeline$name),
    pipeline_version = as.character(config$pipeline$version),
    started_utc = format(started_at, tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ"),
    completed_utc = completed_utc,
    duration_seconds = as.numeric(difftime(completed_at, started_at, units = "secs")),
    r_version = R.version.string,
    platform = R.version$platform,
    package_versions = package_versions,
    output_directory = normalizePath(path.expand(output_dir), winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    input_files = nrow(inventory_frame),
    supported_files = sum(inventory_frame$supported %in% TRUE),
    files_read = sum(inventory_frame$status == "read"),
    files_failed = sum(inventory_frame$status == "failed"),
    datasets = nrow(dataset_metadata),
    variables = nrow(variable_metadata),
    dictionary_rows = nrow(data_dictionary),
    categorical_variables = if (nrow(variable_metadata) == 0L) 0L else sum(variable_metadata$categorical_eligible %in% TRUE),
    noncategorical_character_variables = if (nrow(variable_metadata) == 0L) 0L else sum(variable_metadata$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")),
    variables_missing_business_definition = if (nrow(data_dictionary) == 0L) 0L else sum(!nonblank_text(data_dictionary$business_definition)),
    import_problems = nrow(import_problems),
    quality_issues = nrow(quality_issues),
    errors = nrow(errors_frame),
    stringsAsFactors = FALSE
  )

  results <- list(
    run_metadata = run_metadata,
    summary = summary,
    administrative_metadata = administrative_metadata,
    metadata_coverage = metadata_coverage,
    supported_formats = supported_formats,
    table_catalog = table_catalog,
    input_inventory = inventory_frame,
    file_metadata = file_metadata,
    dataset_metadata = dataset_metadata,
    provenance_metadata = provenance_metadata,
    variable_metadata = variable_metadata,
    data_dictionary = data_dictionary,
    variable_dictionary = data_dictionary,
    dictionary_schema = dictionary_schema,
    documentation_gaps = documentation_gaps,
    categorical_levels = categorical_levels,
    value_labels = value_labels,
    import_problems = import_problems,
    quality_issues = quality_issues,
    errors = errors_frame
  )

  artifacts <- export_metadata_bundle(results, output_dir, config)
  log_info("Created ", length(artifacts), " artifact(s) in ", normalizePath(output_dir, winslash = "/", mustWork = TRUE))

  invisible(list(results = results, artifacts = artifacts, config = config))
}
