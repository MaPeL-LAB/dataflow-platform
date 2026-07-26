dictionary_tables <- function(results) {
  list(
    data_dictionary = results$data_dictionary,
    categorical_levels = results$categorical_levels,
    value_labels = results$value_labels,
    documentation_gaps = results$documentation_gaps,
    dictionary_schema = results$dictionary_schema
  )
}

metadata_core_tables <- function(results) {
  list(
    run_metadata = results$run_metadata,
    summary = results$summary,
    administrative_metadata = results$administrative_metadata,
    metadata_coverage = results$metadata_coverage,
    supported_formats = results$supported_formats,
    table_catalog = results$table_catalog,
    input_inventory = results$input_inventory,
    file_metadata = results$file_metadata,
    dataset_metadata = results$dataset_metadata,
    provenance_metadata = results$provenance_metadata,
    variable_metadata = results$variable_metadata,
    import_problems = results$import_problems,
    quality_issues = results$quality_issues,
    errors = results$errors
  )
}

metadata_tables <- function(results) {
  c(
    metadata_core_tables(results),
    list(
      categorical_levels = results$categorical_levels,
      value_labels = results$value_labels,
      documentation_gaps = results$documentation_gaps
    )
  )
}

all_csv_tables <- function(results) {
  # Companion value tables belong conceptually to both products, but each CSV
  # is written only once.
  c(dictionary_tables(results), metadata_core_tables(results))
}

write_csv_outputs <- function(results, output_dir, config) {
  csv_dir <- file.path(output_dir, "csv")
  if (dir.exists(csv_dir)) unlink(csv_dir, recursive = TRUE, force = TRUE)
  safe_dir_create(csv_dir)
  paths <- character()
  tables <- all_csv_tables(results)
  for (name in names(tables)) {
    path <- file.path(csv_dir, paste0(name, ".csv"))
    frame <- atomicize_data_frame(tables[[name]])
    if (!is.data.frame(frame) || ncol(frame) == 0L) {
      frame <- data.frame(note = character(), stringsAsFactors = FALSE)
    }
    utils::write.csv(frame, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    paths <- c(paths, path)
  }

  if (isTRUE(config$output$write_legacy_variable_dictionary)) {
    legacy_path <- file.path(csv_dir, "variable_dictionary.csv")
    utils::write.csv(
      atomicize_data_frame(results$data_dictionary),
      legacy_path,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )
    paths <- c(paths, legacy_path)
  }
  paths
}

write_json_tables <- function(tables, path, extra = list()) {
  require_package("jsonlite", "JSON output")
  payload <- c(tables, extra)
  jsonlite::write_json(
    payload,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "rows",
    na = "null",
    null = "null",
    digits = NA
  )
  path
}

excel_column_widths <- function(frame, max_rows = 200L) {
  frame <- atomicize_data_frame(frame)
  if (ncol(frame) == 0L) return(numeric())
  sample <- if (nrow(frame) > max_rows) frame[seq_len(max_rows), , drop = FALSE] else frame
  vapply(seq_along(sample), function(i) {
    values <- c(names(sample)[[i]], as.character(sample[[i]]))
    values[is.na(values)] <- ""
    min(50, max(10, max(nchar(values, type = "width"), na.rm = TRUE) + 2))
  }, numeric(1L))
}

write_excel_tables <- function(tables, path, config, creator) {
  require_package("openxlsx", "Excel output")
  wb <- openxlsx::createWorkbook(creator = creator)
  existing <- character()
  header_style <- openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#24324A", textDecoration = "bold",
    border = "Bottom", borderColour = "#D9E0EA"
  )
  warning_style <- openxlsx::createStyle(fgFill = "#FFF2CC")

  for (name in names(tables)) {
    sheet <- safe_sheet_name(name, existing)
    existing <- c(existing, sheet)
    frame <- atomicize_data_frame(tables[[name]])
    if (!is.data.frame(frame) || ncol(frame) == 0L) frame <- data.frame(note = "No records.", stringsAsFactors = FALSE)

    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
    use_filter <- isTRUE(config$output$workbook_filters) && nrow(frame) > 0L
    openxlsx::writeData(wb, sheet, frame, withFilter = use_filter)
    openxlsx::addStyle(wb, sheet, header_style, rows = 1L, cols = seq_len(ncol(frame)), gridExpand = TRUE, stack = TRUE)
    if (isTRUE(config$output$workbook_freeze_header)) openxlsx::freezePane(wb, sheet, firstActiveRow = 2L)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(frame)), widths = excel_column_widths(frame))
    if (nrow(frame) > 0L) openxlsx::setRowHeights(wb, sheet, rows = 1L, heights = 24)

    if (identical(name, "data_dictionary") && "review_required" %in% names(frame) && nrow(frame) > 0L) {
      review_rows <- which(frame$review_required %in% TRUE) + 1L
      if (length(review_rows) > 0L) {
        openxlsx::addStyle(
          wb, sheet, warning_style,
          rows = review_rows,
          cols = seq_len(ncol(frame)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

prepare_output_directory <- function(output_dir, overwrite = FALSE) {
  output_dir <- path.expand(output_dir)
  if (dir.exists(output_dir)) {
    contents <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    if (length(contents) > 0L && !isTRUE(overwrite)) {
      stop("Output directory is not empty. Use --overwrite true or choose another directory: ", output_dir, call. = FALSE)
    }
    if (length(contents) > 0L && isTRUE(overwrite)) {
      known_artifacts <- file.path(
        output_dir,
        c(
          "csv", "metadata_report.html", "metadata.xlsx", "metadata.json",
          "data_dictionary.xlsx", "data_dictionary.json", "resolved_config.yml",
          "artifact_manifest.csv"
        )
      )
      existing_artifacts <- known_artifacts[file.exists(known_artifacts) | dir.exists(known_artifacts)]
      if (length(existing_artifacts) > 0L) unlink(existing_artifacts, recursive = TRUE, force = TRUE)
    }
  }
  safe_dir_create(output_dir)
}

artifact_group_for_path <- function(relative_paths) {
  groups <- rep("metadata", length(relative_paths))
  groups[grepl("metadata_report", relative_paths)] <- "combined report"
  groups[grepl("resolved_config|artifact_manifest", relative_paths)] <- "control"
  groups[grepl("data_dictionary|variable_dictionary|dictionary_schema", relative_paths)] <- "dictionary"
  groups[grepl("categorical_levels|value_labels|documentation_gaps", relative_paths)] <- "dictionary + metadata"
  groups
}

relative_artifact_path <- function(path, output_dir) {
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  normalized_root <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  prefix <- paste0(normalized_root, "/")
  if (startsWith(normalized_path, prefix)) substring(normalized_path, nchar(prefix) + 1L) else basename(normalized_path)
}

export_metadata_bundle <- function(results, output_dir, config) {
  output_dir <- prepare_output_directory(output_dir, config$output$overwrite)
  paths <- character()

  require_package("yaml", "resolved configuration output")
  config_path <- file.path(output_dir, "resolved_config.yml")
  yaml::write_yaml(config, config_path)
  paths <- c(paths, config_path)

  if (isTRUE(config$output$write_csv)) {
    paths <- c(paths, write_csv_outputs(results, output_dir, config))
  }
  if (isTRUE(config$output$write_json)) {
    paths <- c(
      paths,
      write_json_tables(
        dictionary_tables(results),
        file.path(output_dir, "data_dictionary.json"),
        extra = list(concept = "A structured field-level reference that organizes selected metadata.")
      ),
      write_json_tables(
        metadata_tables(results),
        file.path(output_dir, "metadata.json"),
        extra = list(
          concept = "Data about the files, datasets, variables, provenance, administration, and quality.",
          resolved_config = config
        )
      )
    )
  }
  if (isTRUE(config$output$write_excel)) {
    paths <- c(
      paths,
      write_excel_tables(
        dictionary_tables(results),
        file.path(output_dir, "data_dictionary.xlsx"),
        config,
        creator = "data_dictionary pipeline"
      ),
      write_excel_tables(
        metadata_tables(results),
        file.path(output_dir, "metadata.xlsx"),
        config,
        creator = "metadata pipeline"
      )
    )
  }
  if (isTRUE(config$output$write_html)) {
    paths <- c(paths, write_html_report(results, file.path(output_dir, "metadata_report.html"), config))
  }

  normalized_paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  relative_paths <- vapply(normalized_paths, relative_artifact_path, character(1L), output_dir = output_dir)
  manifest <- data.frame(
    artifact = basename(normalized_paths),
    relative_path = relative_paths,
    artifact_group = artifact_group_for_path(relative_paths),
    path = normalized_paths,
    size_bytes = as.numeric(file.info(normalized_paths)$size),
    md5 = unname(tools::md5sum(normalized_paths)),
    stringsAsFactors = FALSE
  )
  manifest_path <- file.path(output_dir, "artifact_manifest.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  c(paths, manifest_path)
}
