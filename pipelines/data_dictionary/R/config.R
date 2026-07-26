validate_data_dictionary_config <- function(config) {
  required_sections <- c("input", "profiling", "dictionary", "metadata", "privacy", "output")
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0L) {
    stop("Missing configuration sections: ", paste(missing_sections, collapse = ", "), call. = FALSE)
  }

  p <- config$profiling
  d <- config$dictionary
  checks <- list(
    max_categorical_levels = c(p$max_categorical_levels, 1, Inf),
    max_categorical_ratio = c(p$max_categorical_ratio, 0, 1),
    numeric_max_categorical_levels = c(p$numeric_max_categorical_levels, 1, Inf),
    numeric_max_categorical_ratio = c(p$numeric_max_categorical_ratio, 0, 1),
    id_uniqueness_threshold = c(p$id_uniqueness_threshold, 0, 1),
    id_min_non_missing = c(p$id_min_non_missing, 1, Inf),
    high_missingness_threshold = c(p$high_missingness_threshold, 0, 1),
    sample_values = c(p$sample_values, 0, Inf),
    max_levels_reported = c(p$max_levels_reported, 1, Inf),
    inline_max_values = c(d$inline_max_values, 1, Inf),
    preview_values = c(d$preview_values, 1, Inf)
  )

  for (name in names(checks)) {
    values <- checks[[name]]
    value <- suppressWarnings(as.numeric(values[[1L]]))
    if (is.na(value) || value < values[[2L]] || value > values[[3L]]) {
      stop(sprintf("Invalid configuration value for '%s': %s", name, as.character(values[[1L]])), call. = FALSE)
    }
  }

  if (as.numeric(d$preview_values) > as.numeric(d$inline_max_values)) {
    stop("dictionary.preview_values cannot exceed dictionary.inline_max_values.", call. = FALSE)
  }

  delimiter <- as.character(config$input$text_delimiter %||% "\t")
  if (length(delimiter) != 1L || nchar(delimiter, type = "chars") != 1L) {
    stop("input.text_delimiter must be exactly one character.", call. = FALSE)
  }

  invisible(config)
}

load_data_dictionary_config <- function(default_path, user_path = NULL, overrides = list()) {
  config <- read_yaml_checked(default_path)
  if (!is.null(user_path) && nzchar(user_path)) {
    user_path <- normalizePath(user_path, winslash = "/", mustWork = TRUE)
    config <- deep_merge(config, read_yaml_checked(user_path))
  }
  config <- deep_merge(config, overrides)
  validate_data_dictionary_config(config)
  config
}

selector_values <- function(value) {
  if (is.null(value) || length(value) == 0L) return("all")
  if (is.character(value) && length(value) == 1L && tolower(value) == "all") return("all")
  values <- split_cli_values(value)
  if (length(values) == 0L) "all" else values
}

build_cli_overrides <- function(args) {
  overrides <- list()

  set_nested <- function(section, name, value) {
    if (is.null(overrides[[section]])) overrides[[section]] <<- list()
    overrides[[section]][[name]] <<- value
  }

  if (!is.null(args$recursive)) set_nested("input", "recursive", as_flag(args$recursive))
  if (!is.null(args$include_hidden)) set_nested("input", "include_hidden", as_flag(args$include_hidden))
  if (!is.null(args$continue_on_error)) set_nested("input", "continue_on_error", as_flag(args$continue_on_error))
  if (!is.null(args$excel_sheets)) set_nested("input", "excel_sheets", selector_values(args$excel_sheets))
  if (!is.null(args$r_objects)) set_nested("input", "r_objects", selector_values(args$r_objects))
  if (!is.null(args$allow_unsafe_pickle)) set_nested("input", "allow_unsafe_pickle", as_flag(args$allow_unsafe_pickle))
  if (!is.null(args$auto_sas_catalog)) set_nested("input", "auto_sas_catalog", as_flag(args$auto_sas_catalog))
  if (!is.null(args$sas_catalog)) set_nested("input", "sas_catalog", as.character(args$sas_catalog[[length(args$sas_catalog)]]))
  if (!is.null(args$sas_catalog_encoding)) set_nested("input", "sas_catalog_encoding", as.character(args$sas_catalog_encoding[[length(args$sas_catalog_encoding)]]))
  if (!is.null(args$haven_encoding)) set_nested("input", "haven_encoding", as.character(args$haven_encoding[[length(args$haven_encoding)]]))
  if (!is.null(args$compute_md5)) set_nested("input", "compute_md5", as_flag(args$compute_md5))
  if (!is.null(args$n_max)) set_nested("input", "n_max", as_number(args$n_max))
  if (!is.null(args$text_delimiter)) set_nested("input", "text_delimiter", as.character(args$text_delimiter[[length(args$text_delimiter)]]))
  if (!is.null(args$text_encoding)) set_nested("input", "text_encoding", as.character(args$text_encoding[[length(args$text_encoding)]]))

  if (!is.null(args$max_categorical_levels)) {
    set_nested("profiling", "max_categorical_levels", as_number(args$max_categorical_levels))
  }
  if (!is.null(args$max_categorical_ratio)) {
    set_nested("profiling", "max_categorical_ratio", as_number(args$max_categorical_ratio))
  }
  if (!is.null(args$id_uniqueness_threshold)) {
    set_nested("profiling", "id_uniqueness_threshold", as_number(args$id_uniqueness_threshold))
  }
  if (!is.null(args$high_missingness_threshold)) {
    set_nested("profiling", "high_missingness_threshold", as_number(args$high_missingness_threshold))
  }
  if (!is.null(args$inline_max_values)) {
    set_nested("dictionary", "inline_max_values", as_number(args$inline_max_values))
  }
  if (!is.null(args$preview_values)) {
    set_nested("dictionary", "preview_values", as_number(args$preview_values))
  }
  if (!is.null(args$use_variable_label_as_definition)) {
    set_nested("dictionary", "use_variable_label_as_definition", as_flag(args$use_variable_label_as_definition))
  }
  if (!is.null(args$flag_missing_definitions)) {
    set_nested("dictionary", "flag_missing_definitions", as_flag(args$flag_missing_definitions))
  }

  if (!is.null(args$include_examples)) {
    set_nested("privacy", "include_examples", as_flag(args$include_examples))
  }
  if (!is.null(args$mask_potential_identifiers)) {
    set_nested("privacy", "mask_potential_identifiers", as_flag(args$mask_potential_identifiers))
  }
  if (!is.null(args$mask_potential_sensitive)) {
    set_nested("privacy", "mask_potential_sensitive", as_flag(args$mask_potential_sensitive))
  }

  metadata_args <- c(
    project_name = "project_name",
    project_description = "project_description",
    author = "author",
    data_owner = "data_owner",
    data_steward = "data_steward",
    source_system = "source_system",
    access_classification = "access_classification",
    access_permissions = "access_permissions",
    license = "license"
  )
  for (arg_name in names(metadata_args)) {
    if (!is.null(args[[arg_name]])) {
      value <- as.character(args[[arg_name]][[length(args[[arg_name]])]])
      set_nested("metadata", metadata_args[[arg_name]], value)
    }
  }

  if (!is.null(args$tags)) set_nested("metadata", "tags", split_cli_values(args$tags))

  if (!is.null(args$overwrite)) set_nested("output", "overwrite", as_flag(args$overwrite))
  if (!is.null(args$write_csv)) set_nested("output", "write_csv", as_flag(args$write_csv))
  if (!is.null(args$write_json)) set_nested("output", "write_json", as_flag(args$write_json))
  if (!is.null(args$write_excel)) set_nested("output", "write_excel", as_flag(args$write_excel))
  if (!is.null(args$write_html)) set_nested("output", "write_html", as_flag(args$write_html))
  if (!is.null(args$write_legacy_variable_dictionary)) {
    set_nested("output", "write_legacy_variable_dictionary", as_flag(args$write_legacy_variable_dictionary))
  }

  overrides
}
