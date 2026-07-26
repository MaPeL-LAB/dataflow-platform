empty_data_dictionary <- function() {
  data.frame(
    dataset_id = character(), dataset_name = character(), label = character(),
    variable = character(), type = character(), value_label = character(),
    value_representation = character(), value_domain_status = character(),
    n_missing = integer(), percent_missing = numeric(),
    n_unique_non_missing = integer(), categorical_eligible = logical(), categorical_status = character(),
    semantic_type = character(), measurement_level = character(), business_definition = character(),
    definition_source = character(), documentation_status = character(), technical_definition = character(),
    inferred_validation_rules = character(), source_format = character(), storage_type = character(),
    source_display_format = character(), units = character(), nullable_observed = logical(),
    n_system_missing = integer(), n_blank_strings = integer(), potential_identifier = logical(),
    potential_sensitive = logical(), review_required = logical(), review_reason = character(),
    classification_reason = character(), examples = character(), variable_position = integer(),
    source_variable_name = character(), stringsAsFactors = FALSE
  )
}

dictionary_column_order <- function() {
  c(
    # Preserve the attached helper's analyst-facing layout first.
    "label", "variable", "type", "value_label", "n_missing",
    "dataset_id", "dataset_name", "value_representation", "value_domain_status",
    "percent_missing", "n_unique_non_missing", "categorical_eligible", "categorical_status",
    "semantic_type", "measurement_level", "business_definition", "definition_source",
    "documentation_status", "technical_definition", "inferred_validation_rules",
    "source_format", "storage_type", "source_display_format", "units", "nullable_observed",
    "n_system_missing", "n_blank_strings", "potential_identifier", "potential_sensitive",
    "review_required", "review_reason", "classification_reason", "examples",
    "variable_position", "source_variable_name"
  )
}

dictionary_column_labels <- function() {
  c(
    dataset_id = "Dataset Identifier",
    dataset_name = "Dataset Name",
    label = "Variable Label",
    variable = "Variable Name",
    type = "Variable Type",
    value_label = "Value Labels or Levels",
    value_representation = "Value Representation",
    value_domain_status = "Value Domain Authority",
    n_missing = "Number Missing per Variable",
    percent_missing = "Percent Missing",
    n_unique_non_missing = "Distinct Nonmissing Values",
    categorical_eligible = "Categorical Eligible",
    categorical_status = "Categorical Decision",
    semantic_type = "Semantic Type",
    measurement_level = "Measurement Level",
    business_definition = "Business Definition",
    definition_source = "Definition Source",
    documentation_status = "Documentation Status",
    technical_definition = "Technical Definition",
    inferred_validation_rules = "Observed or Inferred Rules",
    source_format = "Source Format",
    storage_type = "Storage Type",
    source_display_format = "Source Display Format",
    units = "Units",
    nullable_observed = "Missingness Observed",
    n_system_missing = "Native Missing Count",
    n_blank_strings = "Blank String Count",
    potential_identifier = "Potential Identifier",
    potential_sensitive = "Potential Sensitive Field",
    review_required = "Review Required",
    review_reason = "Review Reason",
    classification_reason = "Classification Reason",
    examples = "Representative Values",
    variable_position = "Variable Position",
    source_variable_name = "Source Variable Name"
  )
}

apply_dictionary_column_labels <- function(frame) {
  preferred <- dictionary_column_order()
  ordered <- c(intersect(preferred, names(frame)), setdiff(names(frame), preferred))
  frame <- frame[ordered]

  labels <- dictionary_column_labels()
  for (name in intersect(names(frame), names(labels))) {
    attr(frame[[name]], "label") <- unname(labels[[name]])
  }
  frame
}

empty_documentation_gaps <- function() {
  data.frame(
    dataset_id = character(), variable_position = integer(), variable_name = character(),
    gap_type = character(), severity = character(), message = character(),
    suggested_action = character(), stringsAsFactors = FALSE
  )
}

nonblank_text <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

collapse_preview <- function(values, total, config, suffix_table) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0L) return(NA_character_)
  inline_max <- as.integer(config$dictionary$inline_max_values)
  preview <- as.integer(config$dictionary$preview_values)
  if (total <= inline_max) return(paste(values, collapse = "; "))
  shown <- head(values, preview)
  paste0(
    paste(shown, collapse = "; "),
    sprintf("; ... [%d total; complete values in %s]", total, suffix_table)
  )
}

inline_value_labels <- function(value_labels, dataset_id, variable_position, config) {
  if (!is.data.frame(value_labels) || nrow(value_labels) == 0L) return(NA_character_)
  rows <- value_labels[
    value_labels$dataset_id == dataset_id & value_labels$variable_position == variable_position,
    , drop = FALSE
  ]
  if (nrow(rows) == 0L) return(NA_character_)
  rows <- rows[order(rows$label_order), , drop = FALSE]
  entries <- paste0(rows$raw_value, " = ", rows$value_label)
  collapse_preview(entries, nrow(rows), config, "value_labels")
}

inline_categorical_levels <- function(categorical_levels, dataset_id, variable_position, config) {
  if (!is.data.frame(categorical_levels) || nrow(categorical_levels) == 0L) return(NA_character_)
  rows <- categorical_levels[
    categorical_levels$dataset_id == dataset_id &
      categorical_levels$variable_position == variable_position &
      !categorical_levels$is_missing &
      !categorical_levels$is_other_unreported,
    , drop = FALSE
  ]
  if (nrow(rows) == 0L) return(NA_character_)
  rows <- rows[order(rows$level_order), , drop = FALSE]
  collapse_preview(rows$display_value, nrow(rows), config, "categorical_levels")
}

categorical_status_text <- function(row, config) {
  if (isTRUE(row$categorical_eligible)) {
    if (identical(row$semantic_type, "labelled_categorical")) {
      return(sprintf("Categorical: imported value-labelled field with %d label(s).", row$n_value_labels))
    }
    if (identical(row$semantic_type, "ordered_categorical")) {
      return(sprintf("Categorical: explicit ordered factor with %d defined level(s).", row$n_defined_factor_levels))
    }
    if (identical(row$semantic_type, "categorical") && !is.na(row$n_defined_factor_levels)) {
      return(sprintf("Categorical: explicit factor with %d defined level(s).", row$n_defined_factor_levels))
    }
    return(sprintf("Categorical: %d observed distinct nonmissing value(s).", row$n_unique_non_missing))
  }

  if (row$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")) {
    return(sprintf(
      "Not categorical: %d distinct nonblank value(s) exceed the configured limit of %d.",
      row$n_unique_non_missing,
      as.integer(config$profiling$max_categorical_levels)
    ))
  }
  if (identical(row$semantic_type, "text_all_missing")) {
    return("Not classified as categorical because no nonblank values were observed.")
  }
  "Not applicable to this variable type."
}

technical_definition_text <- function(row) {
  base <- switch(
    as.character(row$semantic_type),
    date = "Date field",
    datetime = "Date-time field",
    duration = "Duration field",
    ordered_categorical = "Ordered categorical field",
    categorical = "Categorical field",
    labelled_categorical = "Imported value-labelled categorical field",
    binary_categorical = "Binary TRUE/FALSE field",
    numeric_categorical = "Low-cardinality numeric categorical field",
    discrete_numeric = "Discrete numeric field",
    continuous_numeric = "Continuous numeric field",
    identifier = "Identifier-like text field",
    free_text = "Free-text field",
    high_cardinality_text = "High-cardinality text field",
    text_all_missing = "Text field with no observed nonblank values",
    nested_or_list = "Nested or list-column field",
    complex_numeric = "Complex-number field",
    raw_binary = "Raw binary field",
    paste0("Field classified as ", row$semantic_type)
  )
  paste0(base, ". ", row$classification_reason)
}

observed_validation_rules <- function(row, config) {
  if (!isTRUE(config$dictionary$include_inferred_validation_rules)) return(NA_character_)
  rules <- character()
  rules <- c(rules, if (row$n_missing > 0L) "Missing values were observed." else "No missing values were observed.")

  if (isTRUE(row$categorical_eligible)) {
    rules <- c(rules, "Observed or defined categories are documented in the value/level tables.")
  }
  if (row$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")) {
    rules <- c(rules, sprintf(
      "Treat as text rather than a categorical field under the configured %d-level rule.",
      as.integer(config$profiling$max_categorical_levels)
    ))
  }
  if (!is.na(row$numeric_min) && !is.na(row$numeric_max)) {
    rules <- c(rules, paste0("Observed numeric range: ", row$numeric_min, " to ", row$numeric_max, "."))
  }
  if (!is.na(row$date_min) && !is.na(row$date_max)) {
    rules <- c(rules, paste0("Observed date/time range: ", row$date_min, " to ", row$date_max, "."))
  }
  if (!is.na(row$string_max_length)) {
    rules <- c(rules, paste0("Maximum observed text length: ", row$string_max_length, " character(s)."))
  }
  paste(unique(rules), collapse = " ")
}

review_reason_text <- function(row, has_definition) {
  reasons <- character()
  if (!has_definition) reasons <- c(reasons, "business definition is missing")
  if (row$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")) {
    reasons <- c(reasons, "non-categorical character classification should be reviewed")
  }
  if (isTRUE(row$potential_identifier)) reasons <- c(reasons, "potential identifier")
  if (isTRUE(row$potential_sensitive)) reasons <- c(reasons, "potentially sensitive field")
  if (length(reasons) == 0L) NA_character_ else paste(unique(reasons), collapse = "; ")
}

value_representation_text <- function(row, has_imported_labels, has_levels) {
  if (has_imported_labels) return("code = label mapping")
  if (has_levels && !is.na(row$n_defined_factor_levels)) return("defined factor levels")
  if (has_levels && identical(row$semantic_type, "binary_categorical")) return("logical values")
  if (has_levels) return("observed categorical values")
  if (row$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")) {
    return("high-cardinality character field; values intentionally not enumerated")
  }
  "not applicable"
}

value_domain_status_text <- function(row, has_imported_labels, has_levels) {
  if (has_imported_labels) return("source-defined code-label mapping")
  if (has_levels && !is.na(row$n_defined_factor_levels)) return("source-defined factor domain")
  if (has_levels && identical(row$semantic_type, "binary_categorical")) return("type-defined logical domain")
  if (has_levels) return("observed values only; not an authoritative allowed-value rule")
  if (row$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")) {
    return("not enumerated because the field is not categorical")
  }
  "not applicable"
}

build_data_dictionary <- function(variable_metadata, dataset_metadata, categorical_levels, value_labels, config) {
  if (!is.data.frame(variable_metadata) || nrow(variable_metadata) == 0L) {
    return(apply_dictionary_column_labels(empty_data_dictionary()))
  }

  rows <- lapply(seq_len(nrow(variable_metadata)), function(i) {
    row <- variable_metadata[i, , drop = FALSE]
    dataset_index <- match(row$dataset_id, dataset_metadata$dataset_id)
    dataset_name <- if (!is.na(dataset_index)) dataset_metadata$dataset_name[[dataset_index]] else row$dataset_id
    source_format <- if (!is.na(dataset_index)) dataset_metadata$source_format[[dataset_index]] else NA_character_

    imported_labels <- inline_value_labels(value_labels, row$dataset_id, row$variable_position, config)
    levels <- inline_categorical_levels(categorical_levels, row$dataset_id, row$variable_position, config)
    has_imported_labels <- nonblank_text(imported_labels)
    has_levels <- nonblank_text(levels)
    displayed_values <- if (has_imported_labels) imported_labels else if (has_levels) levels else NA_character_

    label <- as.character(row$variable_label)
    has_label <- nonblank_text(label)
    use_label <- isTRUE(config$dictionary$use_variable_label_as_definition) && has_label
    business_definition <- if (use_label) label else NA_character_
    definition_source <- if (use_label) "source variable label (not independently validated)" else "not supplied"
    documentation_status <- if (use_label) "definition candidate available from source metadata" else "needs business definition"
    review_reason <- review_reason_text(row, use_label)

    data.frame(
      dataset_id = as.character(row$dataset_id),
      dataset_name = as.character(dataset_name),
      label = if (has_label) label else NA_character_,
      variable = as.character(row$variable_name),
      type = gsub(";", "/", as.character(row$r_class), fixed = TRUE),
      value_label = if (nonblank_text(displayed_values)) displayed_values else if (
        row$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")
      ) categorical_status_text(row, config) else NA_character_,
      value_representation = value_representation_text(row, has_imported_labels, has_levels),
      value_domain_status = value_domain_status_text(row, has_imported_labels, has_levels),
      n_missing = as.integer(row$n_missing),
      percent_missing = as.numeric(row$percent_missing),
      n_unique_non_missing = as.integer(row$n_unique_non_missing),
      categorical_eligible = as.logical(row$categorical_eligible),
      categorical_status = categorical_status_text(row, config),
      semantic_type = as.character(row$semantic_type),
      measurement_level = as.character(row$measurement_level),
      business_definition = business_definition,
      definition_source = definition_source,
      documentation_status = documentation_status,
      technical_definition = technical_definition_text(row),
      inferred_validation_rules = observed_validation_rules(row, config),
      source_format = as.character(source_format),
      storage_type = as.character(row$storage_type),
      source_display_format = as.character(row$source_display_format),
      units = as.character(row$units),
      nullable_observed = as.logical(row$n_missing > 0L),
      n_system_missing = as.integer(row$n_system_missing),
      n_blank_strings = as.integer(row$n_blank_strings),
      potential_identifier = as.logical(row$potential_identifier),
      potential_sensitive = as.logical(row$potential_sensitive),
      review_required = nonblank_text(review_reason),
      review_reason = review_reason,
      classification_reason = as.character(row$classification_reason),
      examples = as.character(row$examples),
      variable_position = as.integer(row$variable_position),
      source_variable_name = as.character(row$source_variable_name),
      stringsAsFactors = FALSE
    )
  })

  apply_dictionary_column_labels(bind_rows_fill(rows))
}

build_documentation_gaps <- function(data_dictionary, config) {
  if (!is.data.frame(data_dictionary) || nrow(data_dictionary) == 0L) return(empty_documentation_gaps())
  gaps <- list()

  if (isTRUE(config$dictionary$flag_missing_definitions)) {
    missing_definition <- !nonblank_text(data_dictionary$business_definition)
    for (i in which(missing_definition)) {
      gaps[[length(gaps) + 1L]] <- data.frame(
        dataset_id = data_dictionary$dataset_id[[i]],
        variable_position = data_dictionary$variable_position[[i]],
        variable_name = data_dictionary$variable[[i]],
        gap_type = "missing_business_definition",
        severity = "warning",
        message = "No source variable label or curated business definition was available. The pipeline did not invent one.",
        suggested_action = "Add an authoritative plain-language definition and responsible owner/steward.",
        stringsAsFactors = FALSE
      )
    }
  }

  high_card <- data_dictionary$semantic_type %in% c("high_cardinality_text", "free_text", "identifier")
  for (i in which(high_card)) {
    gaps[[length(gaps) + 1L]] <- data.frame(
      dataset_id = data_dictionary$dataset_id[[i]],
      variable_position = data_dictionary$variable_position[[i]],
      variable_name = data_dictionary$variable[[i]],
      gap_type = "classification_review",
      severity = "info",
      message = data_dictionary$categorical_status[[i]],
      suggested_action = "Confirm the field should remain text/identifier rather than be governed by a controlled category list.",
      stringsAsFactors = FALSE
    )
  }

  if (length(gaps) == 0L) empty_documentation_gaps() else bind_rows_fill(gaps)
}

data_dictionary_schema <- function() {
  definitions <- c(
    dataset_id = "Stable identifier for the imported dataset, worksheet, or object.",
    dataset_name = "Dataset, worksheet-derived, or object-derived display name.",
    label = "Variable label preserved from source metadata, when available.",
    variable = "Dictionary field name used in the imported dataset.",
    type = "R class or classes after import.",
    value_label = "Inline code-to-label mapping or categorical levels. Large mappings are previewed and stored completely in companion tables.",
    value_representation = "Explains whether value_label contains mappings, factor levels, observed categories, or an exclusion message.",
    value_domain_status = "Distinguishes source-defined or type-defined domains from values that were merely observed in the current data.",
    n_missing = "Effective missing count, including blank strings when blank_as_missing is enabled.",
    percent_missing = "Effective missing percentage.",
    n_unique_non_missing = "Count of distinct nonmissing/nonblank values.",
    categorical_eligible = "Whether the field qualifies for categorical treatment under the configured and source-defined rules.",
    categorical_status = "Readable categorical decision, including the high-cardinality threshold result.",
    semantic_type = "Inferred technical role such as categorical, identifier, free_text, date, or continuous_numeric.",
    measurement_level = "Inferred measurement level such as nominal, ordinal, continuous, date, or text.",
    business_definition = "Plain-language definition candidate copied from the source variable label when configured; otherwise left blank for curation.",
    definition_source = "Origin of the business definition.",
    documentation_status = "Whether a definition candidate is available and whether further curation is needed.",
    technical_definition = "Automatically generated technical explanation based on observed structure and classification.",
    inferred_validation_rules = "Observed constraints and ranges; these are not asserted as authoritative business rules.",
    source_format = "Format of the source file.",
    storage_type = "Underlying R storage type.",
    source_display_format = "Imported SAS/SPSS/Stata or R display-format attribute, when available.",
    units = "Imported units attribute, when available.",
    nullable_observed = "TRUE when missing or configured blank values were observed.",
    n_system_missing = "Count of native missing values before blank-string handling.",
    n_blank_strings = "Count of blank or whitespace-only strings.",
    potential_identifier = "Heuristic indicator that the field may identify records or people.",
    potential_sensitive = "Heuristic indicator that the field may contain sensitive information.",
    review_required = "TRUE when documentation, classification, identifier, or sensitivity review is recommended.",
    review_reason = "Reasons for recommended review.",
    classification_reason = "Exact rule and evidence used for technical classification.",
    examples = "Representative values, subject to privacy masking.",
    variable_position = "One-based source column position; disambiguates duplicate or blank names.",
    source_variable_name = "Original source column name before display-name fallback."
  )
  schema <- data.frame(
    column_name = names(definitions),
    definition = unname(definitions),
    stringsAsFactors = FALSE
  )
  preferred <- dictionary_column_order()
  schema[order(match(schema$column_name, preferred, nomatch = length(preferred) + 1L)), , drop = FALSE]
}

in_memory_dictionary_config <- function(max_categorical_levels = 10L) {
  list(
    profiling = list(
      blank_as_missing = TRUE,
      max_categorical_levels = as.integer(max_categorical_levels),
      max_categorical_ratio = 1,
      factor_always_categorical = TRUE,
      labelled_always_categorical = TRUE,
      logical_as_categorical = TRUE,
      numeric_low_cardinality_as_categorical = FALSE,
      numeric_max_categorical_levels = as.integer(max_categorical_levels),
      numeric_max_categorical_ratio = 0.05,
      id_uniqueness_threshold = 0.98,
      id_min_non_missing = 10,
      id_max_mean_length = 80,
      long_text_length_threshold = 100,
      sample_values = 5,
      max_levels_reported = 50,
      include_missing_level = TRUE,
      include_level_frequencies = TRUE,
      include_quantiles = TRUE,
      high_missingness_threshold = 0.50,
      check_duplicate_rows = FALSE,
      duplicate_check_max_rows = 100000,
      check_whitespace = TRUE,
      check_infinite_values = TRUE
    ),
    dictionary = list(
      inline_max_values = as.integer(max_categorical_levels),
      preview_values = min(5L, as.integer(max_categorical_levels)),
      use_variable_label_as_definition = TRUE,
      flag_missing_definitions = TRUE,
      include_inferred_validation_rules = TRUE
    ),
    privacy = list(
      include_examples = TRUE,
      mask_potential_identifiers = TRUE,
      mask_potential_sensitive = TRUE,
      masked_token = "<masked>",
      identifier_name_patterns = c(
        '(^|_)(id|key|uuid|guid|record_number|row_number|case_number|reference_number|account_number)(_|$)',
        '(^|_)(code|number|no)$'
      ),
      sensitive_name_patterns = c(
        '(^|_)(name|first_name|last_name|full_name|email|e_mail|phone|mobile|telephone)(_|$)',
        '(^|_)(address|street|postcode|postal_code|zip|ssn|national_id|passport)(_|$)',
        '(^|_)(dob|birth_date|date_of_birth|patient|client|customer|employee)(_|$)'
      )
    )
  )
}

# Compatibility helper inspired by the original analyst-facing function.
# It is deliberately dependency-light and never installs packages at runtime.
func_dictionary <- function(DATA, max_categorical_levels = 10L, dataset_id = "in_memory", dataset_name = dataset_id) {
  if (!is.data.frame(DATA)) {
    DATA <- as.data.frame(DATA, stringsAsFactors = FALSE, check.names = FALSE)
  }
  config <- in_memory_dictionary_config(max_categorical_levels)
  variables <- list()
  levels <- list()
  labels <- list()
  source_names <- names(DATA)
  if (is.null(source_names)) source_names <- rep("", ncol(DATA))

  for (i in seq_len(ncol(DATA))) {
    source_name <- as.character(source_names[[i]] %||% "")
    display_name <- if (nzchar(source_name)) source_name else paste0("<unnamed_", i, ">")
    item <- profile_variable(DATA[[i]], dataset_id, i, source_name, display_name, config)
    variables[[length(variables) + 1L]] <- item$variable
    levels[[length(levels) + 1L]] <- item$categorical_levels
    labels[[length(labels) + 1L]] <- item$value_labels
  }

  variable_metadata <- bind_rows_fill(variables)
  categorical_levels <- if (length(levels) == 0L) empty_categorical_levels() else bind_rows_fill(levels)
  value_labels <- if (length(labels) == 0L) empty_value_labels() else bind_rows_fill(labels)
  dataset_metadata <- data.frame(
    dataset_id = dataset_id,
    dataset_name = dataset_name,
    source_format = "in_memory_R_object",
    stringsAsFactors = FALSE
  )

  build_data_dictionary(variable_metadata, dataset_metadata, categorical_levels, value_labels, config)
}
