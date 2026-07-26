matches_any_pattern <- function(value, patterns) {
  if (is.null(patterns) || length(patterns) == 0L) return(FALSE)
  any(vapply(patterns, function(pattern) grepl(pattern, value, perl = TRUE, ignore.case = TRUE), logical(1L)))
}

normalized_variable_name <- function(name) {
  name <- tolower(as.character(name %||% ""))
  name <- gsub("[^a-z0-9]+", "_", name)
  gsub("^_+|_+$", "", name)
}

character_pattern_ratio <- function(values, pattern) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values) == 0L) return(0)
  mean(grepl(pattern, values, perl = TRUE))
}

detect_role_hints <- function(x, variable_name, stats, config) {
  normalized_name <- normalized_variable_name(variable_name)
  id_name_hint <- matches_any_pattern(normalized_name, config$privacy$identifier_name_patterns)
  sensitive_name_hint <- matches_any_pattern(normalized_name, config$privacy$sensitive_name_patterns)

  values <- if (is.character(x) || is.factor(x)) as.character(x[!stats$effective_missing]) else character()
  sample_values <- head(unique(values), 500L)
  email_ratio <- character_pattern_ratio(sample_values, "^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$")
  phone_ratio <- character_pattern_ratio(sample_values, "^\\+?[0-9][0-9 ()-]{6,}$")
  uuid_ratio <- character_pattern_ratio(sample_values, "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$")

  high_uniqueness <- !is.na(stats$unique_ratio) &&
    stats$n_non_missing >= as.numeric(config$profiling$id_min_non_missing) &&
    stats$unique_ratio >= as.numeric(config$profiling$id_uniqueness_threshold)

  short_values <- is.na(stats$mean_length) || stats$mean_length <= as.numeric(config$profiling$id_max_mean_length)
  long_text <- (!is.na(stats$max_length) && stats$max_length >= as.numeric(config$profiling$long_text_length_threshold)) ||
    (!is.na(stats$mean_length) && stats$mean_length >= as.numeric(config$profiling$long_text_length_threshold) / 2)

  uniqueness_supports_id <- !is.na(stats$unique_ratio) && stats$unique_ratio >= 0.5
  strong_identifier <- (id_name_hint && uniqueness_supports_id) ||
    (sensitive_name_hint && high_uniqueness) ||
    uuid_ratio >= 0.8
  identifier_candidate <- strong_identifier ||
    ((is.character(x) || is.factor(x)) && high_uniqueness && short_values && !long_text)
  potential_sensitive <- sensitive_name_hint || email_ratio >= 0.5 || phone_ratio >= 0.8

  reasons <- character()
  if (id_name_hint) reasons <- c(reasons, "identifier-like variable name")
  if (sensitive_name_hint) reasons <- c(reasons, "sensitive-data name pattern")
  if (email_ratio >= 0.5) reasons <- c(reasons, "email-like values")
  if (phone_ratio >= 0.8) reasons <- c(reasons, "phone-like values")
  if (uuid_ratio >= 0.8) reasons <- c(reasons, "UUID-like values")
  if (high_uniqueness) reasons <- c(reasons, "very high uniqueness")

  list(
    id_name_hint = id_name_hint,
    sensitive_name_hint = sensitive_name_hint,
    high_uniqueness = high_uniqueness,
    long_text = long_text,
    strong_identifier = strong_identifier,
    identifier_candidate = identifier_candidate,
    potential_sensitive = potential_sensitive,
    reason = paste(unique(reasons), collapse = "; ")
  )
}

cardinality_class <- function(stats, config) {
  if (stats$all_missing) return("all_missing")
  if (stats$n_unique == 1L) return("constant")
  if (stats$n_unique == 2L) return("binary")
  if (!is.na(stats$unique_ratio) && stats$unique_ratio == 1) return("all_unique")
  if (stats$n_unique <= as.numeric(config$profiling$max_categorical_levels)) return("low")
  if (stats$n_unique <= 50L) return("moderate")
  "high"
}

classify_variable <- function(x, variable_name, stats, role_hints, config) {
  max_levels <- as.integer(config$profiling$max_categorical_levels)
  max_ratio <- as.numeric(config$profiling$max_categorical_ratio)
  unique_ratio <- stats$unique_ratio %||% NA_real_
  has_value_labels <- stats$n_value_labels > 0L
  is_labelled <- inherits(x, c("haven_labelled", "haven_labelled_spss", "labelled"))

  result <- list(
    semantic_type = "unknown",
    measurement_level = "unknown",
    categorical_eligible = FALSE,
    classification_reason = "No classification rule matched."
  )

  if (inherits(x, "Date")) {
    result$semantic_type <- "date"
    result$measurement_level <- "date"
    result$classification_reason <- "R Date class."
    return(result)
  }
  if (inherits(x, c("POSIXct", "POSIXlt"))) {
    result$semantic_type <- "datetime"
    result$measurement_level <- "datetime"
    result$classification_reason <- "R POSIX date-time class."
    return(result)
  }
  if (inherits(x, "difftime")) {
    result$semantic_type <- "duration"
    result$measurement_level <- "continuous"
    result$classification_reason <- "R difftime duration class."
    return(result)
  }
  if (is.ordered(x)) {
    result$semantic_type <- "ordered_categorical"
    result$measurement_level <- "ordinal"
    result$categorical_eligible <- TRUE
    result$classification_reason <- sprintf("Explicit ordered factor with %d observed distinct value(s).", stats$n_unique)
    return(result)
  }
  if (is.factor(x) && isTRUE(config$profiling$factor_always_categorical)) {
    result$semantic_type <- "categorical"
    result$measurement_level <- "nominal"
    result$categorical_eligible <- TRUE
    result$classification_reason <- sprintf("Explicit factor with %d defined level(s).", length(levels(x)))
    return(result)
  }
  if (is_labelled && has_value_labels && isTRUE(config$profiling$labelled_always_categorical)) {
    result$semantic_type <- "labelled_categorical"
    result$measurement_level <- "nominal"
    result$categorical_eligible <- TRUE
    result$classification_reason <- sprintf("Imported labelled variable with %d value label(s).", stats$n_value_labels)
    return(result)
  }
  if (is.logical(x) && isTRUE(config$profiling$logical_as_categorical)) {
    result$semantic_type <- "binary_categorical"
    result$measurement_level <- "nominal"
    result$categorical_eligible <- TRUE
    result$classification_reason <- "Logical TRUE/FALSE variable."
    return(result)
  }

  if (is.character(x)) {
    if (stats$all_missing) {
      result$semantic_type <- "text_all_missing"
      result$measurement_level <- "text"
      result$classification_reason <- "Character variable contains no nonblank values; categorical eligibility cannot be established."
      return(result)
    }

    within_level_limit <- stats$n_unique <= max_levels
    within_ratio_limit <- is.na(unique_ratio) || unique_ratio <= max_ratio

    if (within_level_limit && within_ratio_limit) {
      result$semantic_type <- "categorical"
      result$measurement_level <- "nominal"
      result$categorical_eligible <- TRUE
      result$classification_reason <- sprintf(
        "Character variable has %d distinct nonmissing value(s), within the configured categorical limit of %d.",
        stats$n_unique, max_levels
      )
      return(result)
    }

    result$categorical_eligible <- FALSE
    result$measurement_level <- "text"
    if (role_hints$strong_identifier) {
      result$semantic_type <- "identifier"
    } else if (role_hints$long_text) {
      result$semantic_type <- "free_text"
    } else {
      result$semantic_type <- "high_cardinality_text"
    }
    ratio_clause <- if (!within_ratio_limit) sprintf(" and uniqueness ratio %.3f exceeds %.3f", unique_ratio, max_ratio) else ""
    result$classification_reason <- sprintf(
      "Character variable has %d distinct nonmissing value(s), exceeding the configured categorical limit of %d%s; it is not treated as categorical.",
      stats$n_unique, max_levels, ratio_clause
    )
    return(result)
  }

  if ((is.numeric(x) || is.integer(x)) && !is.complex(x)) {
    numeric_categorical <- isTRUE(config$profiling$numeric_low_cardinality_as_categorical) &&
      stats$n_unique <= as.numeric(config$profiling$numeric_max_categorical_levels) &&
      (is.na(unique_ratio) || unique_ratio <= as.numeric(config$profiling$numeric_max_categorical_ratio))

    if (numeric_categorical) {
      result$semantic_type <- "numeric_categorical"
      result$measurement_level <- "nominal"
      result$categorical_eligible <- TRUE
      result$classification_reason <- "Numeric low-cardinality rule is enabled and its level and ratio limits were met."
    } else {
      result$semantic_type <- if (is.integer(x)) "discrete_numeric" else "continuous_numeric"
      result$measurement_level <- if (is.integer(x)) "discrete" else "continuous"
      result$classification_reason <- if (is.integer(x)) "Integer variable without categorical labels." else "Numeric variable without categorical labels."
    }
    return(result)
  }

  if (is.complex(x)) {
    result$semantic_type <- "complex_numeric"
    result$measurement_level <- "continuous"
    result$classification_reason <- "Complex numeric vector."
    return(result)
  }
  if (is.list(x)) {
    result$semantic_type <- "nested_or_list"
    result$measurement_level <- "nested"
    result$classification_reason <- "List or nested column; scalar categorical rules do not apply."
    return(result)
  }
  if (is.raw(x)) {
    result$semantic_type <- "raw_binary"
    result$measurement_level <- "binary_data"
    result$classification_reason <- "Raw byte vector."
    return(result)
  }

  result$semantic_type <- paste0("other_", typeof(x))
  result$classification_reason <- paste("Unrecognized class:", paste(class(x), collapse = ", "))
  result
}
