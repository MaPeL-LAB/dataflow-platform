safe_missing_mask <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    return(vapply(x, function(item) {
      tryCatch(is.null(item) || length(item) == 0L || all(is.na(item)), error = function(e) FALSE)
    }, logical(1L)))
  }
  mask <- tryCatch(is.na(x), error = function(e) rep(FALSE, length(x)))
  if (is.matrix(mask)) mask <- apply(mask, 1L, all)
  as.logical(mask)
}

format_value_scalar <- function(value) {
  if (length(value) == 0L || is.null(value)) return("<empty>")
  if (length(value) > 1L) return(truncate_text(json_string(value), 160L))
  is_tagged <- requireNamespace("haven", quietly = TRUE) &&
    isTRUE(tryCatch(haven::is_tagged_na(value), error = function(e) FALSE))
  if (is_tagged) return(paste0("NA(", as.character(haven::na_tag(value)), ")"))
  if (is.na(value)) return("<missing>")
  if (inherits(value, "Date")) return(format(value, "%Y-%m-%d"))
  if (inherits(value, "POSIXt")) return(format(value, tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ"))
  if (is.factor(value)) return(as.character(value))
  if (is.numeric(value)) return(format(value, scientific = FALSE, trim = TRUE, digits = 15L))
  truncate_text(as.character(value), 160L)
}

canonical_values <- function(x) {
  if (is.list(x) && !is.data.frame(x)) return(vapply(x, json_string, character(1L)))
  if (inherits(x, "POSIXt")) return(format(x, tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%OSZ"))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (is.factor(x)) return(as.character(x))
  as.character(x)
}

empty_categorical_levels <- function() {
  data.frame(
    dataset_id = character(), variable_position = integer(), variable_name = character(),
    level_order = integer(), raw_value = character(), value_label = character(),
    display_value = character(), count = integer(), percent_of_nonmissing = numeric(),
    percent_of_all = numeric(), is_missing = logical(), is_other_unreported = logical(),
    stringsAsFactors = FALSE
  )
}

empty_value_labels <- function() {
  data.frame(
    dataset_id = character(), variable_position = integer(), variable_name = character(),
    label_order = integer(), raw_value = character(), value_label = character(),
    stringsAsFactors = FALSE
  )
}

empty_quality_issues <- function() {
  data.frame(
    dataset_id = character(), variable_position = integer(), variable_name = character(),
    severity = character(), issue_type = character(), metric = numeric(),
    threshold = numeric(), message = character(), stringsAsFactors = FALSE
  )
}

empty_import_problems <- function() {
  data.frame(
    dataset_id = character(), source_file = character(), row = integer(), col = integer(),
    expected = character(), actual = character(), problem_file = character(),
    stringsAsFactors = FALSE
  )
}

extract_value_labels <- function(x, dataset_id, variable_position, variable_name) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels) || length(labels) == 0L) return(empty_value_labels())
  label_names <- names(labels)
  if (is.null(label_names)) label_names <- rep(NA_character_, length(labels))
  data.frame(
    dataset_id = dataset_id,
    variable_position = variable_position,
    variable_name = variable_name,
    label_order = seq_along(labels),
    raw_value = vapply(as.list(unname(labels)), format_value_scalar, character(1L)),
    value_label = as.character(label_names),
    stringsAsFactors = FALSE
  )
}

summarize_tagged_missing <- function(x) {
  if (!requireNamespace("haven", quietly = TRUE)) return(NA_character_)
  tagged <- tryCatch(haven::is_tagged_na(x), error = function(e) rep(FALSE, length(x)))
  if (!any(tagged)) return(NA_character_)
  tags <- vapply(x[tagged], function(value) as.character(haven::na_tag(value)), character(1L))
  counts <- sort(table(tags), decreasing = TRUE)
  paste(paste0(names(counts), "=", as.integer(counts)), collapse = "; ")
}

profile_basic_stats <- function(x, config) {
  n <- length(x)
  missing <- safe_missing_mask(x)
  is_character_like <- is.character(x) || is.factor(x)
  character_values <- if (is_character_like) as.character(x) else NULL
  blank <- if (is_character_like) !missing & !nzchar(trimws(character_values)) else rep(FALSE, n)
  effective_missing <- missing | (isTRUE(config$profiling$blank_as_missing) & blank)
  non_missing <- !effective_missing
  values <- x[non_missing]
  canonical <- canonical_values(values)
  n_unique <- length(unique(canonical))
  n_non_missing <- sum(non_missing)
  unique_ratio <- if (n_non_missing == 0L) NA_real_ else n_unique / n_non_missing

  lengths <- if (is_character_like && n_non_missing > 0L) nchar(as.character(values), type = "chars", allowNA = TRUE) else numeric()
  whitespace <- if (is_character_like) {
    sum(!missing & as.character(x) != trimws(as.character(x)), na.rm = TRUE)
  } else 0L

  labels <- attr(x, "labels", exact = TRUE)
  n_value_labels <- if (is.null(labels)) 0L else length(labels)

  list(
    n = n,
    missing = missing,
    blank = blank,
    effective_missing = effective_missing,
    n_missing = sum(missing),
    n_blank = sum(blank),
    n_effective_missing = sum(effective_missing),
    n_non_missing = n_non_missing,
    n_unique = n_unique,
    unique_ratio = unique_ratio,
    all_missing = n_non_missing == 0L,
    constant = n_non_missing > 0L && n_unique == 1L,
    mean_length = if (length(lengths) == 0L) NA_real_ else mean(lengths, na.rm = TRUE),
    min_length = if (length(lengths) == 0L) NA_real_ else min(lengths, na.rm = TRUE),
    max_length = if (length(lengths) == 0L) NA_real_ else max(lengths, na.rm = TRUE),
    whitespace_count = whitespace,
    n_value_labels = n_value_labels
  )
}

numeric_summary <- function(x, effective_missing, include_quantiles = TRUE) {
  if (!(is.numeric(x) || is.integer(x)) || is.complex(x) || inherits(x, c("Date", "POSIXt", "difftime"))) {
    return(list(min = NA_real_, q1 = NA_real_, median = NA_real_, mean = NA_real_, q3 = NA_real_, max = NA_real_, sd = NA_real_, infinite = 0L))
  }
  values <- as.numeric(x[!effective_missing])
  infinite <- sum(is.infinite(values))
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(list(min = NA_real_, q1 = NA_real_, median = NA_real_, mean = NA_real_, q3 = NA_real_, max = NA_real_, sd = NA_real_, infinite = infinite))
  }
  quantiles <- if (isTRUE(include_quantiles)) {
    stats::quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE, type = 7)
  } else c(NA_real_, stats::median(values), NA_real_)
  list(
    min = min(values),
    q1 = quantiles[[1L]],
    median = quantiles[[2L]],
    mean = mean(values),
    q3 = quantiles[[3L]],
    max = max(values),
    sd = if (length(values) <= 1L) NA_real_ else stats::sd(values),
    infinite = infinite
  )
}

date_summary <- function(x, effective_missing) {
  if (!inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(list(min = NA_character_, max = NA_character_, timezone = NA_character_))
  }
  values <- x[!effective_missing]
  if (length(values) == 0L) return(list(min = NA_character_, max = NA_character_, timezone = NA_character_))
  if (inherits(x, "Date")) {
    return(list(min = format(min(values), "%Y-%m-%d"), max = format(max(values), "%Y-%m-%d"), timezone = NA_character_))
  }
  timezone <- attr(x, "tzone", exact = TRUE) %||% ""
  list(
    min = format(min(values), tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ"),
    max = format(max(values), tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ"),
    timezone = paste(timezone, collapse = ";")
  )
}

variable_examples <- function(x, stats, role_hints, config) {
  if (!isTRUE(config$privacy$include_examples) || stats$n_non_missing == 0L) return(NA_character_)
  should_mask <- (isTRUE(config$privacy$mask_potential_identifiers) && role_hints$identifier_candidate) ||
    (isTRUE(config$privacy$mask_potential_sensitive) && role_hints$potential_sensitive)
  if (should_mask) return(as.character(config$privacy$masked_token %||% "<masked>"))

  values <- x[!stats$effective_missing]
  keys <- canonical_values(values)
  keep <- !duplicated(keys)
  values <- values[keep]
  values <- head(values, as.integer(config$profiling$sample_values))
  if (length(values) == 0L) return(NA_character_)
  paste(vapply(as.list(values), format_value_scalar, character(1L)), collapse = " | ")
}

format_user_missing_values <- function(x) {
  values <- attr(x, "na_values", exact = TRUE)
  if (is.null(values) || length(values) == 0L) return(NA_character_)
  paste(vapply(as.list(values), format_value_scalar, character(1L)), collapse = "; ")
}

format_user_missing_range <- function(x) {
  range <- attr(x, "na_range", exact = TRUE)
  if (is.null(range) || length(range) == 0L) return(NA_character_)
  paste(vapply(as.list(range), format_value_scalar, character(1L)), collapse = " to ")
}

source_display_format <- function(x) {
  formats <- c(
    attr(x, "format.stata", exact = TRUE),
    attr(x, "format.spss", exact = TRUE),
    attr(x, "format.sas", exact = TRUE),
    attr(x, "format", exact = TRUE)
  )
  formats <- as.character(formats)
  formats <- unique(formats[!is.na(formats) & nzchar(formats)])
  if (length(formats) == 0L) NA_character_ else paste(formats, collapse = "; ")
}

make_issue <- function(dataset_id, variable_position = NA_integer_, variable_name = NA_character_,
                       severity, issue_type, metric = NA_real_, threshold = NA_real_, message) {
  data.frame(
    dataset_id = dataset_id,
    variable_position = variable_position,
    variable_name = variable_name,
    severity = severity,
    issue_type = issue_type,
    metric = metric,
    threshold = threshold,
    message = message,
    stringsAsFactors = FALSE
  )
}

build_categorical_levels <- function(x, dataset_id, variable_position, variable_name, stats, classification, config) {
  if (!isTRUE(config$profiling$include_level_frequencies) || !isTRUE(classification$categorical_eligible)) {
    return(empty_categorical_levels())
  }

  observed <- x[!stats$effective_missing]
  observed_keys <- canonical_values(observed)
  counts <- sort(table(observed_keys), decreasing = TRUE)

  if (is.factor(x)) {
    defined <- levels(x)
    factor_counts <- table(factor(as.character(observed), levels = defined))
    keys <- names(factor_counts)
    counts <- as.integer(factor_counts)
    names(counts) <- keys
  } else if (is.logical(x)) {
    defined <- c("FALSE", "TRUE")
    logical_counts <- table(factor(as.character(observed), levels = defined))
    keys <- names(logical_counts)
    counts <- as.integer(logical_counts)
    names(counts) <- keys
  }

  label_attr <- attr(x, "labels", exact = TRUE)
  label_map <- character()
  if (!is.null(label_attr) && length(label_attr) > 0L) {
    label_names <- names(label_attr)
    if (is.null(label_names)) label_names <- rep(NA_character_, length(label_attr))
    label_values <- canonical_values(unname(label_attr))
    valid_labels <- !is.na(label_names) & nzchar(label_names)
    label_map <- stats::setNames(as.character(label_names[valid_labels]), label_values[valid_labels])
  }

  keys <- names(counts)
  count_values <- as.integer(counts)
  max_levels <- as.integer(config$profiling$max_levels_reported)
  omitted_count <- 0L
  omitted_levels <- 0L
  if (length(keys) > max_levels) {
    omitted_count <- sum(count_values[(max_levels + 1L):length(count_values)])
    omitted_levels <- length(keys) - max_levels
    keys <- keys[seq_len(max_levels)]
    count_values <- count_values[seq_len(max_levels)]
  }

  total_non_missing <- stats$n_non_missing
  total_all <- stats$n
  rows <- lapply(seq_along(keys), function(i) {
    key <- keys[[i]]
    label <- unname(label_map[key])
    if (length(label) == 0L || is.na(label)) label <- NA_character_
    data.frame(
      dataset_id = dataset_id,
      variable_position = variable_position,
      variable_name = variable_name,
      level_order = i,
      raw_value = key,
      value_label = label,
      display_value = if (!is.na(label)) paste0(key, " — ", label) else key,
      count = count_values[[i]],
      percent_of_nonmissing = if (total_non_missing == 0L) NA_real_ else 100 * count_values[[i]] / total_non_missing,
      percent_of_all = if (total_all == 0L) NA_real_ else 100 * count_values[[i]] / total_all,
      is_missing = FALSE,
      is_other_unreported = FALSE,
      stringsAsFactors = FALSE
    )
  })

  if (omitted_levels > 0L) {
    rows[[length(rows) + 1L]] <- data.frame(
      dataset_id = dataset_id,
      variable_position = variable_position,
      variable_name = variable_name,
      level_order = length(rows) + 1L,
      raw_value = "<OTHER_UNREPORTED>",
      value_label = sprintf("%d additional level(s)", omitted_levels),
      display_value = "<OTHER_UNREPORTED>",
      count = omitted_count,
      percent_of_nonmissing = if (total_non_missing == 0L) NA_real_ else 100 * omitted_count / total_non_missing,
      percent_of_all = if (total_all == 0L) NA_real_ else 100 * omitted_count / total_all,
      is_missing = FALSE,
      is_other_unreported = TRUE,
      stringsAsFactors = FALSE
    )
  }

  if (isTRUE(config$profiling$include_missing_level) && stats$n_effective_missing > 0L) {
    rows[[length(rows) + 1L]] <- data.frame(
      dataset_id = dataset_id,
      variable_position = variable_position,
      variable_name = variable_name,
      level_order = length(rows) + 1L,
      raw_value = "<MISSING>",
      value_label = NA_character_,
      display_value = "<MISSING>",
      count = stats$n_effective_missing,
      percent_of_nonmissing = NA_real_,
      percent_of_all = if (total_all == 0L) NA_real_ else 100 * stats$n_effective_missing / total_all,
      is_missing = TRUE,
      is_other_unreported = FALSE,
      stringsAsFactors = FALSE
    )
  }

  bind_rows_fill(rows)
}

profile_variable <- function(x, dataset_id, variable_position, source_variable_name, display_variable_name, config) {
  stats <- profile_basic_stats(x, config)
  role_hints <- detect_role_hints(x, source_variable_name, stats, config)
  classification <- classify_variable(x, source_variable_name, stats, role_hints, config)
  num <- numeric_summary(x, stats$effective_missing, config$profiling$include_quantiles)
  dates <- date_summary(x, stats$effective_missing)
  missing_pct <- if (stats$n == 0L) NA_real_ else 100 * stats$n_effective_missing / stats$n

  row <- data.frame(
    dataset_id = dataset_id,
    variable_position = variable_position,
    variable_name = display_variable_name,
    source_variable_name = source_variable_name,
    variable_label = as.character(attr(x, "label", exact = TRUE) %||% NA_character_),
    r_class = paste(class(x), collapse = ";"),
    storage_type = typeof(x),
    source_display_format = source_display_format(x),
    units = as.character(attr(x, "units", exact = TRUE) %||% NA_character_),
    semantic_type = classification$semantic_type,
    measurement_level = classification$measurement_level,
    categorical_eligible = classification$categorical_eligible,
    classification_reason = classification$classification_reason,
    cardinality_class = cardinality_class(stats, config),
    n_rows = stats$n,
    n_non_missing = stats$n_non_missing,
    n_system_missing = stats$n_missing,
    n_missing = stats$n_effective_missing,
    percent_missing = missing_pct,
    n_blank_strings = stats$n_blank,
    n_unique_non_missing = stats$n_unique,
    uniqueness_ratio = stats$unique_ratio,
    is_all_missing = stats$all_missing,
    is_constant = stats$constant,
    is_all_unique = !is.na(stats$unique_ratio) && stats$unique_ratio == 1,
    potential_identifier = role_hints$identifier_candidate,
    strong_identifier_evidence = role_hints$strong_identifier,
    potential_sensitive = role_hints$potential_sensitive,
    role_hint_reason = if (nzchar(role_hints$reason)) role_hints$reason else NA_character_,
    n_value_labels = stats$n_value_labels,
    n_defined_factor_levels = if (is.factor(x)) length(levels(x)) else NA_integer_,
    tagged_missing_summary = summarize_tagged_missing(x),
    user_missing_values = format_user_missing_values(x),
    user_missing_range = format_user_missing_range(x),
    numeric_min = num$min,
    numeric_q1 = num$q1,
    numeric_median = num$median,
    numeric_mean = num$mean,
    numeric_q3 = num$q3,
    numeric_max = num$max,
    numeric_sd = num$sd,
    n_infinite = num$infinite,
    date_min = dates$min,
    date_max = dates$max,
    timezone = dates$timezone,
    string_min_length = stats$min_length,
    string_mean_length = stats$mean_length,
    string_max_length = stats$max_length,
    n_leading_trailing_whitespace = stats$whitespace_count,
    examples = variable_examples(x, stats, role_hints, config),
    stringsAsFactors = FALSE
  )

  issues <- list()
  add_issue <- function(...) issues[[length(issues) + 1L]] <<- make_issue(dataset_id, variable_position, display_variable_name, ...)

  if (stats$all_missing) {
    add_issue("warning", "all_missing", metric = 100, threshold = NA_real_, message = "All values are missing or blank.")
  } else if (stats$constant) {
    add_issue("info", "constant", metric = 1, threshold = 1, message = "Only one distinct nonmissing value was observed.")
  }
  if (!is.na(missing_pct) && missing_pct / 100 >= as.numeric(config$profiling$high_missingness_threshold)) {
    add_issue(
      "warning", "high_missingness", metric = missing_pct,
      threshold = 100 * as.numeric(config$profiling$high_missingness_threshold),
      message = sprintf("Missingness is %.2f%%.", missing_pct)
    )
  }
  if (is.character(x) && !classification$categorical_eligible && stats$n_unique > as.numeric(config$profiling$max_categorical_levels)) {
    add_issue(
      "warning", "high_cardinality_character", metric = stats$n_unique,
      threshold = as.numeric(config$profiling$max_categorical_levels),
      message = classification$classification_reason
    )
  }
  if (role_hints$identifier_candidate) {
    add_issue(
      if (role_hints$strong_identifier) "warning" else "info",
      "potential_identifier", metric = 100 * stats$unique_ratio,
      threshold = 100 * as.numeric(config$profiling$id_uniqueness_threshold),
      message = paste("Potential identifier:", role_hints$reason %||% "high uniqueness.")
    )
  }
  if (role_hints$potential_sensitive) {
    add_issue("warning", "potential_sensitive_data", message = paste("Potentially sensitive field:", role_hints$reason %||% "name or value pattern."))
  }
  if (stats$n_blank > 0L) {
    add_issue("info", "blank_strings", metric = stats$n_blank, message = sprintf("%d blank string value(s) were treated as missing for profiling.", stats$n_blank))
  }
  if (isTRUE(config$profiling$check_whitespace) && stats$whitespace_count > 0L) {
    add_issue("info", "leading_trailing_whitespace", metric = stats$whitespace_count, message = sprintf("%d value(s) contain leading or trailing whitespace.", stats$whitespace_count))
  }
  if (isTRUE(config$profiling$check_infinite_values) && num$infinite > 0L) {
    add_issue("warning", "infinite_numeric_values", metric = num$infinite, message = sprintf("%d infinite numeric value(s) were observed.", num$infinite))
  }
  if (!nzchar(source_variable_name)) {
    add_issue("warning", "blank_variable_name", message = "The source column name was blank; a display name was generated in the dictionary.")
  }

  list(
    variable = row,
    categorical_levels = build_categorical_levels(x, dataset_id, variable_position, display_variable_name, stats, classification, config),
    value_labels = extract_value_labels(x, dataset_id, variable_position, display_variable_name),
    issues = if (length(issues) == 0L) empty_quality_issues() else bind_rows_fill(issues)
  )
}

profile_dataset <- function(record, config) {
  data <- record$data
  dataset_id <- record$dataset_id
  n_rows <- nrow(data)
  n_columns <- ncol(data)
  missing_cells <- sum(vapply(data, function(x) sum(safe_missing_mask(x)), numeric(1L)))
  blank_cells <- sum(vapply(data, function(x) {
    if (is.character(x) || is.factor(x)) sum(!safe_missing_mask(x) & !nzchar(trimws(as.character(x)))) else 0
  }, numeric(1L)))
  total_cells <- as.numeric(n_rows) * as.numeric(n_columns)

  duplicate_rows <- NA_integer_
  duplicate_check_performed <- FALSE
  if (isTRUE(config$profiling$check_duplicate_rows) && n_rows <= as.numeric(config$profiling$duplicate_check_max_rows)) {
    duplicate_rows <- tryCatch(sum(duplicated(data)), error = function(e) NA_integer_)
    duplicate_check_performed <- !is.na(duplicate_rows)
  }

  raw_import_problems <- record$import_problems %||% data.frame(stringsAsFactors = FALSE)
  if (is.data.frame(raw_import_problems) && nrow(raw_import_problems) > 0L) {
    import_problems <- data.frame(
      dataset_id = dataset_id,
      source_file = record$source_file,
      row = as.integer(raw_import_problems$row %||% NA_integer_),
      col = as.integer(raw_import_problems$col %||% NA_integer_),
      expected = as.character(raw_import_problems$expected %||% NA_character_),
      actual = as.character(raw_import_problems$actual %||% NA_character_),
      problem_file = as.character(raw_import_problems$file %||% record$source_file),
      stringsAsFactors = FALSE
    )
  } else {
    import_problems <- empty_import_problems()
  }

  file_meta <- record$file_metadata %||% source_file_metadata(record$source_file, config$input$compute_md5)
  dataset_meta <- data.frame(
    dataset_id = dataset_id,
    dataset_name = record$dataset_name,
    dataset_label = record$dataset_label,
    source_file = record$source_file,
    source_basename = basename(record$source_file),
    source_format = record$source_format,
    source_format_description = file_meta$source_format_description[[1L]],
    source_sheet = record$source_sheet,
    source_object = record$source_object,
    reader_package = record$reader_package,
    reader_function = record$reader_function,
    import_notes = record$import_notes,
    import_problem_count = nrow(import_problems),
    source_extension = file_meta$source_extension[[1L]],
    compression = file_meta$compression[[1L]],
    file_size_bytes = file_meta$file_size_bytes[[1L]],
    file_size_human = file_meta$file_size_human[[1L]],
    file_created_utc = file_meta$file_created_utc[[1L]],
    file_modified_utc = file_meta$file_modified_utc[[1L]],
    file_owner = file_meta$file_owner[[1L]],
    file_group = file_meta$file_group[[1L]],
    file_permissions = file_meta$file_permissions[[1L]],
    file_md5 = file_meta$file_md5[[1L]],
    n_rows = n_rows,
    n_columns = n_columns,
    total_cells = total_cells,
    missing_cells = missing_cells,
    percent_missing_cells = if (total_cells == 0) NA_real_ else 100 * missing_cells / total_cells,
    blank_string_cells = blank_cells,
    duplicate_rows = duplicate_rows,
    percent_duplicate_rows = if (n_rows == 0L || is.na(duplicate_rows)) NA_real_ else 100 * duplicate_rows / n_rows,
    duplicate_check_performed = duplicate_check_performed,
    memory_bytes = as.numeric(utils::object.size(data)),
    memory_human = format_bytes(as.numeric(utils::object.size(data))),
    stringsAsFactors = FALSE
  )

  variables <- list()
  levels <- list()
  labels <- list()
  issues <- list()
  source_names <- names(data)
  if (is.null(source_names)) source_names <- rep("", n_columns)
  duplicate_names <- duplicated(source_names) | duplicated(source_names, fromLast = TRUE)

  for (i in seq_len(n_columns)) {
    source_name <- as.character(source_names[[i]] %||% "")
    if (length(source_name) == 0L || is.na(source_name)) source_name <- ""
    display_name <- if (nzchar(source_name)) source_name else paste0("<unnamed_", i, ">")
    item <- profile_variable(data[[i]], dataset_id, i, source_name, display_name, config)
    variables[[length(variables) + 1L]] <- item$variable
    levels[[length(levels) + 1L]] <- item$categorical_levels
    labels[[length(labels) + 1L]] <- item$value_labels
    issues[[length(issues) + 1L]] <- item$issues

    if (duplicate_names[[i]]) {
      issues[[length(issues) + 1L]] <- make_issue(
        dataset_id, i, display_name, "warning", "duplicate_variable_name",
        message = sprintf("The source name '%s' appears more than once; variable_position must be used to disambiguate it.", source_name)
      )
    }
  }

  if (nrow(import_problems) > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      dataset_id, severity = "warning", issue_type = "import_parsing_problems",
      metric = nrow(import_problems), threshold = 0,
      message = sprintf("%d delimited-text parsing problem(s) were reported by the reader.", nrow(import_problems))
    )
  }

  if (!is.na(duplicate_rows) && duplicate_rows > 0L) {
    issues[[length(issues) + 1L]] <- make_issue(
      dataset_id, severity = "warning", issue_type = "duplicate_rows",
      metric = duplicate_rows, threshold = 0,
      message = sprintf("%d row(s) are exact duplicates of an earlier row.", duplicate_rows)
    )
  }
  if (isTRUE(config$profiling$check_duplicate_rows) && !duplicate_check_performed) {
    issues[[length(issues) + 1L]] <- make_issue(
      dataset_id, severity = "info", issue_type = "duplicate_check_skipped",
      metric = n_rows, threshold = as.numeric(config$profiling$duplicate_check_max_rows),
      message = "Exact duplicate-row checking was skipped because the dataset exceeded the configured row limit or contains unsupported nested values."
    )
  }

  list(
    dataset = dataset_meta,
    variables = bind_rows_fill(variables),
    categorical_levels = bind_rows_fill(levels),
    value_labels = bind_rows_fill(labels),
    import_problems = import_problems,
    issues = if (length(issues) == 0L) empty_quality_issues() else bind_rows_fill(issues)
  )
}
