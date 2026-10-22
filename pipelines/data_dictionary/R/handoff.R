DATAMINIMA_HANDOFF_SCHEMA_VERSION <- "1.0"
DATAMINIMA_PRODUCT_VERSION <- "0.2.0"
DATAMINIMA_PIPELINE_VERSION <- "0.2.0"
DATAMINIMA_PAYLOAD_ARTIFACTS <- c(
  "data_dictionary.json",
  "statistical_profile.json",
  "generation_profile.yml",
  "categorical_levels.csv",
  "quality_baseline.json"
)
DATAMINIMA_MANIFEST_COLUMNS <- c(
  "artifact", "relative_path", "artifact_group", "media_type", "schema_version",
  "dataflow_run_id", "created_at", "source_product_version",
  "source_pipeline_version", "configuration_sha256", "review_status",
  "size_bytes", "sha256"
)
DATAMINIMA_CATEGORY_COLUMNS <- c(
  "dataset_id", "variable_position", "variable_name", "level_order", "raw_value",
  "value_label", "display_value", "count", "percent_of_nonmissing",
  "percent_of_all", "is_missing", "is_other_unreported"
)
DATAMINIMA_REVIEW_STATUSES <- c("observed", "proposed", "approved", "rejected", "superseded")
DATAMINIMA_EVIDENCE_STATUSES <- c("observed", "proposed", "approved")
DATAMINIMA_ARTIFACT_SIZE_LIMITS <- c(
  "artifact_manifest.csv" = 1024^2,
  "data_dictionary.json" = 10 * 1024^2,
  "statistical_profile.json" = 25 * 1024^2,
  "generation_profile.yml" = 1024^2,
  "categorical_levels.csv" = 25 * 1024^2,
  "quality_baseline.json" = 10 * 1024^2
)

handoff_empty_object <- function() structure(list(), names = character())

handoff_declarations <- function(value) {
  if (is.null(value) || length(value) == 0L) return(list())
  if (!is.list(value)) stop("DataMinima profile declarations must be YAML sequences of mappings.", call. = FALSE)
  if (!is.null(names(value)) && any(nzchar(names(value)))) return(list(value))
  value
}

handoff_assert_known_names <- function(value, allowed, context) {
  if (is.null(value)) return(invisible(TRUE))
  unknown <- setdiff(names(value) %||% character(), allowed)
  if (length(unknown) > 0L) {
    stop(context, " contains unknown field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

handoff_assert_declarations <- function(value, allowed, required, context) {
  declarations <- handoff_declarations(value)
  for (i in seq_along(declarations)) {
    item <- declarations[[i]]
    if (!is.list(item) || is.null(names(item))) {
      stop(context, " item ", i, " must be a mapping.", call. = FALSE)
    }
    handoff_assert_known_names(item, allowed, paste0(context, " item ", i))
    missing <- required[vapply(required, function(name) is_blank_scalar(item[[name]]), logical(1L))]
    if (length(missing) > 0L) {
      stop(context, " item ", i, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}

validate_dataminima_handoff_config <- function(handoff) {
  handoff_assert_known_names(
    handoff,
    c("enabled", "schema_version", "run_id", "created_at", "review_status", "instruction_status", "profiles", "aggregate_privacy"),
    "dataminima_handoff"
  )
  if (!identical(as.character(handoff$schema_version), DATAMINIMA_HANDOFF_SCHEMA_VERSION)) {
    stop("dataminima_handoff.schema_version must be '1.0'.", call. = FALSE)
  }
  handoff_assert_known_names(
    handoff$review_status,
    c("data_dictionary", "statistical_profile", "generation_profile", "categorical_levels", "quality_baseline"),
    "dataminima_handoff.review_status"
  )
  statuses <- unlist(handoff$review_status, use.names = FALSE)
  if (length(statuses) != 5L || any(!statuses %in% DATAMINIMA_REVIEW_STATUSES)) {
    stop("Every DataMinima artifact review status must be observed, proposed, approved, rejected, or superseded.", call. = FALSE)
  }
  if (!(as.character(handoff$instruction_status) %in% DATAMINIMA_EVIDENCE_STATUSES)) {
    stop("dataminima_handoff.instruction_status must be observed, proposed, or approved.", call. = FALSE)
  }

  profiles <- handoff$profiles
  handoff_assert_known_names(
    profiles,
    c("numeric_fields", "quantiles", "histogram_bins", "conditional_missingness", "relationships", "dependencies", "temporal", "invalid_value_rules"),
    "dataminima_handoff.profiles"
  )
  probabilities <- as.numeric(unlist(profiles$quantiles, use.names = FALSE))
  if (length(probabilities) == 0L || any(!is.finite(probabilities)) ||
      any(probabilities < 0 | probabilities > 1) ||
      !identical(probabilities, sort(unique(probabilities)))) {
    stop("DataMinima quantiles must be unique finite probabilities in ascending order.", call. = FALSE)
  }
  bins <- suppressWarnings(as.integer(profiles$histogram_bins))
  if (is.na(bins) || bins < 1L || bins > 100L) {
    stop("dataminima_handoff.profiles.histogram_bins must be between 1 and 100.", call. = FALSE)
  }
  handoff_assert_declarations(
    profiles$conditional_missingness, c("table", "field", "conditional_on"),
    c("table", "field", "conditional_on"), "conditional_missingness"
  )
  handoff_assert_declarations(
    profiles$relationships,
    c("name", "parent_table", "parent_field", "child_table", "child_field", "status"),
    c("name", "parent_table", "parent_field", "child_table", "child_field", "status"),
    "relationships"
  )
  handoff_assert_declarations(
    profiles$dependencies, c("table", "fields", "method", "status"),
    c("table", "fields", "method", "status"), "dependencies"
  )
  handoff_assert_declarations(
    profiles$temporal,
    c("table", "field", "group_by_field", "ordering_fields", "seasonality_period", "interval_unit", "status"),
    c("table", "field", "interval_unit", "status"), "temporal"
  )
  handoff_assert_declarations(
    profiles$invalid_value_rules, c("table", "field", "minimum", "maximum", "allowed_values"),
    c("table", "field"), "invalid_value_rules"
  )

  privacy <- handoff$aggregate_privacy
  handoff_assert_known_names(
    privacy,
    c("policy_version", "minimum_cell_size", "precision_digits", "sensitive_fields", "prohibited_statistics", "review"),
    "dataminima_handoff.aggregate_privacy"
  )
  minimum_cell_size <- suppressWarnings(as.integer(privacy$minimum_cell_size))
  precision_digits <- suppressWarnings(as.integer(privacy$precision_digits))
  if (is.na(minimum_cell_size) || minimum_cell_size < 1L) {
    stop("aggregate_privacy.minimum_cell_size must be a positive integer.", call. = FALSE)
  }
  if (is.na(precision_digits) || precision_digits < 0L || precision_digits > 12L) {
    stop("aggregate_privacy.precision_digits must be between 0 and 12.", call. = FALSE)
  }
  prohibited <- as.character(unlist(privacy$prohibited_statistics %||% character(), use.names = FALSE))
  unknown_prohibited <- setdiff(prohibited, c("all", "distribution", "categorical_levels", "missingness", "relationships", "dependencies", "temporal"))
  if (length(unknown_prohibited) > 0L) {
    stop("Unknown prohibited statistic family: ", paste(unknown_prohibited, collapse = ", "), call. = FALSE)
  }
  handoff_assert_known_names(
    privacy$review, c("reviewer", "reviewed_at", "decision", "findings", "residual_limitations"),
    "dataminima_handoff.aggregate_privacy.review"
  )
  decision <- as.character(privacy$review$decision %||% "pending")
  if (!(decision %in% c("pending", "approved", "rejected"))) {
    stop("Aggregate disclosure review decision must be pending, approved, or rejected.", call. = FALSE)
  }

  approved_requested <- any(statuses == "approved") || identical(as.character(handoff$instruction_status), "approved")
  if (approved_requested) {
    if (!identical(decision, "approved") || is_blank_scalar(privacy$review$reviewer) ||
        !handoff_timezone_aware(privacy$review$reviewed_at)) {
      stop(
        "An approved DataMinima handoff requires an approved aggregate disclosure-risk review with reviewer and timezone-aware reviewed_at.",
        call. = FALSE
      )
    }
  }
  if (identical(as.character(handoff$review_status$generation_profile), "approved")) {
    nested_statuses <- c(
      as.character(handoff$instruction_status),
      vapply(handoff_declarations(profiles$relationships), function(item) as.character(item$status), character(1L)),
      vapply(handoff_declarations(profiles$dependencies), function(item) as.character(item$status), character(1L)),
      vapply(handoff_declarations(profiles$temporal), function(item) as.character(item$status), character(1L))
    )
    if (any(nested_statuses != "approved")) {
      stop("An approved generation profile cannot contain observed or proposed instructions.", call. = FALSE)
    }
  }
  invisible(handoff)
}

handoff_timezone_aware <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) return(FALSE)
  grepl("(Z|[+-][0-9]{2}:[0-9]{2})$", as.character(value), perl = TRUE)
}

handoff_safe_identifier <- function(value, context) {
  value <- as.character(value %||% "")
  if (length(value) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_.-]{0,127}$", value)) {
    stop(context, " must match the DataMinima safe identifier contract.", call. = FALSE)
  }
  value
}

handoff_text <- function(value) {
  value <- as.character(value %||% "")
  if (length(value) == 0L || is.na(value[[1L]])) "" else value[[1L]]
}

handoff_status <- function(value, evidence = FALSE) {
  value <- as.character(value %||% if (evidence) "observed" else "proposed")
  allowed <- if (evidence) DATAMINIMA_EVIDENCE_STATUSES else DATAMINIMA_REVIEW_STATUSES
  if (length(value) != 1L || !(value %in% allowed)) stop("Invalid DataMinima review status.", call. = FALSE)
  value
}

handoff_round <- function(value, digits) {
  value <- as.numeric(value)
  value[is.finite(value)] <- round(value[is.finite(value)], digits = digits)
  value
}

handoff_assert_finite <- function(value, context = "DataMinima handoff") {
  if (is.numeric(value) && any(!is.finite(value))) stop(context, " contains a non-finite number.", call. = FALSE)
  if (is.list(value)) {
    for (item in value) handoff_assert_finite(item, context)
  }
  invisible(TRUE)
}

handoff_sort_object <- function(value) {
  if (!is.list(value)) {
    if (!is.null(names(value))) return(value[order(names(value))])
    return(value)
  }
  if (!is.null(names(value)) && any(nzchar(names(value)))) {
    value <- value[order(names(value))]
  }
  lapply(value, handoff_sort_object)
}

handoff_json <- function(value, pretty = TRUE) {
  require_package("jsonlite", "DataMinima handoff JSON")
  handoff_assert_finite(value)
  paste0(
    jsonlite::toJSON(
      handoff_sort_object(value), auto_unbox = TRUE, null = "null", na = "null",
      digits = NA, pretty = pretty
    ),
    "\n"
  )
}

handoff_sha256_raw <- function(value) {
  require_package("openssl", "DataMinima SHA-256 checksums")
  paste0(as.character(openssl::sha256(value)), collapse = "")
}

handoff_sha256_file <- function(path) {
  handoff_sha256_raw(readBin(path, what = "raw", n = file.info(path)$size))
}

handoff_write_utf8 <- function(text, path) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), connection)
  path
}

handoff_canonical_configuration <- function(config) {
  canonical <- config
  canonical$dataminima_handoff$run_id <- NULL
  canonical$dataminima_handoff$created_at <- NULL
  handoff_json(canonical, pretty = FALSE)
}

handoff_source <- function(config, run_id, created_at, configuration_sha256) {
  list(
    product = "dataflow",
    product_version = DATAMINIMA_PRODUCT_VERSION,
    pipeline = "data_dictionary",
    pipeline_version = DATAMINIMA_PIPELINE_VERSION,
    configuration_sha256 = configuration_sha256,
    run_id = run_id,
    created_at = created_at
  )
}

handoff_review_evidence <- function(config, source) {
  review <- config$dataminima_handoff$aggregate_privacy$review
  list(
    review_schema_version = "1.0",
    source = source,
    policy_version = handoff_text(config$dataminima_handoff$aggregate_privacy$policy_version),
    reviewer = handoff_text(review$reviewer),
    reviewed_at = if (handoff_timezone_aware(review$reviewed_at)) as.character(review$reviewed_at) else NULL,
    decision = handoff_text(review$decision %||% "pending"),
    findings = as.character(unlist(review$findings %||% character(), use.names = FALSE)),
    residual_limitations = as.character(unlist(review$residual_limitations %||% character(), use.names = FALSE)),
    statement = "This review governs aggregate disclosure risk; it does not establish anonymization."
  )
}

handoff_record_map <- function(records) {
  out <- list()
  for (record in records) {
    dataset_id <- handoff_safe_identifier(record$dataset_id, "Dataset identifier")
    if (!is.null(out[[dataset_id]])) stop("DataMinima handoff dataset identifiers must be unique.", call. = FALSE)
    out[[dataset_id]] <- record$data
  }
  out
}

handoff_dictionary <- function(results, source, review_status) {
  dictionary <- results$data_dictionary
  if (!is.data.frame(dictionary) || nrow(dictionary) == 0L) {
    stop("DataMinima handoff requires at least one dictionary field.", call. = FALSE)
  }
  identities <- character()
  fields <- lapply(seq_len(nrow(dictionary)), function(i) {
    row <- dictionary[i, , drop = FALSE]
    dataset_id <- handoff_safe_identifier(row$dataset_id[[1L]], "Dictionary dataset_id")
    variable <- handoff_safe_identifier(row$variable[[1L]], "Dictionary variable")
    identity <- paste(dataset_id, variable, sep = "::")
    if (identity %in% identities) stop("DataMinima handoff dictionary field identities must be unique.", call. = FALSE)
    identities <<- c(identities, identity)
    rules <- handoff_text(row$inferred_validation_rules[[1L]])
    list(
      dataset_id = dataset_id,
      variable_position = as.integer(row$variable_position[[1L]]),
      variable = variable,
      label = handoff_text(row$label[[1L]]),
      type = handoff_text(row$type[[1L]]),
      storage_type = handoff_text(row$storage_type[[1L]]),
      semantic_type = handoff_text(row$semantic_type[[1L]]),
      business_definition = handoff_text(row$business_definition[[1L]]),
      technical_definition = handoff_text(row$technical_definition[[1L]]),
      definition_source = handoff_text(row$definition_source[[1L]]),
      documentation_status = handoff_text(row$documentation_status[[1L]]),
      nullable_observed = isTRUE(row$nullable_observed[[1L]]),
      value_domain_status = handoff_text(row$value_domain_status[[1L]]),
      source_display_format = handoff_text(row$source_display_format[[1L]]),
      units = handoff_text(row$units[[1L]]),
      potential_identifier = isTRUE(row$potential_identifier[[1L]]),
      potential_sensitive = isTRUE(row$potential_sensitive[[1L]]),
      inferred_validation_rules = if (nzchar(rules)) list(rules) else list(),
      review_required = isTRUE(row$review_required[[1L]]),
      review_reason = handoff_text(row$review_reason[[1L]])
    )
  })
  list(
    artifact_schema_version = DATAMINIMA_HANDOFF_SCHEMA_VERSION,
    source = source,
    review_status = handoff_status(review_status),
    artifact_type = "data_dictionary",
    fields = fields
  )
}

handoff_field_reference <- function(table, field) paste(as.character(table), as.character(field), sep = ".")

handoff_selected_field <- function(selectors, table, field) {
  selectors <- as.character(unlist(selectors %||% character(), use.names = FALSE))
  "*" %in% selectors || handoff_field_reference(table, field) %in% selectors
}

handoff_sensitive_fields <- function(results, config) {
  explicit <- as.character(unlist(config$dataminima_handoff$aggregate_privacy$sensitive_fields %||% character(), use.names = FALSE))
  metadata <- results$variable_metadata
  if (!is.data.frame(metadata) || nrow(metadata) == 0L) return(unique(explicit))
  flagged <- metadata$potential_sensitive %in% TRUE
  inferred <- handoff_field_reference(metadata$dataset_id[flagged], metadata$variable_name[flagged])
  unique(c(explicit, inferred))
}

handoff_prohibited <- function(config, family) {
  prohibited <- as.character(unlist(config$dataminima_handoff$aggregate_privacy$prohibited_statistics %||% character(), use.names = FALSE))
  family %in% prohibited || "all" %in% prohibited
}

handoff_issue <- function(dataset_id, variable_name = "", severity = "info", issue_type,
                          metric = NULL, threshold = NULL, message) {
  issue <- list(
    dataset_id = handoff_safe_identifier(dataset_id, "Quality issue dataset_id"),
    variable_name = handoff_text(variable_name),
    severity = as.character(severity),
    issue_type = handoff_safe_identifier(issue_type, "Quality issue type"),
    message = as.character(message)
  )
  if (!is.null(metric) && length(metric) == 1L && is.finite(as.numeric(metric))) issue$metric <- as.numeric(metric)
  if (!is.null(threshold) && length(threshold) == 1L && is.finite(as.numeric(threshold))) issue$threshold <- as.numeric(threshold)
  issue
}

handoff_apply_categorical_privacy <- function(results, config, sensitive_fields) {
  levels <- results$categorical_levels
  issues <- list()
  if (!is.data.frame(levels) || nrow(levels) == 0L) {
    return(list(rows = empty_categorical_levels(), issues = issues, suppressed_fields = character()))
  }
  minimum_cell_size <- as.integer(config$dataminima_handoff$aggregate_privacy$minimum_cell_size)
  digits <- as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits)
  kept <- list()
  suppressed_fields <- character()
  groups <- split(seq_len(nrow(levels)), handoff_field_reference(levels$dataset_id, levels$variable_name))
  for (reference in names(groups)) {
    rows <- levels[groups[[reference]], , drop = FALSE]
    total_all <- sum(rows$count)
    total_nonmissing <- sum(rows$count[!rows$is_missing])
    dataset_id <- as.character(rows$dataset_id[[1L]])
    variable_name <- as.character(rows$variable_name[[1L]])
    if (reference %in% sensitive_fields || handoff_prohibited(config, "categorical_levels")) {
      suppressed_fields <- c(suppressed_fields, reference)
      issues[[length(issues) + 1L]] <- handoff_issue(
        dataset_id, variable_name, "info", "statistic_excluded", message =
          "Categorical levels were excluded by the aggregate privacy policy."
      )
      next
    }

    ordinary <- !rows$is_missing & !rows$is_other_unreported
    rare <- ordinary & rows$count < minimum_cell_size
    rare_count <- sum(rows$count[rare])
    if (any(rare)) {
      issues[[length(issues) + 1L]] <- handoff_issue(
        dataset_id, variable_name, "info", "rare_levels_suppressed",
        metric = sum(rare), threshold = minimum_cell_size,
        message = "One or more categorical levels were suppressed under the minimum-cell policy."
      )
      rows <- rows[!rare, , drop = FALSE]
      if (rare_count >= minimum_cell_size) {
        template <- levels[groups[[reference]][[1L]], , drop = FALSE]
        other <- data.frame(
          dataset_id = dataset_id,
          variable_position = as.integer(template$variable_position[[1L]]),
          variable_name = variable_name,
          level_order = 0L,
          raw_value = "<OTHER_UNREPORTED>",
          value_label = "Suppressed aggregate",
          display_value = "<OTHER_UNREPORTED>",
          count = as.integer(rare_count),
          percent_of_nonmissing = if (total_nonmissing == 0L) 0 else 100 * rare_count / total_nonmissing,
          percent_of_all = if (total_all == 0L) 0 else 100 * rare_count / total_all,
          is_missing = FALSE,
          is_other_unreported = TRUE,
          stringsAsFactors = FALSE
        )
        existing_other <- rows$is_other_unreported
        if (any(existing_other)) {
          rows$count[existing_other][[1L]] <- rows$count[existing_other][[1L]] + rare_count
        } else {
          rows <- rbind(rows, other)
        }
      } else {
        suppressed_fields <- c(suppressed_fields, reference)
      }
    }
    if (nrow(rows) > 0L) {
      rows <- rows[order(rows$is_missing, rows$is_other_unreported, rows$level_order), , drop = FALSE]
      rows$level_order <- seq_len(nrow(rows))
      rows$value_label[is.na(rows$value_label)] <- ""
      rows$raw_value[is.na(rows$raw_value)] <- ""
      rows$display_value[is.na(rows$display_value)] <- ""
      rows$percent_of_nonmissing[is.na(rows$percent_of_nonmissing)] <- 0
      rows$percent_of_all[is.na(rows$percent_of_all)] <- 0
      rows$percent_of_nonmissing[!rows$is_missing] <- if (total_nonmissing == 0L) 0 else 100 * rows$count[!rows$is_missing] / total_nonmissing
      rows$percent_of_all <- if (total_all == 0L) 0 else 100 * rows$count / total_all
      rows$percent_of_nonmissing <- handoff_round(rows$percent_of_nonmissing, digits)
      rows$percent_of_all <- handoff_round(rows$percent_of_all, digits)
      kept[[length(kept) + 1L]] <- rows[DATAMINIMA_CATEGORY_COLUMNS]
    }
  }
  list(
    rows = if (length(kept) == 0L) empty_categorical_levels() else bind_rows_fill(kept)[DATAMINIMA_CATEGORY_COLUMNS],
    issues = issues,
    suppressed_fields = unique(suppressed_fields)
  )
}

handoff_numeric_distribution <- function(values, config, status, family = "empirical_numeric_v1") {
  digits <- as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits)
  minimum_cell_size <- as.integer(config$dataminima_handoff$aggregate_privacy$minimum_cell_size)
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(list(value = NULL, suppressed_bins = 0L))
  parameters <- list(
    count = as.integer(length(values)),
    mean = handoff_round(mean(values), digits),
    minimum = handoff_round(min(values), digits),
    maximum = handoff_round(max(values), digits)
  )
  if (length(values) > 1L) parameters$sd <- handoff_round(stats::sd(values), digits)
  probabilities <- as.numeric(unlist(config$dataminima_handoff$profiles$quantiles, use.names = FALSE))
  quantiles <- stats::quantile(values, probs = probabilities, names = FALSE, type = 7)
  quantile_rows <- lapply(seq_along(probabilities), function(i) list(
    probability = probabilities[[i]], value = handoff_round(quantiles[[i]], digits)
  ))
  histogram <- list()
  suppressed_bins <- 0L
  if (length(unique(values)) > 1L) {
    breaks <- seq(min(values), max(values), length.out = as.integer(config$dataminima_handoff$profiles$histogram_bins) + 1L)
    bins <- findInterval(values, breaks, all.inside = TRUE)
    counts <- tabulate(bins, nbins = length(breaks) - 1L)
    for (i in seq_along(counts)) {
      if (counts[[i]] < minimum_cell_size) {
        suppressed_bins <- suppressed_bins + 1L
      } else {
        lower <- handoff_round(breaks[[i]], digits)
        upper <- handoff_round(breaks[[i + 1L]], digits)
        if (upper <= lower) {
          suppressed_bins <- suppressed_bins + 1L
        } else {
          histogram[[length(histogram) + 1L]] <- list(
            lower = lower, upper = upper, count = as.integer(counts[[i]])
          )
        }
      }
    }
  }
  standard_deviation <- if (length(values) > 1L) stats::sd(values) else 0
  skewness <- if (length(values) >= 3L && is.finite(standard_deviation) && standard_deviation > 0) {
    mean(((values - mean(values)) / standard_deviation)^3)
  } else NULL
  list(
    value = list(
      family = family,
      parameters = parameters,
      quantiles = quantile_rows,
      histogram = histogram,
      skewness = if (is.null(skewness)) NULL else handoff_round(skewness, digits),
      zero_fraction = handoff_round(mean(values == 0), digits),
      status = handoff_status(status, evidence = TRUE)
    ),
    suppressed_bins = suppressed_bins
  )
}

handoff_get_vector <- function(records, table, field) {
  data <- records[[table]]
  if (is.null(data)) stop("DataMinima profile references an unknown table: ", table, call. = FALSE)
  matches <- which(names(data) == field)
  if (length(matches) != 1L) {
    stop("DataMinima profile field must resolve to exactly one source column: ", table, ".", field, call. = FALSE)
  }
  data[[matches[[1L]]]]
}

handoff_conditional_declaration <- function(config, table, field) {
  declarations <- handoff_declarations(config$dataminima_handoff$profiles$conditional_missingness)
  matches <- Filter(function(item) identical(as.character(item$table), table) && identical(as.character(item$field), field), declarations)
  if (length(matches) > 1L) stop("Conditional missingness declarations must be unique per field.", call. = FALSE)
  if (length(matches) == 0L) NULL else matches[[1L]]
}

handoff_conditional_missingness <- function(target, condition, condition_name, config) {
  minimum_cell_size <- as.integer(config$dataminima_handoff$aggregate_privacy$minimum_cell_size)
  digits <- as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits)
  target_missing <- safe_missing_mask(target)
  condition_missing <- safe_missing_mask(condition)
  keys <- canonical_values(condition)
  available <- !condition_missing
  groups <- split(seq_along(keys)[available], keys[available])
  rates <- list()
  suppressed <- 0L
  for (key in sort(names(groups))) {
    index <- groups[[key]]
    missing_count <- sum(target_missing[index])
    if (length(index) < minimum_cell_size || (missing_count > 0L && missing_count < minimum_cell_size)) {
      suppressed <- suppressed + 1L
      next
    }
    rates[[key]] <- handoff_round(missing_count / length(index), digits)
  }
  list(
    conditional_on = if (length(rates) > 0L) list(condition_name) else list(),
    conditional_rates = rates,
    default_rate = if (length(rates) > 0L && length(target) > 0L) handoff_round(mean(target_missing), digits) else NULL,
    suppressed_groups = suppressed
  )
}

handoff_temporal_iso <- function(value) {
  if (inherits(value, "Date")) return(format(as.POSIXct(value, tz = "UTC"), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"))
  format(as.POSIXct(value, tz = "UTC"), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
}

handoff_temporal_intervals <- function(time, group = NULL, unit = "days") {
  present <- !safe_missing_mask(time)
  time <- as.POSIXct(time[present], tz = "UTC")
  if (is.null(group)) {
    groups <- list(all = time)
  } else {
    group <- canonical_values(group[present])
    groups <- split(time, group)
  }
  values <- unlist(lapply(groups, function(item) {
    if (length(item) < 2L) return(numeric())
    as.numeric(diff(sort(item)), units = unit)
  }), use.names = FALSE)
  values[is.finite(values) & values >= 0]
}

handoff_cramers_v <- function(x, y) {
  keep <- !safe_missing_mask(x) & !safe_missing_mask(y)
  if (sum(keep) == 0L) return(NULL)
  table_xy <- table(canonical_values(x[keep]), canonical_values(y[keep]))
  if (nrow(table_xy) < 2L || ncol(table_xy) < 2L) return(NULL)
  statistic <- suppressWarnings(stats::chisq.test(table_xy, correct = FALSE)$statistic[[1L]])
  denominator <- sum(table_xy) * min(nrow(table_xy) - 1L, ncol(table_xy) - 1L)
  if (!is.finite(statistic) || denominator <= 0) NULL else sqrt(statistic / denominator)
}

handoff_conditional_probabilities <- function(condition, outcome, config) {
  minimum_cell_size <- as.integer(config$dataminima_handoff$aggregate_privacy$minimum_cell_size)
  digits <- as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits)
  keep <- !safe_missing_mask(condition) & !safe_missing_mask(outcome)
  condition <- canonical_values(condition[keep])
  outcome <- canonical_values(outcome[keep])
  groups <- split(seq_along(condition), condition)
  probabilities <- list()
  suppressed <- 0L
  for (key in sort(names(groups))) {
    index <- groups[[key]]
    if (length(index) < minimum_cell_size) {
      suppressed <- suppressed + 1L
      next
    }
    counts <- sort(table(outcome[index]), decreasing = TRUE)
    rare <- counts < minimum_cell_size
    rare_count <- sum(counts[rare])
    counts <- counts[!rare]
    if (rare_count >= minimum_cell_size) counts[["<OTHER_UNREPORTED>"]] <- rare_count
    if (rare_count > 0L && rare_count < minimum_cell_size) {
      suppressed <- suppressed + 1L
      next
    }
    if (length(counts) == 0L) next
    probabilities[[key]] <- handoff_probability_map(counts, digits)
  }
  overall <- sort(table(outcome), decreasing = TRUE)
  rare <- overall < minimum_cell_size
  rare_count <- sum(overall[rare])
  overall <- overall[!rare]
  if (rare_count >= minimum_cell_size) overall[["<OTHER_UNREPORTED>"]] <- rare_count
  default <- handoff_empty_object()
  if (!(rare_count > 0L && rare_count < minimum_cell_size) && length(overall) > 0L) {
    default <- handoff_probability_map(overall, digits)
  }
  list(probabilities = probabilities, default = default, suppressed_groups = suppressed)
}

handoff_probability_map <- function(counts, digits) {
  level_names <- names(counts)
  counts <- as.numeric(counts)
  if (length(counts) == 0L || sum(counts) <= 0) return(handoff_empty_object())
  scale <- 10^as.integer(digits)
  exact_units <- counts / sum(counts) * scale
  units <- floor(exact_units)
  remainder <- as.integer(round(scale - sum(units)))
  if (remainder > 0L) {
    order_index <- order(exact_units - units, decreasing = TRUE)
    for (i in seq_len(remainder)) {
      index <- order_index[[(i - 1L) %% length(order_index) + 1L]]
      units[[index]] <- units[[index]] + 1
    }
  }
  as.list(stats::setNames(units / scale, level_names))
}

handoff_relationship_profiles <- function(records, config, sensitive_fields, add_issue) {
  declarations <- handoff_declarations(config$dataminima_handoff$profiles$relationships)
  profiles <- list()
  if (length(declarations) == 0L) return(profiles)
  for (item in declarations) {
    name <- handoff_safe_identifier(item$name, "Relationship name")
    parent_table <- handoff_safe_identifier(item$parent_table, "Relationship parent table")
    parent_field <- handoff_safe_identifier(item$parent_field, "Relationship parent field")
    child_table <- handoff_safe_identifier(item$child_table, "Relationship child table")
    child_field <- handoff_safe_identifier(item$child_field, "Relationship child field")
    status <- handoff_status(item$status, evidence = TRUE)
    refs <- c(handoff_field_reference(parent_table, parent_field), handoff_field_reference(child_table, child_field))
    if (any(refs %in% sensitive_fields) || handoff_prohibited(config, "relationships")) {
      add_issue(child_table, child_field, "info", "statistic_excluded", message = "Relationship evidence was excluded by the aggregate privacy policy.")
      next
    }
    parent <- handoff_get_vector(records, parent_table, parent_field)
    child <- handoff_get_vector(records, child_table, child_field)
    parent_keys <- canonical_values(parent[!safe_missing_mask(parent)])
    child_keys <- canonical_values(child[!safe_missing_mask(child)])
    parent_unique_rate <- if (length(parent_keys) == 0L) 0 else length(unique(parent_keys)) / length(parent_keys)
    foreign_key_coverage <- if (length(child_keys) == 0L) 0 else mean(child_keys %in% unique(parent_keys))
    child_counts <- as.numeric(table(factor(child_keys, levels = unique(parent_keys))))
    distribution <- handoff_numeric_distribution(child_counts, config, status, family = "empirical_child_count_v1")
    if (is.null(distribution$value)) {
      distribution$value <- list(family = "empirical_child_count_v1", parameters = list(count = 0L), quantiles = list(), histogram = list(), skewness = NULL, zero_fraction = NULL, status = status)
    }
    profiles[[length(profiles) + 1L]] <- list(
      table = child_table,
      value = list(
        name = name,
        parent_table = parent_table,
        parent_field = parent_field,
        child_table = child_table,
        child_field = child_field,
        parent_unique_rate = handoff_round(parent_unique_rate, as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits)),
        foreign_key_coverage = handoff_round(foreign_key_coverage, as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits)),
        child_count_distribution = distribution$value
      )
    )
  }
  profiles
}

handoff_dependency_profiles <- function(records, config, sensitive_fields, add_issue) {
  declarations <- handoff_declarations(config$dataminima_handoff$profiles$dependencies)
  statistical <- list()
  generation <- list()
  minimum_cell_size <- as.integer(config$dataminima_handoff$aggregate_privacy$minimum_cell_size)
  digits <- as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits)
  for (item in declarations) {
    table <- handoff_safe_identifier(item$table, "Dependency table")
    fields <- as.character(unlist(item$fields, use.names = FALSE))
    if (length(fields) != 2L || length(unique(fields)) != 2L) stop("Dependency declarations require two distinct fields.", call. = FALSE)
    fields <- vapply(fields, handoff_safe_identifier, character(1L), context = "Dependency field")
    method <- as.character(item$method)
    if (!(method %in% c("pearson", "spearman", "cramers_v", "conditional_frequency", "rank_correlation"))) {
      stop("Unsupported DataMinima dependency method: ", method, call. = FALSE)
    }
    status <- handoff_status(item$status, evidence = TRUE)
    refs <- handoff_field_reference(table, fields)
    if (any(refs %in% sensitive_fields) || handoff_prohibited(config, "dependencies")) {
      add_issue(table, fields[[2L]], "info", "statistic_excluded", message = "Dependency evidence was excluded by the aggregate privacy policy.")
      next
    }
    x <- handoff_get_vector(records, table, fields[[1L]])
    y <- handoff_get_vector(records, table, fields[[2L]])
    keep <- !safe_missing_mask(x) & !safe_missing_mask(y)
    if (method %in% c("pearson", "spearman", "rank_correlation")) {
      x_numeric <- suppressWarnings(as.numeric(x[keep]))
      y_numeric <- suppressWarnings(as.numeric(y[keep]))
      finite <- is.finite(x_numeric) & is.finite(y_numeric)
      x_numeric <- x_numeric[finite]
      y_numeric <- y_numeric[finite]
      if (length(x_numeric) < minimum_cell_size || length(unique(x_numeric)) < 2L || length(unique(y_numeric)) < 2L) {
        add_issue(table, fields[[2L]], "info", "statistic_excluded", message = "Dependency evidence was excluded because the configured fields lacked sufficient non-disclosive variation.")
        next
      }
      correlation_method <- if (method == "pearson") "pearson" else "spearman"
      value <- stats::cor(x_numeric, y_numeric, method = correlation_method)
      if (!is.finite(value)) next
      statistical[[length(statistical) + 1L]] <- list(
        table = table,
        value = list(
          fields = unname(as.list(fields)), method = method,
          value = handoff_round(value, digits),
          parameters = list(n_complete = as.integer(length(x_numeric))), status = status
        )
      )
      fit <- stats::lm(y_numeric ~ x_numeric)
      coefficients <- stats::coef(fit)
      residual_sd <- if (length(x_numeric) > 2L) suppressWarnings(summary(fit)$sigma) else 0
      generation[[length(generation) + 1L]] <- list(
        table = table,
        value = list(
          strategy = "numeric_linear", fields = unname(as.list(fields)),
          intercept = handoff_round(coefficients[[1L]], digits),
          slope = handoff_round(coefficients[[2L]], digits),
          noise_sd = handoff_round(if (is.finite(residual_sd)) residual_sd else 0, digits),
          status = status
        )
      )
    } else if (method == "cramers_v") {
      cells <- table(canonical_values(x[keep]), canonical_values(y[keep]))
      if (length(cells) == 0L || any(cells > 0L & cells < minimum_cell_size)) {
        add_issue(table, fields[[2L]], "info", "statistic_excluded", message = "Categorical association was excluded because one or more cells were below the minimum-cell threshold.")
        next
      }
      value <- handoff_cramers_v(x, y)
      if (is.null(value)) next
      statistical[[length(statistical) + 1L]] <- list(
        table = table,
        value = list(
          fields = unname(as.list(fields)), method = method, value = handoff_round(value, digits),
          parameters = list(n_complete = as.integer(sum(keep))), status = status
        )
      )
    } else {
      probabilities <- handoff_conditional_probabilities(x, y, config)
      if (length(probabilities$probabilities) == 0L) {
        add_issue(table, fields[[2L]], "info", "statistic_excluded", message = "Conditional frequency evidence was excluded by the minimum-cell policy.")
        next
      }
      statistical[[length(statistical) + 1L]] <- list(
        table = table,
        value = list(
          fields = unname(as.list(fields)), method = "conditional_frequency", value = NULL,
          parameters = list(
            n_complete = as.integer(sum(keep)),
            condition_groups_disclosed = as.integer(length(probabilities$probabilities)),
            condition_groups_suppressed = as.integer(probabilities$suppressed_groups)
          ),
          status = status
        )
      )
      generation[[length(generation) + 1L]] <- list(
        table = table,
        value = list(
          strategy = "categorical_conditional", fields = unname(as.list(fields)),
          probabilities = probabilities$probabilities,
          default_probabilities = probabilities$default,
          status = status
        )
      )
    }
  }
  list(statistical = statistical, generation = generation)
}

handoff_temporal_profiles <- function(records, config, sensitive_fields, add_issue) {
  declarations <- handoff_declarations(config$dataminima_handoff$profiles$temporal)
  statistical <- list()
  generation <- list()
  for (item in declarations) {
    table <- handoff_safe_identifier(item$table, "Temporal table")
    field <- handoff_safe_identifier(item$field, "Temporal field")
    status <- handoff_status(item$status, evidence = TRUE)
    unit <- as.character(item$interval_unit)
    if (!(unit %in% c("seconds", "minutes", "hours", "days"))) stop("Temporal interval_unit is invalid.", call. = FALSE)
    group_field <- handoff_text(item$group_by_field)
    ordering_fields <- as.character(unlist(item$ordering_fields %||% field, use.names = FALSE))
    ordering_fields <- vapply(ordering_fields, handoff_safe_identifier, character(1L), context = "Temporal ordering field")
    refs <- c(handoff_field_reference(table, field), if (nzchar(group_field)) handoff_field_reference(table, group_field))
    if (any(refs %in% sensitive_fields) || handoff_prohibited(config, "temporal")) {
      add_issue(table, field, "info", "statistic_excluded", message = "Temporal evidence was excluded by the aggregate privacy policy.")
      next
    }
    time <- handoff_get_vector(records, table, field)
    if (!inherits(time, c("Date", "POSIXct", "POSIXlt"))) stop("Configured temporal field is not a date or datetime: ", table, ".", field, call. = FALSE)
    present <- !safe_missing_mask(time)
    if (!any(present)) {
      add_issue(table, field, "info", "statistic_excluded", message = "Temporal evidence was excluded because no timestamps were observed.")
      next
    }
    group <- if (nzchar(group_field)) handoff_get_vector(records, table, handoff_safe_identifier(group_field, "Temporal group field")) else NULL
    intervals <- handoff_temporal_intervals(time, group, unit)
    distribution <- if (length(intervals) > 0L) {
      handoff_numeric_distribution(intervals, config, status, family = paste0("empirical_intervals_", unit, "_v1"))$value
    } else NULL
    minimum <- handoff_temporal_iso(min(time[present]))
    maximum <- handoff_temporal_iso(max(time[present]))
    statistical[[length(statistical) + 1L]] <- list(
      table = table,
      value = list(
        field = field, minimum = minimum, maximum = maximum,
        interval_distribution = distribution,
        seasonality_period = handoff_text(item$seasonality_period),
        ordering_fields = unname(as.list(ordering_fields)), status = status
      )
    )
    if (is.null(distribution)) {
      generation_value <- list(strategy = "uniform_range", field = field, minimum = minimum, maximum = maximum, status = status)
    } else {
      generation_value <- list(
        strategy = "monotonic_intervals", field = field, minimum = minimum, maximum = maximum,
        interval_distribution = list(family = distribution$family, parameters = distribution$parameters, status = status),
        interval_unit = unit, group_by_field = if (nzchar(group_field)) group_field else NULL, status = status
      )
    }
    generation[[length(generation) + 1L]] <- list(table = table, value = generation_value)
  }
  list(statistical = statistical, generation = generation)
}

handoff_validate_field_references <- function(results, config, records) {
  dictionary <- results$data_dictionary
  known <- handoff_field_reference(dictionary$dataset_id, dictionary$variable)
  selectors <- as.character(unlist(config$dataminima_handoff$profiles$numeric_fields %||% character(), use.names = FALSE))
  unknown_selectors <- setdiff(selectors, c("*", known))
  if (length(unknown_selectors) > 0L) stop("Unknown DataMinima numeric field selector(s): ", paste(unknown_selectors, collapse = ", "), call. = FALSE)
  explicit_sensitive <- as.character(unlist(config$dataminima_handoff$aggregate_privacy$sensitive_fields %||% character(), use.names = FALSE))
  unknown_sensitive <- setdiff(explicit_sensitive, known)
  if (length(unknown_sensitive) > 0L) stop("Unknown DataMinima sensitive field(s): ", paste(unknown_sensitive, collapse = ", "), call. = FALSE)

  references <- list()
  add_reference <- function(table, field) references[[length(references) + 1L]] <<- handoff_field_reference(table, field)
  for (item in handoff_declarations(config$dataminima_handoff$profiles$conditional_missingness)) {
    add_reference(item$table, item$field); add_reference(item$table, item$conditional_on)
  }
  for (item in handoff_declarations(config$dataminima_handoff$profiles$relationships)) {
    add_reference(item$parent_table, item$parent_field); add_reference(item$child_table, item$child_field)
  }
  for (item in handoff_declarations(config$dataminima_handoff$profiles$dependencies)) {
    for (field in unlist(item$fields, use.names = FALSE)) add_reference(item$table, field)
  }
  for (item in handoff_declarations(config$dataminima_handoff$profiles$temporal)) {
    add_reference(item$table, item$field)
    if (!is_blank_scalar(item$group_by_field)) add_reference(item$table, item$group_by_field)
    for (field in unlist(item$ordering_fields %||% character(), use.names = FALSE)) add_reference(item$table, field)
  }
  for (item in handoff_declarations(config$dataminima_handoff$profiles$invalid_value_rules)) add_reference(item$table, item$field)
  unknown <- setdiff(unique(unlist(references, use.names = FALSE)), known)
  if (length(unknown) > 0L) stop("DataMinima profile references unknown field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  for (table in names(records)) handoff_safe_identifier(table, "DataMinima table identifier")
  invisible(TRUE)
}

handoff_build_profiles <- function(results, record_map, config, source, categorical) {
  issues <- categorical$issues
  add_issue <- function(dataset_id, variable_name = "", severity = "info", issue_type, metric = NULL, threshold = NULL, message) {
    issues[[length(issues) + 1L]] <<- handoff_issue(dataset_id, variable_name, severity, issue_type, metric, threshold, message)
  }
  sensitive_fields <- handoff_sensitive_fields(results, config)
  instruction_status <- handoff_status(config$dataminima_handoff$instruction_status, evidence = TRUE)
  statistical_tables <- list()
  generation_tables <- list()

  metadata <- results$variable_metadata
  for (table in unique(as.character(metadata$dataset_id))) {
    handoff_safe_identifier(table, "Statistical profile table")
    rows <- metadata[metadata$dataset_id == table, , drop = FALSE]
    row_count <- as.integer(results$dataset_metadata$n_rows[match(table, results$dataset_metadata$dataset_id)])
    statistical_fields <- list()
    generation_fields <- list()
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      field <- handoff_safe_identifier(row$variable_name[[1L]], "Statistical profile field")
      reference <- handoff_field_reference(table, field)
      values <- handoff_get_vector(record_map, table, field)
      n_rows <- length(values)
      n_missing <- as.integer(row$n_missing[[1L]])
      n_non_missing <- as.integer(row$n_non_missing[[1L]])
      distribution <- NULL
      distribution_result <- NULL
      selected_numeric <- handoff_selected_field(config$dataminima_handoff$profiles$numeric_fields, table, field)
      privacy_excluded <- reference %in% sensitive_fields || isTRUE(row$potential_identifier[[1L]]) || handoff_prohibited(config, "distribution")
      if (selected_numeric) {
        if (privacy_excluded) {
          add_issue(table, field, "info", "statistic_excluded", message = "Numeric distribution evidence was excluded by the aggregate privacy policy.")
        } else if (!(is.numeric(values) || is.integer(values)) || inherits(values, c("Date", "POSIXt", "difftime"))) {
          stop("Configured numeric profile field is not numeric: ", reference, call. = FALSE)
        } else {
          distribution_result <- handoff_numeric_distribution(values[!safe_missing_mask(values)], config, "observed")
          distribution <- distribution_result$value
          if (!is.null(distribution_result) && distribution_result$suppressed_bins > 0L) {
            add_issue(
              table, field, "info", "histogram_bins_suppressed",
              metric = distribution_result$suppressed_bins,
              threshold = config$dataminima_handoff$aggregate_privacy$minimum_cell_size,
              message = "One or more histogram bins were suppressed under the minimum-cell policy."
            )
          }
        }
      }

      conditional <- NULL
      declaration <- handoff_conditional_declaration(config, table, field)
      if (!is.null(declaration)) {
        condition_name <- handoff_safe_identifier(declaration$conditional_on, "Conditional missingness field")
        condition_reference <- handoff_field_reference(table, condition_name)
        if (reference %in% sensitive_fields || condition_reference %in% sensitive_fields || handoff_prohibited(config, "missingness")) {
          add_issue(table, field, "info", "statistic_excluded", message = "Conditional missingness was excluded by the aggregate privacy policy.")
        } else {
          conditional <- handoff_conditional_missingness(values, handoff_get_vector(record_map, table, condition_name), condition_name, config)
          if (conditional$suppressed_groups > 0L) {
            add_issue(table, field, "info", "conditional_groups_suppressed", metric = conditional$suppressed_groups,
                      threshold = config$dataminima_handoff$aggregate_privacy$minimum_cell_size,
                      message = "One or more conditional missingness groups were suppressed under the minimum-cell policy.")
          }
        }
      }
      conditional_on <- if (!is.null(conditional)) conditional$conditional_on else list()
      mechanism_note <- if (length(conditional_on) > 0L) {
        "Observed conditional missingness rates are provided as descriptive evidence; MCAR, MAR, or MNAR has not been proven."
      } else {
        "Observed overall missingness is descriptive evidence; MCAR, MAR, or MNAR has not been proven."
      }
      statistical_fields[[field]] <- list(
        field_name = field,
        n_rows = as.integer(n_rows),
        n_non_missing = n_non_missing,
        n_missing = n_missing,
        n_unique_non_missing = as.integer(row$n_unique_non_missing[[1L]]),
        distribution = distribution,
        missingness = list(
          rate = if (n_rows == 0L) 0 else n_missing / n_rows,
          conditional_on = conditional_on,
          mechanism_note = mechanism_note,
          status = "observed"
        )
      )

      instruction <- list()
      if (!privacy_excluded && !handoff_prohibited(config, "missingness")) {
        instruction$missingness <- list(rate = if (n_rows == 0L) 0 else n_missing / n_rows, status = instruction_status)
        if (!is.null(conditional) && length(conditional$conditional_rates) > 0L) {
          instruction$missingness$conditional_on <- conditional$conditional_on
          instruction$missingness$conditional_rates <- conditional$conditional_rates
          instruction$missingness$default_rate <- conditional$default_rate
        }
      }
      if (!is.null(distribution)) {
        instruction$distribution <- list(
          family = distribution$family,
          parameters = distribution$parameters,
          status = instruction_status
        )
      }
      category_rows <- categorical$rows[
        categorical$rows$dataset_id == table & categorical$rows$variable_name == field & !categorical$rows$is_missing,
        , drop = FALSE
      ]
      if (nrow(category_rows) > 0L && !(reference %in% categorical$suppressed_fields) && !privacy_excluded) {
        counts <- stats::setNames(category_rows$count, category_rows$display_value)
        instruction$category_probabilities <- handoff_probability_map(counts, as.integer(config$dataminima_handoff$aggregate_privacy$precision_digits))
      }
      if (length(instruction) > 0L) {
        instruction$status <- instruction_status
        generation_fields[[field]] <- instruction
      }
    }
    statistical_tables[[table]] <- list(
      row_count = row_count, fields = statistical_fields,
      relationships = list(), dependencies = list(), temporal = list()
    )
    generation_tables[[table]] <- list(
      fields = generation_fields, relationships = list(), dependencies = list(), temporal = list()
    )
  }

  relationships <- handoff_relationship_profiles(record_map, config, sensitive_fields, add_issue)
  for (item in relationships) {
    statistical_tables[[item$table]]$relationships[[length(statistical_tables[[item$table]]$relationships) + 1L]] <- item$value
    generation_tables[[item$table]]$relationships[[length(generation_tables[[item$table]]$relationships) + 1L]] <- item$value
  }
  dependencies <- handoff_dependency_profiles(record_map, config, sensitive_fields, add_issue)
  for (item in dependencies$statistical) statistical_tables[[item$table]]$dependencies[[length(statistical_tables[[item$table]]$dependencies) + 1L]] <- item$value
  for (item in dependencies$generation) generation_tables[[item$table]]$dependencies[[length(generation_tables[[item$table]]$dependencies) + 1L]] <- item$value
  temporal <- handoff_temporal_profiles(record_map, config, sensitive_fields, add_issue)
  for (item in temporal$statistical) statistical_tables[[item$table]]$temporal[[length(statistical_tables[[item$table]]$temporal) + 1L]] <- item$value
  for (item in temporal$generation) generation_tables[[item$table]]$temporal[[length(generation_tables[[item$table]]$temporal) + 1L]] <- item$value

  list(
    statistical = list(
      artifact_schema_version = DATAMINIMA_HANDOFF_SCHEMA_VERSION,
      source = source,
      review_status = handoff_status(config$dataminima_handoff$review_status$statistical_profile),
      artifact_type = "statistical_profile",
      tables = statistical_tables
    ),
    generation = list(
      artifact_schema_version = DATAMINIMA_HANDOFF_SCHEMA_VERSION,
      source = source,
      review_status = handoff_status(config$dataminima_handoff$review_status$generation_profile),
      artifact_type = "generation_profile",
      generation_mode = "profile_guided",
      tables = generation_tables
    ),
    issues = issues
  )
}

handoff_quality_baseline <- function(results, record_map, config, source, extra_issues) {
  issues <- extra_issues
  has_issue <- function(dataset_id, issue_type) {
    any(vapply(issues, function(issue) {
      identical(issue$dataset_id, as.character(dataset_id)) && identical(issue$issue_type, issue_type)
    }, logical(1L)))
  }
  existing <- results$quality_issues
  if (is.data.frame(existing) && nrow(existing) > 0L) {
    for (i in seq_len(nrow(existing))) {
      dataset_id <- existing$dataset_id[[i]]
      issue_type <- existing$issue_type[[i]]
      if (identical(issue_type, "import_parsing_problems") && has_issue(dataset_id, issue_type)) next
      issues[[length(issues) + 1L]] <- handoff_issue(
        dataset_id, existing$variable_name[[i]], existing$severity[[i]],
        issue_type, existing$metric[[i]], existing$threshold[[i]],
        existing$message[[i]]
      )
    }
  }
  gaps <- results$documentation_gaps
  if (is.data.frame(gaps) && nrow(gaps) > 0L) {
    for (i in seq_len(nrow(gaps))) {
      issues[[length(issues) + 1L]] <- handoff_issue(
        gaps$dataset_id[[i]], gaps$variable_name[[i]], gaps$severity[[i]],
        gaps$gap_type[[i]], message = gaps$message[[i]]
      )
    }
  }
  import_problems <- results$import_problems
  if (is.data.frame(import_problems) && nrow(import_problems) > 0L) {
    counts <- table(import_problems$dataset_id)
    for (dataset_id in names(counts)) {
      if (has_issue(dataset_id, "import_parsing_problems")) next
      issues[[length(issues) + 1L]] <- handoff_issue(
        dataset_id, severity = "warning", issue_type = "import_parsing_problems",
        metric = as.integer(counts[[dataset_id]]), threshold = 0,
        message = "One or more input parsing problems were recorded; operational reader diagnostics are excluded from the handoff."
      )
    }
  }
  for (item in handoff_declarations(config$dataminima_handoff$profiles$invalid_value_rules)) {
    table <- handoff_safe_identifier(item$table, "Invalid-value rule table")
    field <- handoff_safe_identifier(item$field, "Invalid-value rule field")
    values <- handoff_get_vector(record_map, table, field)
    missing <- safe_missing_mask(values)
    invalid <- rep(FALSE, length(values))
    if (!is.null(item$minimum)) invalid <- invalid | (!missing & suppressWarnings(as.numeric(values)) < as.numeric(item$minimum))
    if (!is.null(item$maximum)) invalid <- invalid | (!missing & suppressWarnings(as.numeric(values)) > as.numeric(item$maximum))
    allowed <- as.character(unlist(item$allowed_values %||% character(), use.names = FALSE))
    if (length(allowed) > 0L) invalid <- invalid | (!missing & !(canonical_values(values) %in% allowed))
    count <- sum(invalid, na.rm = TRUE)
    if (count > 0L) {
      issues[[length(issues) + 1L]] <- handoff_issue(
        table, field, "warning", "invalid_values", metric = count, threshold = 0,
        message = "Observed values fell outside the explicitly configured validation rule."
      )
    }
  }
  review <- config$dataminima_handoff$aggregate_privacy$review
  issues[[length(issues) + 1L]] <- handoff_issue(
    as.character(results$dataset_metadata$dataset_id[[1L]]), severity = "info",
    issue_type = "aggregate_disclosure_review",
    message = paste0("Aggregate disclosure-risk review decision: ", as.character(review$decision %||% "pending"), ".")
  )
  severity <- vapply(issues, function(item) item$severity, character(1L))
  summary <- list(
    info = as.integer(sum(severity == "info")),
    warnings = as.integer(sum(severity == "warning")),
    errors = as.integer(sum(severity == "error"))
  )
  list(
    artifact_schema_version = DATAMINIMA_HANDOFF_SCHEMA_VERSION,
    source = source,
    review_status = handoff_status(config$dataminima_handoff$review_status$quality_baseline),
    artifact_type = "quality_baseline",
    summary = summary,
    issues = issues
  )
}

handoff_csv_text <- function(frame, columns) {
  frame <- frame[columns]
  connection <- textConnection("lines", open = "w", local = TRUE)
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  utils::write.csv(frame, connection, row.names = FALSE, na = "", quote = TRUE)
  close(connection)
  paste0(paste(lines, collapse = "\n"), "\n")
}

handoff_contract_dir <- function() {
  resolved_root <- get0("repo_root", envir = parent.frame(), inherits = TRUE, ifnotfound = NULL)
  if (is.null(resolved_root)) resolved_root <- find_repo_root()
  file.path(resolved_root, "contracts", "dataflow-handoff", DATAMINIMA_HANDOFF_SCHEMA_VERSION)
}

handoff_validate_json_schema <- function(json, schema_filename, context) {
  require_package("jsonvalidate", "DataMinima contract schema validation")
  schema_path <- file.path(handoff_contract_dir(), schema_filename)
  if (!file.exists(schema_path)) stop("DataMinima contract schema is missing: ", schema_filename, call. = FALSE)
  valid <- jsonvalidate::json_validate(json, schema_path, verbose = TRUE, greedy = TRUE, engine = "ajv")
  if (!isTRUE(valid)) {
    errors <- attr(valid, "errors")
    safe_columns <- intersect(c("instancePath", "schemaPath", "keyword", "message"), names(errors))
    error_text <- if (length(safe_columns) == 0L) "contract mismatch" else paste(
      apply(errors[safe_columns], 1L, function(row) paste(row, collapse = " ")),
      collapse = "; "
    )
    stop(context, " failed DataMinima contract validation: ", error_text, call. = FALSE)
  }
  invisible(TRUE)
}

handoff_validate_categorical_rows <- function(frame) {
  if (nrow(frame) > 250000L) stop("categorical_levels.csv exceeds the contract row limit.", call. = FALSE)
  if (nrow(frame) == 0L) return(invisible(TRUE))
  for (i in seq_len(nrow(frame))) {
    row <- as.list(frame[i, DATAMINIMA_CATEGORY_COLUMNS, drop = FALSE])
    row$variable_position <- as.integer(row$variable_position)
    row$level_order <- as.integer(row$level_order)
    row$count <- as.integer(row$count)
    row$percent_of_nonmissing <- as.numeric(row$percent_of_nonmissing)
    row$percent_of_all <- as.numeric(row$percent_of_all)
    row$is_missing <- as.logical(row$is_missing)
    row$is_other_unreported <- as.logical(row$is_other_unreported)
    if (any(vapply(row, function(value) nchar(as.character(value), type = "bytes") > 16 * 1024, logical(1L)))) {
      stop("categorical_levels.csv contains a cell above the contract size limit.", call. = FALSE)
    }
    handoff_validate_json_schema(handoff_json(row, pretty = FALSE), "categorical_level_row.schema.json", paste0("categorical_levels.csv row ", i + 1L))
  }
  invisible(TRUE)
}

handoff_manifest <- function(directory, config, source) {
  media_types <- c(
    "data_dictionary.json" = "application/json",
    "statistical_profile.json" = "application/json",
    "generation_profile.yml" = "application/yaml",
    "categorical_levels.csv" = "text/csv",
    "quality_baseline.json" = "application/json"
  )
  groups <- c(
    "data_dictionary.json" = "dictionary",
    "statistical_profile.json" = "profile",
    "generation_profile.yml" = "profile",
    "categorical_levels.csv" = "profile",
    "quality_baseline.json" = "quality"
  )
  status_names <- c(
    "data_dictionary.json" = "data_dictionary",
    "statistical_profile.json" = "statistical_profile",
    "generation_profile.yml" = "generation_profile",
    "categorical_levels.csv" = "categorical_levels",
    "quality_baseline.json" = "quality_baseline"
  )
  rows <- lapply(sort(DATAMINIMA_PAYLOAD_ARTIFACTS), function(name) {
    path <- file.path(directory, name)
    data.frame(
      artifact = name,
      relative_path = name,
      artifact_group = unname(groups[[name]]),
      media_type = unname(media_types[[name]]),
      schema_version = DATAMINIMA_HANDOFF_SCHEMA_VERSION,
      dataflow_run_id = source$run_id,
      created_at = source$created_at,
      source_product_version = source$product_version,
      source_pipeline_version = source$pipeline_version,
      configuration_sha256 = source$configuration_sha256,
      review_status = handoff_status(config$dataminima_handoff$review_status[[status_names[[name]]]]),
      size_bytes = as.integer(file.info(path)$size),
      sha256 = handoff_sha256_file(path),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)[DATAMINIMA_MANIFEST_COLUMNS]
}

handoff_manifest_document <- function(manifest) {
  entries <- lapply(seq_len(nrow(manifest)), function(i) {
    row <- as.list(manifest[i, , drop = FALSE])
    row$size_bytes <- as.integer(row$size_bytes)
    row
  })
  list(manifest_schema_version = DATAMINIMA_HANDOFF_SCHEMA_VERSION, entries = entries)
}

handoff_assert_privacy_boundary <- function(paths, results) {
  text <- paste(vapply(paths, function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), character(1L)), collapse = "\n")
  forbidden_fields <- c("source_file", "source_file_md5", "reader_function", "reader_package", "file_owner", "file_permissions", "examples", "run_comment", "output_directory")
  for (field in forbidden_fields) {
    if (grepl(paste0('"', field, '"'), text, fixed = TRUE)) stop("DataMinima handoff contains prohibited operational field: ", field, call. = FALSE)
  }
  source_paths <- unique(as.character(results$file_metadata$source_file %||% character()))
  source_paths <- source_paths[!is.na(source_paths) & nzchar(source_paths)]
  if (any(vapply(source_paths, function(path) grepl(path, text, fixed = TRUE), logical(1L)))) {
    stop("DataMinima handoff contains a private source path.", call. = FALSE)
  }
  invisible(TRUE)
}

write_dataminima_handoff <- function(results, records, output_dir, config) {
  validate_dataminima_handoff_config(config$dataminima_handoff)
  if (!is.data.frame(results$errors) || nrow(results$errors) > 0L) {
    stop("DataMinima handoff creation fails closed when ingestion or profiling errors are present.", call. = FALSE)
  }
  if (length(records) == 0L) stop("DataMinima handoff requires at least one successfully profiled dataset.", call. = FALSE)
  require_package("zip", "the exact DataMinima handoff archive")

  record_map <- handoff_record_map(records)
  handoff_validate_field_references(results, config, record_map)
  configured_run_id <- config$dataminima_handoff$run_id
  run_id <- if (is_blank_scalar(configured_run_id)) format(Sys.time(), tz = "UTC", format = "run_%Y%m%dT%H%M%SZ") else as.character(configured_run_id)
  run_id <- handoff_safe_identifier(run_id, "DataMinima run_id")
  configured_created_at <- config$dataminima_handoff$created_at
  created_at <- if (is_blank_scalar(configured_created_at)) {
    format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  } else {
    as.character(configured_created_at)
  }
  if (!handoff_timezone_aware(created_at)) stop("DataMinima created_at must be timezone-aware.", call. = FALSE)
  configuration_sha256 <- handoff_sha256_raw(charToRaw(handoff_canonical_configuration(config)))
  source <- handoff_source(config, run_id, created_at, configuration_sha256)

  dictionary <- handoff_dictionary(results, source, config$dataminima_handoff$review_status$data_dictionary)
  sensitive_fields <- handoff_sensitive_fields(results, config)
  categorical <- handoff_apply_categorical_privacy(results, config, sensitive_fields)
  profiles <- handoff_build_profiles(results, record_map, config, source, categorical)
  quality <- handoff_quality_baseline(results, record_map, config, source, profiles$issues)

  handoff_dir <- file.path(output_dir, "dataminima_handoff")
  if (dir.exists(handoff_dir)) unlink(handoff_dir, recursive = TRUE, force = TRUE)
  safe_dir_create(handoff_dir)
  payloads <- c(
    data_dictionary = file.path(handoff_dir, "data_dictionary.json"),
    statistical_profile = file.path(handoff_dir, "statistical_profile.json"),
    generation_profile = file.path(handoff_dir, "generation_profile.yml"),
    categorical_levels = file.path(handoff_dir, "categorical_levels.csv"),
    quality_baseline = file.path(handoff_dir, "quality_baseline.json")
  )
  handoff_write_utf8(handoff_json(dictionary), payloads[["data_dictionary"]])
  handoff_write_utf8(handoff_json(profiles$statistical), payloads[["statistical_profile"]])
  # JSON is a strict, non-executable YAML 1.2 subset and cannot contain YAML tags, anchors, or aliases.
  handoff_write_utf8(handoff_json(profiles$generation), payloads[["generation_profile"]])
  handoff_write_utf8(handoff_csv_text(categorical$rows, DATAMINIMA_CATEGORY_COLUMNS), payloads[["categorical_levels"]])
  handoff_write_utf8(handoff_json(quality), payloads[["quality_baseline"]])

  handoff_validate_json_schema(readChar(payloads[["data_dictionary"]], file.info(payloads[["data_dictionary"]])$size), "data_dictionary.schema.json", "data_dictionary.json")
  handoff_validate_json_schema(readChar(payloads[["statistical_profile"]], file.info(payloads[["statistical_profile"]])$size), "statistical_profile.schema.json", "statistical_profile.json")
  handoff_validate_json_schema(readChar(payloads[["generation_profile"]], file.info(payloads[["generation_profile"]])$size), "generation_profile.schema.json", "generation_profile.yml")
  handoff_validate_json_schema(readChar(payloads[["quality_baseline"]], file.info(payloads[["quality_baseline"]])$size), "quality_baseline.schema.json", "quality_baseline.json")
  handoff_validate_categorical_rows(categorical$rows)

  manifest <- handoff_manifest(handoff_dir, config, source)
  manifest_path <- file.path(handoff_dir, "artifact_manifest.csv")
  handoff_write_utf8(handoff_csv_text(manifest, DATAMINIMA_MANIFEST_COLUMNS), manifest_path)
  handoff_validate_json_schema(handoff_json(handoff_manifest_document(manifest), pretty = FALSE), "artifact_manifest.schema.json", "artifact_manifest.csv")

  bundle_members <- c(unname(payloads), manifest_path)
  sizes <- file.info(bundle_members)$size
  limits <- DATAMINIMA_ARTIFACT_SIZE_LIMITS[basename(bundle_members)]
  if (any(is.na(sizes)) || any(sizes > limits)) {
    stop("A DataMinima handoff artifact exceeds its contract size limit.", call. = FALSE)
  }
  handoff_assert_privacy_boundary(bundle_members, results)
  review_path <- file.path(output_dir, "dataminima_disclosure_review.json")
  handoff_write_utf8(handoff_json(handoff_review_evidence(config, source)), review_path)

  fixed_time <- suppressWarnings(as.POSIXct(created_at, tz = "UTC"))
  if (!is.na(fixed_time)) Sys.setFileTime(bundle_members, fixed_time)
  bundle_path <- file.path(output_dir, "dataminima_handoff.zip")
  if (file.exists(bundle_path)) unlink(bundle_path, force = TRUE)
  zip::zipr(
    zipfile = bundle_path,
    files = sort(basename(bundle_members)),
    root = handoff_dir,
    include_directories = FALSE,
    compression_level = 0
  )
  inventory <- utils::unzip(bundle_path, list = TRUE)$Name
  expected <- sort(c(DATAMINIMA_PAYLOAD_ARTIFACTS, "artifact_manifest.csv"))
  if (!identical(sort(inventory), expected) || length(inventory) != 6L) {
    stop("DataMinima ZIP did not contain exactly the six allowlisted root artifacts.", call. = FALSE)
  }
  log_info("Created governed DataMinima handoff bundle: ", normalizePath(bundle_path, winslash = "/", mustWork = TRUE))
  c(bundle_members, review_path, bundle_path)
}
