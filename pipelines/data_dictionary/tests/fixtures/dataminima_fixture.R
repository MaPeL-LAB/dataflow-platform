dataminima_fixture <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  people <- data.frame(
    entity_key = sprintf("K%03d", seq_len(20L)),
    age = c(21.123456789, 22:39, 999),
    region = rep(c("East", "West"), each = 10L),
    risk_band = c(rep("low", 10L), rep("high", 8L), "rare_one", "rare_two"),
    secret_note = paste0("private_fixture_value_", seq_len(20L)),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    entity_key = rep(people$entity_key, each = 2L),
    event_time = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + seq(0, by = 86400, length.out = 40L),
    dose = seq_len(40L),
    score = 3 + 2 * seq_len(40L),
    phase = rep(c("early", "late"), each = 20L),
    outcome = rep(rep(c("yes", "no"), each = 10L), 2L),
    stringsAsFactors = FALSE
  )
  events$score[c(1:5, 21:25)] <- NA_real_
  quality_cases <- data.frame(
    case_key = c("Q001", "Q002", "Q002", "Q003"),
    constant_marker = rep("generated_constant", 4L),
    group = c("alpha", "beta", "beta", "gamma"),
    stringsAsFactors = FALSE
  )
  people_path <- file.path(root, "people.rds")
  events_path <- file.path(root, "events.rds")
  quality_cases_path <- file.path(root, "quality_cases.rds")
  parser_cases_path <- file.path(root, "parser_cases.csv")
  saveRDS(people, people_path, version = 2)
  saveRDS(events, events_path, version = 2)
  saveRDS(quality_cases, quality_cases_path, version = 2)
  writeLines(
    c(
      "row_id,value",
      "1,10",
      "2,20,source_row_marker_must_not_ship",
      "3,30"
    ),
    parser_cases_path,
    useBytes = TRUE
  )
  c(people_path, events_path, quality_cases_path, parser_cases_path)
}

dataminima_fixture_overrides <- function() {
  list(
    privacy = list(include_examples = TRUE),
    output = list(
      write_csv = FALSE, write_json = FALSE, write_excel = FALSE,
      write_html = FALSE, write_legacy_variable_dictionary = FALSE, overwrite = TRUE
    ),
    dataminima_handoff = list(
      enabled = TRUE,
      run_id = "run_fixture_20260813",
      created_at = "2026-08-13T10:00:00+02:00",
      review_status = list(
        data_dictionary = "proposed",
        statistical_profile = "observed",
        generation_profile = "approved",
        categorical_levels = "observed",
        quality_baseline = "observed"
      ),
      instruction_status = "approved",
      profiles = list(
        numeric_fields = c("people.age", "events.dose", "events.score"),
        histogram_bins = 4L,
        conditional_missingness = list(
          list(table = "events", field = "score", conditional_on = "phase")
        ),
        relationships = list(
          list(
            name = "people_events", parent_table = "people", parent_field = "entity_key",
            child_table = "events", child_field = "entity_key", status = "approved"
          )
        ),
        dependencies = list(
          list(table = "events", fields = c("dose", "score"), method = "pearson", status = "approved"),
          list(table = "events", fields = c("phase", "outcome"), method = "cramers_v", status = "approved"),
          list(table = "events", fields = c("phase", "outcome"), method = "conditional_frequency", status = "approved")
        ),
        temporal = list(
          list(
            table = "events", field = "event_time", group_by_field = "entity_key",
            ordering_fields = c("entity_key", "event_time"), seasonality_period = "none_observed",
            interval_unit = "days", status = "approved"
          )
        ),
        invalid_value_rules = list(
          list(table = "people", field = "age", minimum = 0, maximum = 120)
        )
      ),
      aggregate_privacy = list(
        policy_version = "fixture-policy-1.0",
        minimum_cell_size = 5L,
        precision_digits = 6L,
        sensitive_fields = c("people.secret_note"),
        review = list(
          reviewer = "fixture-steward",
          reviewed_at = "2026-08-13T09:30:00+02:00",
          decision = "approved",
          findings = c("Rare categories and sensitive-field statistics are suppressed."),
          residual_limitations = c("Aggregate statistics may still disclose information and do not establish anonymization.")
        )
      )
    )
  )
}

run_dataminima_fixture <- function(root, output, default_config_path, overrides = dataminima_fixture_overrides()) {
  withCallingHandlers(
    run_data_dictionary_pipeline(
      inputs = dataminima_fixture(root),
      output_dir = output,
      default_config_path = default_config_path,
      overrides = overrides
    ),
    warning = function(warning) {
      if (grepl("^One or more parsing issues", conditionMessage(warning))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
