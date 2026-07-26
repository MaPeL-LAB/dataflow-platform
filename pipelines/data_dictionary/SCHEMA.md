# Output Schema

The output is divided into a **data dictionary product** and a broader **metadata product**.

## Data dictionary product

### `data_dictionary`

One row per field. The first five analyst-facing columns retain the attached helper’s pattern and order:

1. `label`: source variable label;
2. `variable`: field name;
3. `type`: imported R class;
4. `value_label`: inline code-label mappings, levels, observed character categories, or a high-cardinality exclusion message;
5. `n_missing`: effective missing count.

Each of these and the extended fields also carries a readable R column-label attribute.

Additional groups include:

#### Value-domain interpretation

- `value_representation` explains what is displayed in `value_label`.
- `value_domain_status` states the authority of that display: `source-defined code-label mapping`, `source-defined factor domain`, `type-defined logical domain`, `observed values only`, or `not enumerated`.
- Observed low-cardinality character values are profiling evidence; they are not automatically declared to be a governed allowed-value list.

#### Identity and classification

- `dataset_id`, `dataset_name`, `variable_position`, `source_variable_name`
- `categorical_eligible`, `categorical_status`
- `semantic_type`, `measurement_level`
- `classification_reason`

#### Definitions and governance readiness

- `business_definition`
- `definition_source`
- `documentation_status`
- `technical_definition`
- `inferred_validation_rules`
- `review_required`, `review_reason`

#### Structure, completeness, and privacy

- `source_format`, `storage_type`, `source_display_format`, `units`
- `n_missing`, `n_system_missing`, `n_blank_strings`, `percent_missing`
- `n_unique_non_missing`, `nullable_observed`
- `potential_identifier`, `potential_sensitive`
- `examples` subject to configured masking

### `categorical_levels`

Observed or defined level detail for categorical fields. Includes frequency, percentage, missing counts, value labels, and an `OTHER_UNREPORTED` row when the configured report limit is exceeded.

### `value_labels`

Complete imported code-to-label mappings from Stata, SAS, SPSS, or labelled R vectors, including labels not observed in the current data.

### `documentation_gaps`

One row per item needing human curation or confirmation. Current gap types include:

- `missing_business_definition`
- `classification_review`

### `dictionary_schema`

One row per `data_dictionary` column with its definition.

## Metadata product

### `run_metadata`

Pipeline version, start/completion time, duration, R version, platform, package versions, and output location.

### `summary`

Counts of inputs, successful/failed files, datasets, variables, dictionary rows, categorical fields, noncategorical character fields, missing definitions, import problems, quality flags, and errors.

### `administrative_metadata`

Optional project name/description, author, data owner, steward, source system, access classification, permissions, license, and tags supplied in configuration.

### `metadata_coverage`

Explains structural, descriptive, administrative, provenance, and quality metadata coverage and identifies the primary tables for each category.

### `supported_formats`

Machine-readable ingestion capability matrix with formats, extensions, readers, availability, and notes.

### `table_catalog`

Catalog of generated tables, their grain, artifact group, and purpose.

### `input_inventory`

One row per discovered file with format, support status, ingestion status, datasets created, and error message.

### `file_metadata`

One row per discovered file. Includes path, extension, format, compression, size, creation time when the file system exposes it, modified/access/status-change times, file-system owner/group/permissions, optional MD5, support status, and ingestion outcome.

### `dataset_metadata`

One row per imported dataset, worksheet, or R/Python object. Includes source, reader package/function, file attributes, dimensions, missing/blank cells, duplicate rows, parsing-problem count, and in-memory size.

### `provenance_metadata`

Source-to-dataset lineage with source file/sheet/object, reader package/function, optional hash, pipeline name/version, import time, and resulting dimensions.

### `variable_metadata`

The detailed technical profile used to derive the dictionary. Important field groups include:

- labels, classes, storage types, source display formats, units;
- semantic type, measurement level, categorical eligibility and reason;
- system/effective missingness, blanks, cardinality, uniqueness;
- factor/value-label counts and special missing definitions;
- numeric summaries and infinite values;
- date/datetime ranges and time zones;
- text lengths, whitespace, examples;
- identifier and sensitivity hints.

### `import_problems`

Delimited-text parsing diagnostics reported by `readr`.

### `quality_issues`

One row per observed flag. Common values include:

- `high_cardinality_character`
- `potential_identifier`
- `potential_sensitive_data`
- `high_missingness`
- `all_missing`
- `constant`
- `blank_strings`
- `leading_trailing_whitespace`
- `infinite_numeric_values`
- `duplicate_rows`
- `duplicate_variable_name`
- `duplicate_check_skipped`
- `import_parsing_problems`

### `errors`

Ingestion or profiling exceptions captured when `input.continue_on_error` is enabled.

## Shared companion metadata

`categorical_levels`, `value_labels`, and `documentation_gaps` are members of both conceptual products. They are included in the dictionary package because they complete the field reference, and in the metadata package because they are descriptive/structural metadata. Their CSV files are emitted once.
