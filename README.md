# DataFlow Platform

A local-first, extensible R platform for reproducible research-data pipelines. The first pipeline creates a data dictionary and metadata bundle from common statistical and tabular file formats.

## Privacy model

The repository contains code, configuration, tests, and synthetic examples only. Users clone the repository and run it against files stored on their own computer or institutional server. The pipeline does not upload data or call an external data-processing service.

## Supported inputs

- CSV, TSV, and delimited text
- Excel `.xls` and `.xlsx` (all worksheets by default)
- Stata `.dta`
- SAS `.sas7bdat` and `.xpt`
- SPSS `.sav`, `.zsav`, and `.por`
- R `.rds`, `.rda`, and `.RData`
- JSON, JSONL, and NDJSON
- Optional Parquet and Feather through `arrow`

Python-created datasets should be exported to CSV, JSON, Parquet, or Feather. Unsafe pickle ingestion is intentionally excluded from the initial tested release.

## Quick start

```bash
Rscript scripts/bootstrap.R
Rscript scripts/generate_examples.R
Rscript scripts/run_data_dictionary.R \
  --input examples/data \
  --output outputs/example \
  --max-categorical-levels 10
```

## Character classification

Plain character variables with at most 10 distinct nonblank values are considered categorically eligible by default. Character variables exceeding the threshold are reported as non-categorical and classified as likely identifiers, free text, or high-cardinality text. Explicit factors and imported labelled variables remain categorical.

## Outputs

Each run produces:

- `variable_dictionary.csv`
- `dataset_metadata.csv`
- `categorical_levels.csv`
- `quality_issues.csv`
- `metadata.json`
- `metadata.xlsx`
- `metadata_report.html`
- `resolved_config.yml`

## Tests

```bash
Rscript scripts/run_tests.R
```

GitHub Actions runs the tests and the synthetic example pipeline on every push and pull request.

## Repository direction

The initial release deliberately focuses on completing and validating the data-dictionary pipeline. Future pipelines will live under `pipelines/` and reuse shared ingestion, configuration, logging, and metadata conventions.
