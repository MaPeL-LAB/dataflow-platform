# Validation

## Automated runtime validation

The GitHub Actions workflow has been executed successfully on Ubuntu with R and the required plus optional dependencies. The successful validation covered:

- repository-level pipeline registration tests;
- the reusable future-pipeline template test;
- all data-dictionary classification, dictionary, utility, and integration tests;
- CSV, TSV, configured-delimiter text, XLS, XLSX, Stata, SAS, SAS transport, SPSS, RDS, RData, JSON, JSON Lines, Parquet, Feather, Arrow IPC file/stream, and explicitly trusted Python pickle ingestion paths;
- SAS catalog discovery and optional-argument handling;
- the complete heterogeneous example pipeline, including output generation.

The CI workflow retains the `testthat` log as a short-lived artifact on every run to make any future regression directly inspectable.

## Included automated checks

The repository contains `testthat` tests for:

- inclusive character-cardinality behavior at 10 values and exclusion at 11;
- explicit factor and labelled-variable categorical intent;
- code-to-label direction (`code = label`);
- the original five-column dictionary order and R column-label attributes;
- source-defined versus observed-domain status;
- missing-definition gaps;
- supported-format registration;
- separate data-dictionary and metadata artifacts;
- repository pipeline registration;
- version-robust Excel and Haven package fixtures;
- end-to-end output generation.

`.github/workflows/ci.yml` installs runtime dependencies, runs all tests, uploads the test log, and executes the full example pipeline.

## Static and release checks

The release also underwent the following non-runtime checks:

- all R source files were checked for balanced delimiters, complete strings/comments, and suspicious truncated expressions;
- YAML and JSON files were parsed;
- the included Excel example was opened and its worksheets inspected;
- configuration, registry, output-catalog, and format-matrix contracts were cross-checked;
- the release archive was extracted and verified for integrity;
- SHA-256 checksums were generated for the repository files.

## Local reproduction

```bash
Rscript scripts/bootstrap.R --ci --with-optional
Rscript scripts/run_tests.R
Rscript scripts/run_pipeline.R \
  --pipeline data_dictionary \
  --input examples/data \
  --output outputs/validation \
  --overwrite true
```
