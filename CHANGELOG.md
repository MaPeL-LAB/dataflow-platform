# Changelog

## 0.2.0 - 2026-07-24

- Separated the data dictionary from the broader metadata package conceptually and in generated files.
- Added standalone `data_dictionary.xlsx`/`.json` and `metadata.xlsx`/`.json` products.
- Preserved and improved the analyst-facing `func_dictionary()` interface without runtime package installation or implicit tidyverse dependencies.
- Restored `label`, `variable`, `type`, `value_label`, and `n_missing` as the first five output columns and retained their column-label attributes.
- Corrected value-label rendering to `code = label`.
- Added low-cardinality character values and explicit high-cardinality exclusion messages to the dictionary.
- Added business-definition status, technical definitions, inferred observed rules, review status, and documentation gaps.
- Added `value_domain_status` so observed categories are not misrepresented as authoritative allowed-value rules.
- Added file, provenance, administrative, metadata-coverage, supported-format, and output-table catalog tables.
- Added reader package/function lineage and richer file-system metadata.
- Retained all existing CSV/text, Excel, Stata, SAS, SPSS, R, JSON, Arrow/Parquet, and opt-in Python pickle ingestion families.
- Added tests for the compatibility dictionary, code-label direction, high-cardinality reporting, and separate outputs.

## 0.1.0 - 2026-07-24

- Added the extensible pipeline registry and generic CLI runner.
- Added the data dictionary pipeline.
- Added CSV/TSV/TXT, Excel, Stata, SAS, SPSS, R, JSON, Arrow/Parquet, and opt-in Python pickle readers.
- Added configurable high-cardinality character classification, metadata preservation, quality flags, privacy masking, and CSV/JSON/Excel/HTML exports.
- Added example data, tests, and GitHub Actions CI.
