# Improvements to the Original `func_dictionary()` Pattern

The pipeline preserves the original function’s useful analyst-facing columns while correcting several implementation and semantics issues.

## Preserved ideas

- variable labels first;
- variable names and R classes;
- inline value labels or factor levels;
- missing counts;
- readable output suitable for an analyst;
- the original five columns remain first and retain readable column-label attributes.

## Improvements

1. **No runtime package installation.** Analysis functions fail with an actionable dependency message instead of modifying the user’s R library.
2. **No implicit tidyverse dependency.** The compatibility helper uses base R plus the pipeline modules.
3. **Correct label direction.** Imported labels are rendered as `raw code = human label`, not `label = code`.
4. **Character categories are included.** Character fields at or below the configured threshold list their observed categories.
5. **High-cardinality characters are explicit.** Fields above the threshold state that they are not categorical and why.
6. **Factors retain source intent.** Explicit factors remain categorical even when they have many levels; the inline output is previewed and the complete level table is retained.
7. **Multiple datasets are safe.** Dataset IDs and variable positions disambiguate workbook sheets, R objects, duplicate names, and blank names.
8. **Metadata is not conflated with the dictionary.** The dictionary and the broader metadata package are exported separately.
9. **Definitions are governed honestly.** Missing business definitions are flagged rather than fabricated.
10. **Value-domain authority is explicit.** Source-defined mappings, explicit factor domains, logical domains, and merely observed values are not conflated.
11. **Privacy and quality context is included.** Identifier, sensitivity, missingness, whitespace, parsing, duplication, and cardinality signals are recorded.
12. **All established ingestion families remain available.** CSV/text, Excel, Stata, SAS, SPSS, R, JSON/JSONL, Parquet/Feather/Arrow IPC, and explicitly enabled trusted Python pickle inputs use the same profiling and output model.
