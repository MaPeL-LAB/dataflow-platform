# Data Dictionary Pipeline

This pipeline inventories supported inputs, imports each selected tabular dataset, profiles technical metadata, builds an analyst-facing data dictionary, and exports the dictionary separately from the broader metadata package.

## Run

From the repository root:

```bash
Rscript scripts/run_pipeline.R \
  --pipeline data_dictionary \
  --input path/to/file_or_directory \
  --output outputs/dictionary \
  --overwrite true
```

Use repeated `--input` flags for multiple files or directories. Directory scans are nonrecursive unless `--recursive true` is supplied.

## Main CLI overrides

| Option | Meaning |
|---|---|
| `--config path.yml` | Merge a custom YAML file over the defaults |
| `--recursive true` | Recurse through input directories |
| `--include-hidden true` | Include hidden files during discovery |
| `--excel-sheets all,a,b` | Read all or selected Excel worksheets |
| `--r-objects all,a,b` | Read all or selected objects from `.RData`/`.rda` |
| `--max-categorical-levels 10` | Maximum distinct values for plain character categoricals |
| `--max-categorical-ratio 1.0` | Optional additional uniqueness-ratio ceiling |
| `--id-uniqueness-threshold 0.98` | Threshold used for possible identifier flags |
| `--text-encoding UTF-8` | Encoding for delimited and JSON text |
| `--haven-encoding UTF-8` | Optional encoding override for Stata/SAS/SPSS |
| `--auto-sas-catalog true` | Automatically pair a sibling `.sas7bcat` |
| `--sas-catalog path` | Explicit catalog path, absolute or relative to the SAS data file |
| `--allow-unsafe-pickle true` | Permit loading a trusted Python pickle |
| `--inline-max-values 10` | Maximum values rendered inline before previewing |
| `--preview-values 5` | Number of values shown in a large-domain preview |
| `--include-examples false` | Suppress representative values |
| `--project-name ...` | Add descriptive metadata |
| `--data-owner ...` | Add administrative metadata |
| `--data-steward ...` | Add administrative metadata |
| `--overwrite true` | Permit output in an existing nonempty directory |

The complete configuration is in `config/default.yml`.

## Processing stages

1. Discover files and classify supported, auxiliary, and unsupported extensions.
2. Capture file-system and format metadata for every discovered file.
3. Read supported sources and expand multi-sheet Excel files and multi-object R files into separate datasets.
4. Preserve variable labels, value labels, display formats, units, tagged missing values, and SPSS user-missing definitions where available.
5. Profile dataset shape, missing cells, duplicate rows, memory size, and import diagnostics.
6. Profile every variable and infer semantic role, measurement level, categorical eligibility, and quality/privacy signals.
7. Build a concise data dictionary from the richer variable metadata.
8. Flag missing business definitions and classification-review items rather than inventing domain meaning.
9. Export separate dictionary and metadata products plus a combined HTML report.

## Character cardinality behavior

For a plain character vector, categorical eligibility uses distinct nonblank values. With the default threshold of 10:

```text
2 distinct values   -> categorical
10 distinct values  -> categorical
11 distinct values  -> not categorical; high_cardinality_character flag
```

Potential name, email, phone, UUID, code, and ID fields are additionally flagged. Long strings are distinguished from short high-cardinality text. The rule does not change source values or coerce columns to factors.

Explicit factors and imported value-labelled variables retain their categorical status even when they exceed 10 levels. Their inline dictionary values are previewed, and the complete mappings remain in `categorical_levels` or `value_labels`.

## `func_dictionary()` compatibility helper

The module `R/dictionary.R` provides a direct in-memory helper:

```r
dictionary <- func_dictionary(
  DATA = analysis_data,
  max_categorical_levels = 10,
  dataset_id = "analysis_data"
)
```

It preserves `label`, `variable`, `type`, `value_label`, and `n_missing` as the first five columns, while adding definitions, categorical decisions, semantic types, review status, and exact reasons. It does not install packages during execution. `value_domain_status` distinguishes source-defined mappings and factor domains from values merely observed in the current extract.

## Output schemas

See [`SCHEMA.md`](SCHEMA.md) and [`../../docs/data-dictionary-vs-metadata.md`](../../docs/data-dictionary-vs-metadata.md).
