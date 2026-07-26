# Data Dictionary versus Metadata

## Metadata

Metadata is the broad concept: information that describes other data. In this repository it includes structural, descriptive, administrative, provenance, and quality information.

Examples include file name and size, modification time, data owner, source system, worksheet or R-object name, column type, source display format, missingness, import reader, pipeline version, and access classification.

## Data dictionary

A data dictionary is the structured repository or document that organizes selected metadata into a field-level reference. Once its definitions and rules are curated and approved, it can serve as the authoritative single source of truth. It focuses on the dataset schema and how each field should be understood and used.

Typical dictionary fields include variable name, label, business definition, type, categorical status, value labels/levels, nullability, validation rules, sensitivity, and review status. A list of observed values is not automatically an authoritative allowed-value domain; the pipeline records that distinction explicitly.

## Comparison

| Feature | Metadata | Data dictionary |
|---|---|---|
| Definition | Information that describes other data. | A structured repository or document that catalogs, organizes, and explains selected metadata for a dataset or data system. |
| Purpose | Classify, locate, understand, govern, and trace data. | Provide a consistent field-level reference for interpreting, validating, querying, and maintaining data. |
| Scope | Broad: files, datasets, fields, provenance, ownership, access, quality, and processing context. | Focused: datasets/tables, fields, types, definitions, domains, nullability, constraints, review status, and known relationships. |
| Typical format | Embedded attributes, file headers, logs, catalogs, sidecars, or external tables. | A workbook, table, JSON document, catalog, or managed platform. |
| Examples | File size, modification date, source system, owner, worksheet name, import reader, and missingness. | `Customer_ID` / integer / unique customer identifier / cannot be null / key candidate. |

Metadata supplies the ingredients. The data dictionary is the organized reference that selects, categorizes, and explains the ingredients needed to use the data correctly.

## How the pipeline applies the distinction

The pipeline writes separate products:

- `data_dictionary.xlsx` and `data_dictionary.json` contain the curated field-level reference and its companion value tables.
- `metadata.xlsx` and `metadata.json` contain the broader file, dataset, variable, provenance, administrative, import, and quality context, including the category/value-label ingredients reused by the dictionary.
- `metadata_report.html` presents both but labels them separately.

The detailed `variable_metadata` table is intentionally richer and more technical than the concise `data_dictionary` table. The dictionary is derived from that metadata rather than replacing it.

## Authority and inferred content

The pipeline can observe types, values, ranges, labels, and quality characteristics. It cannot reliably infer the organization’s intended business meaning. Therefore:

- source variable labels may seed `business_definition`, with their unvalidated source recorded;
- missing definitions are left blank and reported in `documentation_gaps`;
- generated descriptions are stored as `technical_definition`;
- observed ranges and nullability are labeled `inferred_validation_rules`, not authoritative policy;
- `value_domain_status` differentiates source-defined, type-defined, and merely observed value domains;
- owner, steward, author, access, license, and source system are accepted through configuration.
