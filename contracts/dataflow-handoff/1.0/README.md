# DataFlow Handoff Contract 1.0

This directory contains the generated machine-readable schemas for the
DataFlow-to-DataMinima handoff used during DataMinima `0.0.1` development.

The six-artifact bundle has five payload artifacts recorded by
`artifact_manifest.csv`. The manifest itself is validated by
`artifact_manifest.schema.json` and is not included in its own digest list.

`statistical_profile.json` is canonical evidence.
`generation_profile.yml` is a safe, human-reviewable configuration and never
an executable document. DataMinima generates only from explicitly approved
parameters.

The source models live in
`src/metadata_synthetic_data_platform/dataflow_contracts.py` and
`src/metadata_synthetic_data_platform/dataflow_mapping.py`. DataFlow runs
independently; DataMinima neither launches its runtime nor accepts source rows.
Contract `1.0` accepts only DataFlow product and `data_dictionary` pipeline
version `0.2.0`. Every governed artifact and manifest row carries the SHA-256
of the canonical resolved producer configuration.
Regenerate the model-derived schemas from the repository root:

```bash
.venv/bin/python scripts/write_dataflow_contract_schemas.py
```

`manual_dictionary_fallback_row.schema.json` is the separately maintained,
metadata-only CSV fallback contract. Its header allowlist is enforced by
`dictionary.py` and checked against the published schema by tests; it is not a
seventh DataFlow bundle artifact and carries no aggregate-profile authority.

Do not add source rows, private file-system paths, credentials, unmasked
examples, or unnecessary operational metadata to this contract.
