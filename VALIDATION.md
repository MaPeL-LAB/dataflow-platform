# Validation

## Included automated runtime checks

The repository contains `testthat` tests for:

- inclusive character-cardinality behavior at 10 values and exclusion at 11;
- explicit factor and labelled-variable categorical intent;
- code-to-label direction (`code = label`);
- the original five-column dictionary order and R column-label attributes;
- source-defined versus observed-domain status;
- missing-definition gaps;
- supported-format registration;
- separate data-dictionary and metadata artifacts;
- repository pipeline registration.

`.github/workflows/ci.yml` installs required R packages, runs all tests, and executes the full example pipeline.

## Release checks performed when this bundle was assembled

- all R source files were checked for balanced delimiters, complete strings/comments, and suspicious truncated expressions;
- YAML and JSON files were parsed;
- the included Excel example was opened and its worksheets inspected;
- configuration, registry, output-catalog, and format-matrix contracts were cross-checked;
- the ZIP archive was extracted and verified for integrity;
- SHA-256 checksums were generated for the repository files and final archive.

An R executable was not present in the assembly container, so the `testthat` suite could not be executed there. Runtime execution is delegated to the included CI workflow and should also be run locally with:

```bash
Rscript scripts/bootstrap.R --ci
Rscript scripts/run_tests.R
Rscript scripts/run_pipeline.R \
  --pipeline data_dictionary \
  --input examples/data \
  --output outputs/validation \
  --overwrite true
```
