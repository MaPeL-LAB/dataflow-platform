# Adding a Pipeline

The root framework intentionally has a small contract.

## 1. Copy the template or create the folder

Start with `pipelines/_template/` or create the following structure:

```text
pipelines/my_pipeline/
├── config/default.yml
├── R/
├── tests/testthat/
├── README.md
└── run.R
```

Shared helpers from `R/common.R` and registry information are already available when the entrypoint is sourced.

## 2. Define the entrypoint

`run.R` must define:

```r
run_pipeline_cli <- function(args, pipeline, repo_root) {
  # Parse pipeline-specific options.
  # Run the work synchronously.
  # Return an invisible result object.
}
```

Keep pipeline implementation code in its own `R/` directory. Source those modules from `run.R` so names do not need to be added to the repository-wide framework.

## 3. Register it

Add an entry to `config/pipelines.yml`:

```yaml
pipelines:
  my_pipeline:
    version: 0.1.0
    description: What the pipeline does.
    entrypoint: pipelines/my_pipeline/run.R
    default_config: pipelines/my_pipeline/config/default.yml
```

Verify registration:

```bash
Rscript scripts/list_pipelines.R
```

## 4. Follow repository conventions

- Treat the `--output` directory as the pipeline's artifact root.
- Write a resolved configuration alongside outputs.
- Provide machine-readable tables plus a readable summary when appropriate.
- Record recoverable file-level errors rather than silently dropping inputs.
- Put unit/integration tests under the pipeline's `tests/testthat/` directory.
- Add required packages to `DESCRIPTION` and `scripts/bootstrap.R`; keep heavy format-specific packages optional where possible.
- Document security-sensitive formats and default them to the safer behavior.
