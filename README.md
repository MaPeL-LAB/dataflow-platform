# Data Pipeline Suite

An extensible R repository for reusable data pipelines. The first registered pipeline, `data_dictionary`, imports one or more heterogeneous data sources and produces **two separate but linked products**:

1. **Metadata** — data about the files, datasets, variables, provenance, administration, ingestion process, and observed quality.
2. **A data dictionary** — the structured field-level reference that organizes the most useful metadata into a reviewable reference that can become the single source of truth after curation and approval.

The repository is designed for more than one pipeline. Shared command-line handling, configuration, tests, CI, documentation, and pipeline registration live at the root. Each pipeline owns its implementation under `pipelines/<pipeline_name>/`.

## Repository layout

```text
data-pipeline-suite/
├── dataflow                         # macOS/Linux Bash launcher
├── DataFlow.command                 # double-click macOS launcher
├── DataFlow.ps1                     # Windows PowerShell launcher
├── DataFlow-Windows.cmd             # double-click Windows launcher
├── config/pipelines.yml
├── R/                               # shared framework helpers
├── scripts/                         # runtime check, bootstrap, run, and test entrypoints
├── pipelines/
│   ├── _template/                   # copyable skeleton for future pipelines
│   └── data_dictionary/
│       ├── config/default.yml
│       ├── R/
│       │   ├── ingest.R             # multi-format readers
│       │   ├── classify.R           # semantic/categorical rules
│       │   ├── profile.R            # technical metadata
│       │   ├── dictionary.R         # analyst-facing dictionary + func_dictionary()
│       │   ├── report.R
│       │   ├── export.R
│       │   └── pipeline.R
│       ├── tests/testthat/
│       └── README.md
├── examples/
├── docs/
├── tests/testthat/
└── outputs/
```

## Quick start

Requirements:

- Windows, macOS, or Linux;
- R 4.1 or later, with `Rscript` available on `PATH`; and
- the required R packages.

After cloning the repository, install missing packages once:

```text
Rscript --vanilla scripts/bootstrap.R
```

The bootstrap step may contact CRAN to install missing packages. Normal pipeline
runs are strictly local and do not install software, call an LLM, send
telemetry, or access a network service.

## Local project launcher

The guided workflow is available on all three major desktop operating systems.

| Operating system | Guided launch | Terminal launch |
|---|---|---|
| macOS | Double-click `DataFlow.command` | `./dataflow` |
| Linux | Run `./dataflow` | `./dataflow --project ...` |
| Windows | Double-click `DataFlow-Windows.cmd` | `.\DataFlow.ps1` |

The launcher asks for a project name, one or more input paths, an output root, a
run mode, an optional comment, and reusable public metadata.

### macOS and Linux

```bash
./dataflow \
  --project "Study ABC" \
  --input "/path/to/study/data" \
  --output-root "/path/to/study/outputs" \
  --mode replace \
  --comment "Updated after investigator review"
```

On macOS, HTML previews open in Safari. On Linux, the launcher uses `xdg-open`
or `gio` to open the system browser when available.

### Windows

From PowerShell:

```powershell
.\DataFlow.ps1 `
  -Project "Study ABC" `
  -InputPaths "C:\path\to\study\data" `
  -OutputRoot "C:\path\to\study\outputs" `
  -Mode replace `
  -Comment "Updated after investigator review" `
  -OpenReport true
```

`DataFlow-Windows.cmd` starts the same PowerShell workflow in interactive mode.
It does not permanently change the machine's PowerShell execution policy. On
managed computers where local scripts are prohibited by organisational policy,
users can still run the shared R entrypoint directly.

### Shared version and metadata behaviour

Each project receives dated output under `versions/`, a portable `current.txt`
pointer, and—when the filesystem permits it—a convenient `current` link or
Windows junction.
`version` creates a dated run without changing an existing current version.
`replace` safely promotes the new run to current while retaining the prior
version. Run comments are included in the metadata files and internal technical
HTML report.

On the first interactive run for a project, the launcher offers to collect and
save the public metadata used by the open-science report:

- project description;
- author list or preferred dataset citation;
- license;
- access classification;
- access conditions; and
- comma-separated tags.

The values are stored locally in
`<output-root>/<project-slug>/project_metadata.yml`. Later runs reuse that
profile by default and offer an edit option. Press Return to retain a displayed
value, or enter a single `-` to clear it. The profile is separate from dated run
outputs, so replacing the current run does not remove it. When an existing
profile is edited, the prior file is retained as `project_metadata.previous.yml`.

Non-interactive Bash and PowerShell runs automatically use the saved project
profile when it exists. Advanced Bash users can still pass their own pipeline
configuration after `--`; an explicit `--config` takes precedence over the
saved profile. The cross-platform R entrypoint remains available for automation:

```text
Rscript --vanilla scripts/run_pipeline.R --pipeline data_dictionary --input <path> --output <path>
```

Install optional Arrow/Parquet and trusted Python-pickle support with:

```bash
Rscript scripts/bootstrap.R --with-optional
```

## The improved analyst-facing dictionary

The pipeline retains the useful shape of the attached `func_dictionary()` helper. Its original five analyst-facing columns remain the **first five columns, in the same order**:

1. source variable label (`label`);
2. variable name (`variable`);
3. R class (`type`);
4. value labels or levels (`value_label`);
5. missing count (`n_missing`).

Dataset identity and the extended technical/governance fields follow those five columns.

It then adds categorical eligibility, cardinality, definitions, documentation status, semantic and measurement types, observed rules, source formats, identifier/sensitivity flags, and exact classification reasons.

The compatibility function can still be used directly on an in-memory object:

```r
source("R/common.R")
for (module in c(
  "config.R", "utils.R", "ingest.R", "classify.R", "profile.R", "dictionary.R"
)) {
  source(file.path("pipelines", "data_dictionary", "R", module))
}

dictionary <- func_dictionary(
  DATA = my_data,
  max_categorical_levels = 10,
  dataset_id = "analysis_data"
)
```

A runnable example is included at `examples/in_memory_dictionary.R`.

Unlike the original helper, this version:

- never installs packages during analysis;
- does not depend on `%>%`, `mutate()`, or `map_chr()`;
- correctly prints imported mappings as `code = label`;
- includes observed low-cardinality character values;
- explicitly reports why high-cardinality character fields are not categorical;
- keeps full mappings and frequencies in companion tables;
- marks whether a domain is source-defined, type-defined, or merely observed in `value_domain_status`;
- retains readable column-label attributes on the returned R data frame;
- flags missing business definitions instead of inventing them.

## Character versus categorical classification

The default rule is configurable and visible:

- A plain character variable with **10 or fewer** distinct nonblank values is eligible to be categorical.
- A plain character variable with **more than 10** distinct nonblank values is marked `categorical_eligible = FALSE` and classified as `high_cardinality_text`, `free_text`, or `identifier`.
- The data dictionary states the decision in `categorical_status` and `value_label`.
- The detailed variable metadata stores the observed distinct count, uniqueness ratio, semantic type, and exact `classification_reason`.
- The quality table adds `high_cardinality_character`.
- Explicit factors, ordered factors, and imported Stata/SAS/SPSS value-labelled variables remain categorical because the source explicitly encoded categorical intent.

Change the threshold at runtime:

```bash
Rscript scripts/run_pipeline.R \
  --pipeline data_dictionary \
  --input data/my_file.xlsx \
  --output outputs/my_dictionary \
  --max-categorical-levels 15 \
  --overwrite true
```

## Supported input formats

| Family | Extensions | Reader | Notes |
|---|---|---|---|
| Delimited text | `.csv`, `.tsv`, `.tab`, `.txt` | `readr` | Configurable text delimiter and encoding |
| Excel | `.xls`, `.xlsx` | `readxl` | All worksheets by default |
| Stata | `.dta` | `haven` | Preserves labels, formats, dates, and tagged missing metadata where available |
| SAS | `.sas7bdat`, `.xpt`; `.sas7bcat` auxiliary | `haven` | Matching catalogs can be used automatically |
| SPSS | `.sav`, `.zsav`, `.por` | `haven` | User-defined missing values are retained |
| R | `.rds`, `.rda`, `.RData` | base R | Multiple tabular objects are supported |
| JSON | `.json`, `.jsonl`, `.ndjson` | `jsonlite` | Record arrays and named tabular members are normalized |
| Arrow/cross-language | `.parquet`, `.feather`, `.arrow`, `.ipc` | optional `arrow` | IPC file and stream encodings are supported |
| Python pickle | `.pkl`, `.pickle` | optional `reticulate` | Disabled by default; trusted files only |

A `.py` program is source code, not a tabular data file. Python-created datasets should usually be exchanged through CSV, JSON, Parquet, Feather/Arrow, or another interoperable format. Pickle is available only after explicit opt-in.

Compressed delimited, JSON, Stata, SAS, and SPSS inputs with `.gz`, `.bz2`, or `.xz` suffixes are recognized where supported by the reader.

Haven-family readers run in an isolated local R worker so a native reader crash
cannot terminate the main pipeline. For legacy Stata 114/115 files with a
malformed trailing region, the pipeline can recover the valid data block with a
base-R safety reader. That recovery preserves raw coded values, variable labels,
and display formats, while clearly recording that trailing value-label mappings
were not trusted or imported.

## Outputs

Each run can produce:

```text
<output>/
├── data_dictionary.xlsx             # field-level reference workbook
├── data_dictionary.json             # field-level machine-readable product
├── metadata.xlsx                    # full metadata workbook
├── metadata.json                    # full machine-readable metadata product
├── metadata_report.html             # comprehensive internal technical report
├── open_science_metadata_report.html # concise, printable public metadata report
├── resolved_config.yml
├── artifact_manifest.csv
└── csv/
    ├── data_dictionary.csv
    ├── categorical_levels.csv
    ├── value_labels.csv
    ├── documentation_gaps.csv
    ├── dictionary_schema.csv
    ├── run_metadata.csv
    ├── summary.csv
    ├── administrative_metadata.csv
    ├── metadata_coverage.csv
    ├── supported_formats.csv
    ├── table_catalog.csv
    ├── input_inventory.csv
    ├── file_metadata.csv
    ├── dataset_metadata.csv
    ├── provenance_metadata.csv
    ├── variable_metadata.csv
    ├── import_problems.csv
    ├── quality_issues.csv
    ├── errors.csv
    └── variable_dictionary.csv       # compatibility alias of data_dictionary.csv
```

### HTML reports

Each run produces two deliberately different reports:

- `metadata_report.html` is the comprehensive technical report for internal
  review. It retains operational, provenance, quality, privacy-screening, and
  detailed variable metadata.
- `open_science_metadata_report.html` is a concise public-facing metadata
  report appropriate for open-science documentation. It contains a release
  overview and a six-column field dictionary, but excludes private paths,
  hashes, filesystem details, reader diagnostics, run comments, representative
  values, and record-level data.

The open-science report is standalone and uses no external fonts, scripts, or
network resources. Its **Print / Save as PDF** button opens the browser's system
print dialog. On macOS, select **PDF → Save as PDF**; on Windows, choose
**Microsoft Print to PDF** or **Save as PDF**; on Linux, use the browser's
**Print to File/PDF** option. Print styling uses A4 landscape pages, repeats
table headers, and includes every dictionary row even when the on-screen filter
is active.

### Data dictionary contents

The dictionary is the official field-level reference. It includes source labels, names, types, value mappings/levels, missingness, categorical decisions, definitions, documentation status, technical explanations, observed validation rules, source format, privacy flags, and review requirements.

A source variable label may seed an initial business-definition candidate. Its origin is recorded as `source variable label (not independently validated)`. When no definition candidate exists, the field is left blank and a `missing_business_definition` record is created. Automatically generated technical descriptions, observed categories, ranges, and nullability are explicitly labeled as observed or inferred rather than authoritative business rules.

### Metadata contents

The metadata package is broader and includes:

- **structural metadata:** file formats, sheets/objects, rows, columns, storage types, labels, formats, ranges, file size, and available file-system timestamps;
- **descriptive metadata:** dataset labels, variable labels, definitions, tags, and documentation status;
- **administrative metadata:** author, owner, steward, access classification/permissions, license, and source system when supplied;
- **provenance metadata:** source file, reader package/function, hash when enabled, pipeline version, and import time;
- **quality metadata:** missingness, cardinality, parsing issues, duplicates, whitespace, identifiers, sensitive-field hints, and errors.

`categorical_levels`, `value_labels`, and `documentation_gaps` are included in both the dictionary and metadata workbooks/JSON packages. They are metadata ingredients in their own right, while also serving as companion tables that make the data dictionary complete. The standalone CSV files are written once and catalogued as `dictionary + metadata`.

Supply administrative metadata in YAML or with selected command-line options:

```yaml
metadata:
  project_name: Customer analytics extract
  author: Data Engineering
  data_owner: Commercial Analytics
  data_steward: Data Governance
  source_system: CRM
  access_classification: Confidential
  access_permissions: Approved analytics team only
  license: Internal use
  tags: [customer, monthly, governed]
```

## Useful commands

```bash
# One Excel file, selected sheets
Rscript scripts/run_pipeline.R \
  --pipeline data_dictionary \
  --input data/study.xlsx \
  --excel-sheets baseline,follow_up \
  --output outputs/study \
  --overwrite true

# Multiple source families in one run
Rscript scripts/run_pipeline.R \
  --pipeline data_dictionary \
  --input data/source.csv \
  --input data/source.dta \
  --input data/source.sas7bdat \
  --input data/workbook.xlsx \
  --output outputs/combined \
  --overwrite true

# Recursive directory scan with project metadata
Rscript scripts/run_pipeline.R \
  --pipeline data_dictionary \
  --input data \
  --recursive true \
  --project-name "Research data inventory" \
  --data-owner "Research Office" \
  --output outputs/all_data \
  --overwrite true

# Tests
Rscript scripts/bootstrap.R --ci
Rscript scripts/run_tests.R
```

## Adding another pipeline

Copy `pipelines/_template/` to `pipelines/<name>/`, define `run_pipeline_cli(args, pipeline, repo_root)` in its `run.R`, and register it in `config/pipelines.yml`. See [`docs/adding-a-pipeline.md`](docs/adding-a-pipeline.md).

## Validation and release checks

Repository-level static validation, archive integrity checks, and the runtime test strategy are documented in [`VALIDATION.md`](VALIDATION.md). The included GitHub Actions workflow installs dependencies, executes the test suite, and runs the bundled heterogeneous-data example on every push and pull request.

## Privacy and security

Representative examples are masked when a variable looks like an identifier or sensitive field. These detections are heuristic and require human review. Python pickle loading is disabled because unpickling an untrusted file can execute code. See [`docs/security-and-privacy.md`](docs/security-and-privacy.md).
