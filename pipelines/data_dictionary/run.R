pipeline_dir <- file.path(repo_root, "pipelines", "data_dictionary")
module_files <- c("config.R", "utils.R", "ingest.R", "classify.R", "profile.R", "dictionary.R", "report.R", "export.R", "pipeline.R")
for (module in module_files) sys.source(file.path(pipeline_dir, "R", module), envir = environment())

print_data_dictionary_help <- function() {
  cat(paste0(
    "Data dictionary pipeline options:\n",
    "  --input <path>                      File, directory, or glob; may be repeated\n",
    "  --output <directory>                Output directory (default: outputs/data_dictionary)\n",
    "  --config <yml>                      Optional configuration override\n",
    "  --recursive true|false              Recurse through input directories\n",
    "  --include-hidden true|false         Include hidden files during discovery\n",
    "  --excel-sheets all|a,b              Excel sheets to read\n",
    "  --r-objects all|a,b                 Objects to read from .RData/.rda\n",
    "  --max-categorical-levels <number>   Character cardinality limit (default: 10)\n",
    "  --max-categorical-ratio <0..1>      Optional additional character ratio limit\n",
    "  --allow-unsafe-pickle true|false    Enable trusted .pkl/.pickle input\n",
    "  --auto-sas-catalog true|false       Match a sibling .sas7bcat automatically\n",
    "  --sas-catalog <path>                Explicit SAS catalog path (absolute or data-file relative)\n",
    "  --haven-encoding <encoding>         Override Stata/SAS/SPSS text encoding\n",
    "  --text-encoding <encoding>          Encoding for delimited and JSON text (default: UTF-8)\n",
    "  --inline-max-values <number>       Inline dictionary values before previewing\n",
    "  --preview-values <number>          Values shown when an inline domain is previewed\n",
    "  --include-examples true|false       Include representative values\n",
    "  --project-name <text>               Optional descriptive metadata\n",
    "  --data-owner <text>                 Optional administrative metadata\n",
    "  --data-steward <text>               Optional administrative metadata\n",
    "  --overwrite true|false              Allow writing to a nonempty output directory\n"
  ))
}

run_pipeline_cli <- function(args, pipeline, repo_root) {
  if (as_flag(args$help_pipeline, FALSE)) {
    print_data_dictionary_help()
    return(invisible(NULL))
  }

  inputs <- split_cli_values(args$input)
  if (length(inputs) == 0L) {
    print_data_dictionary_help()
    stop("The data_dictionary pipeline requires at least one --input path.", call. = FALSE)
  }

  output_dir <- as.character(args$output %||% file.path(repo_root, "outputs", "data_dictionary"))[[1L]]
  user_config <- if (is.null(args$config)) NULL else as.character(args$config[[1L]])
  overrides <- build_cli_overrides(args)

  result <- run_data_dictionary_pipeline(
    inputs = inputs,
    output_dir = output_dir,
    default_config_path = pipeline$default_config,
    user_config_path = user_config,
    overrides = overrides
  )

  cat("\nGenerated artifacts:\n")
  cat(paste0("  - ", normalizePath(result$artifacts, winslash = "/", mustWork = TRUE), collapse = "\n"), "\n")
  invisible(result)
}
