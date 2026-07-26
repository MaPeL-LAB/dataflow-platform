#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) == 0L) stop("This script must be run with Rscript.", call. = FALSE)
script_path <- sub("^--file=", "", file_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "common.R"))
source(file.path(repo_root, "R", "pipeline_registry.R"))

print_help <- function() {
  cat(paste0(
    "Usage:\n",
    "  Rscript scripts/run_pipeline.R --pipeline <name> [pipeline options]\n\n",
    "Repository options:\n",
    "  --pipeline <name>    Registered pipeline name\n",
    "  --list               List registered pipelines\n",
    "  --help               Show this help\n\n",
    "Data dictionary example:\n",
    "  Rscript scripts/run_pipeline.R \\\n",
    "    --pipeline data_dictionary \\\n",
    "    --input examples/data \\\n",
    "    --output outputs/example \\\n",
    "    --max-categorical-levels 10 \\\n",
    "    --overwrite true\n"
  ))
}

args <- parse_cli_args()
if (as_flag(args$help, FALSE)) {
  print_help()
  quit(status = 0L)
}
if (as_flag(args$list, FALSE)) {
  print(list_registered_pipelines(repo_root), row.names = FALSE, right = FALSE)
  quit(status = 0L)
}

pipeline_name <- as.character(args$pipeline %||% "")[[1L]]
if (!nzchar(pipeline_name)) {
  print_help()
  stop("--pipeline is required.", call. = FALSE)
}

pipeline <- resolve_pipeline(pipeline_name, repo_root)
log_info("Running pipeline '", pipeline$name, "' (version ", pipeline$version, ").")

env <- new.env(parent = globalenv())
env$repo_root <- repo_root
sys.source(pipeline$entrypoint, envir = env)
if (!exists("run_pipeline_cli", envir = env, inherits = FALSE)) {
  stop(sprintf("Entrypoint %s does not define run_pipeline_cli().", pipeline$entrypoint), call. = FALSE)
}

env$run_pipeline_cli(args = args, pipeline = pipeline, repo_root = repo_root)
