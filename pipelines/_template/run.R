pipeline_dir <- file.path(repo_root, "pipelines", "_template")
sys.source(file.path(pipeline_dir, "R", "pipeline.R"), envir = environment())

run_pipeline_cli <- function(args, pipeline, repo_root) {
  output_dir <- as.character(args$output %||% file.path(repo_root, "outputs", "replace_me"))[[1L]]
  config <- read_yaml_checked(pipeline$default_config)
  artifact <- run_template_pipeline(output_dir, config)
  cat("Generated: ", normalizePath(artifact, winslash = "/", mustWork = TRUE), "\n", sep = "")
  invisible(artifact)
}
