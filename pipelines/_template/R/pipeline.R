run_template_pipeline <- function(output_dir, config) {
  output_dir <- safe_dir_create(output_dir)
  artifact <- file.path(output_dir, "replace_me.txt")
  writeLines("Replace this template with pipeline output.", artifact)
  invisible(artifact)
}
