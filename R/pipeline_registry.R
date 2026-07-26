load_pipeline_registry <- function(repo_root = find_repo_root()) {
  registry_path <- file.path(repo_root, "config", "pipelines.yml")
  registry <- read_yaml_checked(registry_path)
  if (is.null(registry$pipelines) || length(registry$pipelines) == 0L) {
    stop("No pipelines are registered in config/pipelines.yml.", call. = FALSE)
  }
  registry
}

list_registered_pipelines <- function(repo_root = find_repo_root()) {
  registry <- load_pipeline_registry(repo_root)
  entries <- lapply(names(registry$pipelines), function(name) {
    item <- registry$pipelines[[name]]
    data.frame(
      pipeline = name,
      version = as.character(item$version %||% NA_character_),
      description = as.character(item$description %||% ""),
      entrypoint = as.character(item$entrypoint %||% ""),
      default_config = as.character(item$default_config %||% ""),
      stringsAsFactors = FALSE
    )
  })
  bind_rows_fill(entries)
}

resolve_pipeline <- function(name, repo_root = find_repo_root()) {
  registry <- load_pipeline_registry(repo_root)
  item <- registry$pipelines[[name]]
  if (is.null(item)) {
    available <- paste(names(registry$pipelines), collapse = ", ")
    stop(sprintf("Unknown pipeline '%s'. Available pipelines: %s", name, available), call. = FALSE)
  }

  entrypoint <- file.path(repo_root, item$entrypoint)
  default_config <- file.path(repo_root, item$default_config)
  if (!file.exists(entrypoint)) stop(sprintf("Pipeline entrypoint not found: %s", entrypoint), call. = FALSE)
  if (!file.exists(default_config)) stop(sprintf("Pipeline default config not found: %s", default_config), call. = FALSE)

  list(
    name = name,
    version = item$version %||% NA_character_,
    description = item$description %||% "",
    entrypoint = entrypoint,
    default_config = default_config
  )
}
