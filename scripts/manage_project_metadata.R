#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

stop_with_usage <- function(message = NULL) {
  if (!is.null(message)) message(message)
  stop(
    paste(
      "Usage:",
      "  Rscript --vanilla scripts/manage_project_metadata.R read <profile.yml>",
      "  Rscript --vanilla scripts/manage_project_metadata.R read-json <profile.yml>",
      paste(
        "  Rscript --vanilla scripts/manage_project_metadata.R write <profile.yml>",
        "<project> <description> <author_or_citation> <license>",
        "<access> <access_conditions> <comma_separated_tags>"
      ),
      sep = "\n"
    ),
    call. = FALSE
  )
}

if (length(args) < 2L) stop_with_usage()
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required to manage project metadata.", call. = FALSE)
}

action <- args[[1L]]
profile_path <- path.expand(args[[2L]])

clean_scalar <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) return("")
  value <- gsub("[\r\n\t]+", " ", as.character(value[[1L]]))
  trimws(value)
}

read_profile <- function(path) {
  if (!file.exists(path)) return(list())
  profile <- yaml::read_yaml(path)
  if (is.null(profile)) list() else profile
}

if (action %in% c("read", "read-json")) {
  profile <- read_profile(profile_path)
  metadata <- profile$metadata
  if (is.null(metadata) || !is.list(metadata)) metadata <- list()
  tags <- metadata$tags
  if (is.null(tags)) tags <- character()
  if (is.list(tags)) tags <- unlist(tags, use.names = FALSE)

  values <- c(
    clean_scalar(metadata$project_description),
    clean_scalar(metadata$author),
    clean_scalar(metadata$license),
    clean_scalar(metadata$access_classification),
    clean_scalar(metadata$access_permissions),
    paste(vapply(tags, clean_scalar, character(1L)), collapse = ", ")
  )
  if (identical(action, "read-json")) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' is required to read project metadata as JSON.", call. = FALSE)
    }
    names(values) <- c(
      "project_description", "author", "license",
      "access_classification", "access_permissions", "tags"
    )
    cat(jsonlite::toJSON(as.list(values), auto_unbox = TRUE, null = "null"), "\n", sep = "")
  } else {
    writeLines(values, useBytes = TRUE)
  }
  quit(save = "no", status = 0L)
}

if (!identical(action, "write") || length(args) != 9L) stop_with_usage()

profile <- read_profile(profile_path)
if (!is.list(profile)) profile <- list()
metadata <- profile$metadata
if (is.null(metadata) || !is.list(metadata)) metadata <- list()

tag_values <- trimws(strsplit(args[[9L]], ",", fixed = TRUE)[[1L]])
tag_values <- unique(tag_values[nzchar(tag_values) & toupper(tag_values) != "NA"])

metadata$project_name <- clean_scalar(args[[3L]])
metadata$project_description <- clean_scalar(args[[4L]])
metadata$author <- clean_scalar(args[[5L]])
metadata$license <- clean_scalar(args[[6L]])
metadata$access_classification <- clean_scalar(args[[7L]])
metadata$access_permissions <- clean_scalar(args[[8L]])
metadata$tags <- tag_values
profile$metadata <- metadata

profile_dir <- dirname(profile_path)
if (!dir.exists(profile_dir) && !dir.create(profile_dir, recursive = TRUE, showWarnings = FALSE)) {
  stop("Could not create project metadata directory: ", profile_dir, call. = FALSE)
}

temporary_path <- tempfile(pattern = ".project-metadata-", tmpdir = profile_dir, fileext = ".yml")
on.exit(if (file.exists(temporary_path)) unlink(temporary_path), add = TRUE)
yaml::write_yaml(profile, temporary_path)

if (file.exists(profile_path)) {
  previous_path <- if (grepl("\\.ya?ml$", profile_path, ignore.case = TRUE)) {
    sub("\\.ya?ml$", ".previous.yml", profile_path, ignore.case = TRUE)
  } else {
    paste0(profile_path, ".previous")
  }
  if (!file.copy(profile_path, previous_path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)) {
    stop("Could not preserve the previous project metadata profile: ", previous_path, call. = FALSE)
  }
}

if (!file.copy(temporary_path, profile_path, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)) {
  stop("Could not safely write project metadata profile: ", profile_path, call. = FALSE)
}
unlink(temporary_path)

cat(normalizePath(profile_path, winslash = "/", mustWork = TRUE), "\n", sep = "")
