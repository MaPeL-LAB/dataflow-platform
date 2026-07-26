`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

is_blank_scalar <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && is.character(x) && !nzchar(trimws(x)))
}

script_file_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0L) return(NULL)
  normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = FALSE)
}

find_repo_root <- function(start = NULL) {
  start <- start %||% script_file_path() %||% getwd()
  if (file.exists(start) && !dir.exists(start)) start <- dirname(start)
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    marker <- file.path(current, "config", "pipelines.yml")
    if (file.exists(marker)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }

  stop("Could not locate repository root. Expected config/pipelines.yml in this directory or a parent directory.", call. = FALSE)
}

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  result <- list()
  positional <- character()
  i <- 1L

  add_value <- function(key, value) {
    key <- gsub("-", "_", key, fixed = TRUE)
    if (is.null(result[[key]])) {
      result[[key]] <<- value
    } else {
      result[[key]] <<- c(result[[key]], value)
    }
  }

  while (i <= length(args)) {
    token <- args[[i]]
    if (startsWith(token, "--")) {
      stripped <- substring(token, 3L)
      if (grepl("=", stripped, fixed = TRUE)) {
        pieces <- strsplit(stripped, "=", fixed = TRUE)[[1L]]
        key <- pieces[[1L]]
        value <- paste(pieces[-1L], collapse = "=")
        add_value(key, value)
      } else {
        key <- stripped
        next_is_value <- i < length(args) && !startsWith(args[[i + 1L]], "--")
        if (next_is_value) {
          add_value(key, args[[i + 1L]])
          i <- i + 1L
        } else {
          add_value(key, TRUE)
        }
      }
    } else {
      positional <- c(positional, token)
    }
    i <- i + 1L
  }

  if (length(positional) > 0L) result$positional <- positional
  result
}

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L) return(default)
  if (is.logical(x)) return(isTRUE(x[[length(x)]]))
  value <- tolower(trimws(as.character(x[[length(x)]])))
  if (value %in% c("true", "t", "1", "yes", "y", "on")) return(TRUE)
  if (value %in% c("false", "f", "0", "no", "n", "off")) return(FALSE)
  stop(sprintf("Cannot interpret '%s' as true/false.", value), call. = FALSE)
}

as_number <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0L) return(default)
  value <- suppressWarnings(as.numeric(x[[length(x)]]))
  if (is.na(value)) stop(sprintf("Cannot interpret '%s' as a number.", x[[length(x)]]), call. = FALSE)
  value
}

split_cli_values <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character())
  values <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
  values <- trimws(values)
  values[nzchar(values)]
}

deep_merge <- function(base, override) {
  if (is.null(override)) return(base)
  if (!is.list(base) || !is.list(override)) return(override)

  out <- base
  for (name in names(override)) {
    if (!is.null(out[[name]]) && is.list(out[[name]]) && is.list(override[[name]])) {
      out[[name]] <- deep_merge(out[[name]], override[[name]])
    } else {
      out[[name]] <- override[[name]]
    }
  }
  out
}

require_package <- function(package, purpose = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    suffix <- if (is.null(purpose)) "" else paste0(" for ", purpose)
    stop(
      sprintf("Package '%s' is required%s. Run: Rscript scripts/bootstrap.R", package, suffix),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

read_yaml_checked <- function(path) {
  require_package("yaml", "configuration files")
  if (!file.exists(path)) stop(sprintf("Configuration file not found: %s", path), call. = FALSE)
  yaml::read_yaml(path)
}

safe_dir_create <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop(sprintf("Could not create directory: %s", path), call. = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

utc_now <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ")
}

log_message <- function(level, ..., .time = TRUE) {
  prefix <- if (.time) sprintf("[%s] [%s]", utc_now(), toupper(level)) else sprintf("[%s]", toupper(level))
  cat(prefix, paste0(..., collapse = ""), "\n")
}

log_info <- function(...) log_message("info", ...)
log_warn <- function(...) log_message("warn", ...)
log_error <- function(...) log_message("error", ...)

slugify <- function(x, fallback = "item") {
  x <- enc2utf8(as.character(x %||% ""))
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "_")
  x[is.na(x)] <- ""
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, fallback)
}

truncate_text <- function(x, width = 120L) {
  x <- as.character(x)
  too_long <- nchar(x, type = "width", allowNA = TRUE) > width
  x[!is.na(too_long) & too_long] <- paste0(substr(x[!is.na(too_long) & too_long], 1L, max(1L, width - 1L)), "…")
  x
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

bind_rows_fill <- function(frames) {
  frames <- Filter(function(x) !is.null(x) && is.data.frame(x), frames)
  if (length(frames) == 0L) return(data.frame(stringsAsFactors = FALSE))

  all_names <- unique(unlist(lapply(frames, names), use.names = FALSE))
  aligned <- lapply(frames, function(frame) {
    missing <- setdiff(all_names, names(frame))
    for (name in missing) frame[[name]] <- rep(NA, nrow(frame))
    frame[all_names]
  })
  rownames(aligned[[1L]]) <- NULL
  out <- do.call(rbind, aligned)
  rownames(out) <- NULL
  out
}

json_string <- function(x) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(paste(capture.output(dput(x)), collapse = " "))
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}

atomicize_data_frame <- function(frame) {
  if (!is.data.frame(frame)) return(frame)
  out <- frame
  for (name in names(out)) {
    column <- out[[name]]
    if (inherits(column, "POSIXt")) {
      out[[name]] <- format(column, tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ")
    } else if (inherits(column, "Date")) {
      out[[name]] <- format(column, "%Y-%m-%d")
    } else if (is.data.frame(column)) {
      out[[name]] <- vapply(seq_len(nrow(column)), function(i) {
        json_string(as.list(column[i, , drop = FALSE]))
      }, character(1L))
    } else if (is.matrix(column) || is.array(column)) {
      if (length(dim(column)) == 2L && nrow(column) == nrow(out)) {
        out[[name]] <- vapply(seq_len(nrow(column)), function(i) json_string(column[i, ]), character(1L))
      } else {
        out[[name]] <- rep(json_string(column), nrow(out))
      }
    } else if (is.list(column)) {
      out[[name]] <- vapply(column, json_string, character(1L))
    }
  }
  out
}

safe_sheet_name <- function(x, existing = character()) {
  x <- gsub("[\\/:?*\\[\\]]", "_", as.character(x))
  x <- substr(x, 1L, 31L)
  if (!nzchar(x)) x <- "Sheet"
  candidate <- x
  counter <- 2L
  while (candidate %in% existing) {
    suffix <- paste0("_", counter)
    candidate <- paste0(substr(x, 1L, 31L - nchar(suffix)), suffix)
    counter <- counter + 1L
  }
  candidate
}

package_version_safe <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) return("-")
  as.character(utils::packageVersion(package))
}

format_bytes <- function(bytes) {
  if (is.na(bytes)) return(NA_character_)
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  index <- 1L
  while (value >= 1024 && index < length(units)) {
    value <- value / 1024
    index <- index + 1L
  }
  sprintf("%.2f %s", value, units[[index]])
}
