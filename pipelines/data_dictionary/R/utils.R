supported_source_formats <- function() {
  c(
    csv = "Delimited text (CSV)",
    tsv = "Delimited text (TSV/TAB)",
    txt = "Delimited text (configured delimiter)",
    xls = "Microsoft Excel 97-2003",
    xlsx = "Microsoft Excel",
    dta = "Stata",
    sas7bdat = "SAS data set",
    sas7bcat = "SAS catalog (auxiliary)",
    xpt = "SAS transport",
    sav = "SPSS system file",
    zsav = "Compressed SPSS system file",
    por = "SPSS portable file",
    rds = "R serialized object",
    rdata = "R workspace",
    json = "JSON",
    jsonl = "Newline-delimited JSON",
    parquet = "Apache Parquet",
    feather = "Apache Arrow/Feather",
    arrow = "Apache Arrow IPC file",
    ipc = "Apache Arrow IPC file or stream",
    pickle = "Python pickle (explicit opt-in required)"
  )
}

supported_formats_table <- function() {
  data.frame(
    family = c(
      "Delimited text", "Delimited text", "Delimited text", "Excel", "Excel",
      "Stata", "SAS", "SAS", "SAS", "SPSS", "SPSS", "SPSS",
      "R", "R", "JSON", "JSON", "Arrow", "Arrow", "Arrow", "Arrow", "Python"
    ),
    format = c(
      "csv", "tsv", "txt", "xls", "xlsx", "dta", "sas7bdat", "sas7bcat", "xpt",
      "sav", "zsav", "por", "rds", "rdata", "json", "jsonl", "parquet", "feather",
      "arrow", "ipc", "pickle"
    ),
    extensions = c(
      ".csv", ".tsv; .tab", ".txt", ".xls", ".xlsx", ".dta", ".sas7bdat",
      ".sas7bcat (auxiliary)", ".xpt", ".sav", ".zsav", ".por", ".rds",
      ".rda; .RData", ".json", ".jsonl; .ndjson", ".parquet", ".feather",
      ".arrow", ".ipc", ".pkl; .pickle"
    ),
    reader_package = c(
      "readr", "readr", "readr", "readxl", "readxl", "haven", "haven", "haven",
      "haven", "haven", "haven", "haven", "base R", "base R", "jsonlite", "jsonlite",
      "arrow", "arrow", "arrow", "arrow", "reticulate"
    ),
    reader_function = c(
      "read_delim", "read_delim", "read_delim", "read_excel", "read_excel", "read_dta",
      "read_sas", "read_sas catalog_file", "read_xpt", "read_sav", "read_sav", "read_por",
      "readRDS", "load", "fromJSON", "stream_in", "read_parquet", "read_feather",
      "read_ipc_file", "read_ipc_file/read_ipc_stream", "py_load_object"
    ),
    availability = c(
      rep("core", 16L), rep("optional", 4L), "optional; disabled unless explicitly enabled"
    ),
    notes = c(
      "Compression suffixes .gz/.bz2/.xz are recognized.",
      "Compression suffixes .gz/.bz2/.xz are recognized.",
      "Delimiter is configurable; compression suffixes .gz/.bz2/.xz are recognized.",
      "All selected worksheets are expanded into datasets.",
      "All selected worksheets are expanded into datasets.",
      "Variable labels, value labels, formats, dates, and tagged missing values are retained where available.",
      "A matching .sas7bcat can be selected automatically or supplied in configuration.",
      "Catalogs are auxiliary and are not treated as standalone datasets.",
      "SAS transport files are imported through haven.",
      "SPSS user-defined missing values are retained.",
      "SPSS user-defined missing values are retained.",
      "SPSS portable format.",
      "A tabular object or a list of tabular objects can be expanded.",
      "Selected tabular objects are expanded; non-tabular objects are skipped with a warning.",
      "Record arrays, data-frame-like objects, and named tabular members are normalized.",
      "Newline-delimited records are streamed.",
      "Requires the optional arrow package.",
      "Requires the optional arrow package.",
      "Requires the optional arrow package.",
      "File format is tried first, then stream format.",
      "Only trusted files should be loaded because Python pickle deserialization can execute code."
    ),
    stringsAsFactors = FALSE
  )
}

detect_source_format <- function(path) {
  lower <- tolower(basename(path))
  compressed <- grepl("\\.(gz|bz2|xz)$", lower)
  base <- sub("\\.(gz|bz2|xz)$", "", lower)

  format <- if (grepl("\\.csv$", base)) {
    "csv"
  } else if (grepl("\\.(tsv|tab)$", base)) {
    "tsv"
  } else if (grepl("\\.txt$", base)) {
    "txt"
  } else if (grepl("\\.xls$", base)) {
    "xls"
  } else if (grepl("\\.xlsx$", base)) {
    "xlsx"
  } else if (grepl("\\.dta$", base)) {
    "dta"
  } else if (grepl("\\.sas7bdat$", base)) {
    "sas7bdat"
  } else if (grepl("\\.xpt$", base)) {
    "xpt"
  } else if (grepl("\\.sav$", base)) {
    "sav"
  } else if (grepl("\\.zsav$", base)) {
    "zsav"
  } else if (grepl("\\.por$", base)) {
    "por"
  } else if (grepl("\\.rds$", base)) {
    "rds"
  } else if (grepl("\\.(rda|rdata)$", base)) {
    "rdata"
  } else if (grepl("\\.json$", base)) {
    "json"
  } else if (grepl("\\.(jsonl|ndjson)$", base)) {
    "jsonl"
  } else if (grepl("\\.parquet$", base)) {
    "parquet"
  } else if (grepl("\\.feather$", base)) {
    "feather"
  } else if (grepl("\\.arrow$", base)) {
    "arrow"
  } else if (grepl("\\.ipc$", base)) {
    "ipc"
  } else if (grepl("\\.(pkl|pickle)$", base)) {
    "pickle"
  } else {
    NA_character_
  }

  compressible_formats <- c("csv", "tsv", "txt", "json", "jsonl", "dta", "sas7bdat", "xpt", "sav", "zsav", "por")
  if (compressed && !(format %in% compressible_formats)) return(NA_character_)
  format
}

detect_auxiliary_source_format <- function(path) {
  lower <- tolower(basename(path))
  lower <- sub("\\.(gz|bz2|xz)$", "", lower)
  if (grepl("\\.sas7bcat$", lower)) return("sas7bcat")
  NA_character_
}

source_compression <- function(path) {
  match <- regmatches(tolower(basename(path)), regexpr("\\.(gz|bz2|xz)$", tolower(basename(path))))
  if (length(match) == 0L || identical(match, "")) NA_character_ else sub("^\\.", "", match)
}

source_stem <- function(path) {
  name <- basename(path)
  name <- sub("\\.(gz|bz2|xz)$", "", name, ignore.case = TRUE)
  tools::file_path_sans_ext(name)
}

compound_extension <- function(path) {
  name <- tolower(basename(path))
  compression <- source_compression(path)
  if (!is.na(compression)) {
    base <- sub(paste0("\\.", compression, "$"), "", name)
    extension <- tools::file_ext(base)
    if (nzchar(extension)) return(paste0(".", extension, ".", compression))
  }
  extension <- tools::file_ext(name)
  if (nzchar(extension)) paste0(".", extension) else NA_character_
}

is_supported_source <- function(path) {
  !is.na(detect_source_format(path))
}

discover_input_files <- function(inputs, recursive = FALSE, include_hidden = FALSE) {
  if (length(inputs) == 0L) stop("At least one --input path is required.", call. = FALSE)

  expanded <- character()
  for (input in inputs) {
    matches <- Sys.glob(path.expand(input))
    if (length(matches) == 0L && file.exists(path.expand(input))) matches <- path.expand(input)
    if (length(matches) == 0L) {
      log_warn("Input path did not match any file or directory: ", input)
      next
    }

    for (path in matches) {
      if (dir.exists(path)) {
        files <- list.files(
          path,
          recursive = recursive,
          full.names = TRUE,
          all.files = include_hidden,
          no.. = TRUE
        )
        files <- files[file.info(files)$isdir %in% FALSE]
        expanded <- c(expanded, files)
      } else {
        expanded <- c(expanded, path)
      }
    }
  }

  expanded <- unique(normalizePath(expanded, winslash = "/", mustWork = TRUE))
  expanded[order(expanded)]
}

format_file_time <- function(value) {
  if (length(value) == 0L || is.na(value)) return(NA_character_)
  format(value, tz = "UTC", usetz = TRUE, format = "%Y-%m-%dT%H:%M:%SZ")
}

file_info_value <- function(info, name, default = NA) {
  if (!is.data.frame(info) || !(name %in% names(info)) || nrow(info) == 0L) return(default)
  value <- info[[name]][[1L]]
  if (is.null(value) || length(value) == 0L) default else value
}

source_file_metadata <- function(path, compute_md5 = FALSE, source_format = NULL) {
  info <- file.info(path)
  source_format <- source_format %||% detect_source_format(path)
  if (is.na(source_format)) source_format <- detect_auxiliary_source_format(path)
  descriptions <- supported_source_formats()
  format_description <- if (!is.na(source_format) && source_format %in% names(descriptions)) {
    unname(descriptions[[source_format]])
  } else {
    NA_character_
  }

  mode <- tryCatch(as.character(file_info_value(info, "mode", NA)), error = function(e) NA_character_)
  birthtime <- file_info_value(info, "birthtime", NA)
  data.frame(
    source_file = normalizePath(path, winslash = "/", mustWork = TRUE),
    source_basename = basename(path),
    source_extension = compound_extension(path),
    source_format = source_format,
    source_format_description = format_description,
    compression = source_compression(path),
    file_size_bytes = as.numeric(file_info_value(info, "size", NA_real_)),
    file_size_human = format_bytes(as.numeric(file_info_value(info, "size", NA_real_))),
    file_created_utc = format_file_time(birthtime),
    file_modified_utc = format_file_time(file_info_value(info, "mtime", NA)),
    file_accessed_utc = format_file_time(file_info_value(info, "atime", NA)),
    file_status_changed_utc = format_file_time(file_info_value(info, "ctime", NA)),
    file_owner = as.character(file_info_value(info, "uname", NA_character_)),
    file_group = as.character(file_info_value(info, "grname", NA_character_)),
    file_permissions = mode,
    file_md5 = if (isTRUE(compute_md5)) unname(tools::md5sum(path)) else NA_character_,
    stringsAsFactors = FALSE
  )
}

open_text_connection <- function(path, encoding = "UTF-8") {
  lower <- tolower(path)
  if (grepl("\\.gz$", lower)) return(gzfile(path, open = "rt", encoding = encoding))
  if (grepl("\\.bz2$", lower)) return(bzfile(path, open = "rt", encoding = encoding))
  if (grepl("\\.xz$", lower)) return(xzfile(path, open = "rt", encoding = encoding))
  file(path, open = "rt", encoding = encoding)
}

limit_rows <- function(data, n_max = NULL) {
  if (is.null(n_max) || is.na(n_max) || is.infinite(n_max)) return(data)
  n_max <- max(0L, as.integer(n_max))
  data[seq_len(min(nrow(data), n_max)), , drop = FALSE]
}

is_tabular_object <- function(x) {
  is.data.frame(x) || is.matrix(x) || inherits(x, "table")
}

coerce_tabular <- function(x) {
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  if (is.matrix(x) || inherits(x, "table")) {
    return(as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE))
  }
  stop("Object is not tabular.", call. = FALSE)
}

normalize_tabular_object <- function(object, base_name = "dataset") {
  if (is_tabular_object(object)) {
    return(stats::setNames(list(coerce_tabular(object)), base_name))
  }

  if (is.list(object)) {
    object_names <- names(object)

    tabular_indices <- which(vapply(object, is_tabular_object, logical(1L)))
    if (length(tabular_indices) > 0L) {
      out <- lapply(object[tabular_indices], coerce_tabular)
      names(out) <- if (!is.null(object_names)) object_names[tabular_indices] else paste0(base_name, "_", tabular_indices)
      blank_names <- is.na(names(out)) | !nzchar(names(out))
      names(out)[blank_names] <- paste0(base_name, "_", tabular_indices[blank_names])
      return(out)
    }

    atomic_columns <- length(object) > 0L && all(vapply(
      object,
      function(x) is.atomic(x) || inherits(x, c("Date", "POSIXt", "factor")),
      logical(1L)
    ))
    lengths_equal <- atomic_columns && length(unique(vapply(object, length, integer(1L)))) == 1L
    if (lengths_equal) {
      candidate <- tryCatch(
        as.data.frame(object, stringsAsFactors = FALSE, check.names = FALSE),
        error = function(e) NULL
      )
      if (!is.null(candidate)) return(stats::setNames(list(candidate), base_name))
    }
  }

  candidate <- tryCatch(
    as.data.frame(object, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (!is.null(candidate) && ncol(candidate) > 0L) {
    return(stats::setNames(list(candidate), base_name))
  }

  stop(sprintf("Object '%s' could not be converted to a tabular data frame.", base_name), call. = FALSE)
}

select_named_items <- function(items, selector, item_type) {
  if (identical(selector, "all") || (length(selector) == 1L && tolower(selector) == "all")) return(items)
  missing <- setdiff(selector, items)
  if (length(missing) > 0L) {
    stop(sprintf("Requested %s not found: %s", item_type, paste(missing, collapse = ", ")), call. = FALSE)
  }
  selector
}

make_dataset_record <- function(data, path, source_format, dataset_name,
                                source_sheet = NA_character_, source_object = NA_character_,
                                import_notes = NA_character_, import_problems = NULL,
                                reader_package = NA_character_, reader_function = NA_character_) {
  list(
    data = as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE),
    dataset_name = as.character(dataset_name),
    source_file = normalizePath(path, winslash = "/", mustWork = TRUE),
    source_format = as.character(source_format),
    source_sheet = as.character(source_sheet),
    source_object = as.character(source_object),
    import_notes = as.character(import_notes),
    import_problems = import_problems,
    reader_package = as.character(reader_package),
    reader_function = as.character(reader_function),
    dataset_label = as.character(attr(data, "label", exact = TRUE) %||% NA_character_)
  )
}
