read_delimited_source <- function(path, format, config) {
  require_package("readr", "delimited text input")
  delimiter <- switch(
    format,
    csv = ",",
    tsv = "\t",
    txt = as.character(config$input$text_delimiter %||% "\t")
  )
  n_max <- config$input$n_max %||% Inf
  encoding <- as.character(config$input$text_encoding %||% "UTF-8")
  data <- suppressMessages(readr::read_delim(
    file = path,
    delim = delimiter,
    n_max = n_max,
    locale = readr::locale(encoding = encoding),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal",
    trim_ws = FALSE
  ))
  problems <- tryCatch(readr::problems(data), error = function(e) data.frame(stringsAsFactors = FALSE))
  list(make_dataset_record(
    data = data,
    path = path,
    source_format = format,
    dataset_name = source_stem(path),
    import_problems = problems,
    reader_package = "readr",
    reader_function = "read_delim"
  ))
}

read_excel_source <- function(path, format, config) {
  require_package("readxl", "Excel input")
  sheets <- readxl::excel_sheets(path)
  selector <- selector_values(config$input$excel_sheets)
  selected <- select_named_items(sheets, selector, "Excel sheet(s)")
  n_max <- config$input$n_max %||% Inf

  lapply(selected, function(sheet) {
    data <- readxl::read_excel(
      path,
      sheet = sheet,
      n_max = n_max,
      trim_ws = FALSE,
      .name_repair = "minimal"
    )
    make_dataset_record(
      data = data,
      path = path,
      source_format = format,
      dataset_name = paste0(source_stem(path), "__", sheet),
      source_sheet = sheet,
      import_notes = "Cell values were imported. Workbook presentation, formulas, comments, and validation rules are not treated as tabular data.",
      reader_package = "readxl",
      reader_function = "read_excel"
    )
  })
}

resolve_sas_catalog <- function(path, config) {
  catalog <- config$input$sas_catalog %||% NULL
  if (!is.null(catalog) && nzchar(as.character(catalog))) {
    configured <- path.expand(as.character(catalog))
    is_windows_drive <- nchar(configured) >= 3L &&
      substr(configured, 2L, 2L) == ":" &&
      substr(configured, 3L, 3L) %in% c("/", "\\")
    is_unc_path <- startsWith(configured, "\\\\")
    is_absolute <- startsWith(configured, "/") || is_windows_drive || is_unc_path
    if (!file.exists(configured) && !is_absolute) {
      configured <- file.path(dirname(path), configured)
    }
    return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  }
  if (!isTRUE(config$input$auto_sas_catalog)) return(NULL)

  compression <- if (grepl("\\.(gz|bz2|xz)$", path, ignore.case = TRUE)) {
    sub("^.*(\\.(gz|bz2|xz))$", "\\1", path, ignore.case = TRUE)
  } else {
    ""
  }
  data_base <- sub("\\.(gz|bz2|xz)$", "", path, ignore.case = TRUE)
  catalog_base <- sub("\\.sas7bdat$", ".sas7bcat", data_base, ignore.case = TRUE)
  candidates <- unique(c(
    catalog_base,
    if (nzchar(compression)) paste0(catalog_base, compression) else character(),
    paste0(catalog_base, ".gz"),
    paste0(catalog_base, ".bz2"),
    paste0(catalog_base, ".xz")
  ))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) > 0L) return(existing[[1L]])

  # Case-insensitive fallback for filesystems or exports that use upper-case
  # SAS extensions. Match on the data-set stem and prefer an uncompressed catalog.
  directory_files <- list.files(dirname(path), full.names = TRUE, all.files = FALSE, no.. = TRUE)
  catalogs <- directory_files[vapply(
    directory_files,
    function(candidate) identical(detect_auxiliary_source_format(candidate), "sas7bcat"),
    logical(1L)
  )]
  same_stem <- catalogs[tolower(source_stem(catalogs)) == tolower(source_stem(path))]
  if (length(same_stem) == 0L) return(NULL)
  compression_rank <- ifelse(is.na(vapply(same_stem, source_compression, character(1L))), 0L, 1L)
  same_stem[order(compression_rank, tolower(same_stem))][[1L]]
}

read_stata_unsigned <- function(connection, size, endian) {
  value <- readBin(
    connection,
    what = integer(),
    n = 1L,
    size = size,
    signed = TRUE,
    endian = endian
  )
  if (length(value) != 1L) stop("Unexpected end of Stata file.", call. = FALSE)
  if (value < 0) value <- value + 2^(8L * size)
  as.numeric(value)
}

decode_stata_fixed_strings <- function(bytes, width, count, encoding = "latin1") {
  if (count == 0L) return(character())
  if (length(bytes) != width * count) {
    stop("Stata string metadata block has an unexpected length.", call. = FALSE)
  }

  blocks <- matrix(bytes, nrow = width, ncol = count)
  vapply(seq_len(count), function(index) {
    value <- blocks[, index]
    zero <- which(value == as.raw(0L))
    if (length(zero) > 0L) value <- value[seq_len(zero[[1L]] - 1L)]
    if (length(value) == 0L) return("")
    text <- rawToChar(value)
    converted <- suppressWarnings(iconv(text, from = encoding, to = "UTF-8", sub = "byte"))
    if (is.na(converted)) text else converted
  }, character(1L))
}

inspect_stata_legacy_layout <- function(path, encoding = "latin1") {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)

  dataset_format <- read_stata_unsigned(connection, 1L, "little")
  byte_order <- read_stata_unsigned(connection, 1L, "little")
  file_type <- read_stata_unsigned(connection, 1L, "little")
  read_stata_unsigned(connection, 1L, "little")

  if (!(dataset_format %in% c(114, 115))) {
    stop(
      "Base-R recovery supports legacy Stata formats 114 and 115 only; detected format ",
      dataset_format, ".",
      call. = FALSE
    )
  }
  if (!(byte_order %in% c(1, 2))) stop("Unrecognized Stata byte order.", call. = FALSE)
  if (file_type != 1) stop("Unsupported Stata file type.", call. = FALSE)
  endian <- if (byte_order == 1) "big" else "little"

  variable_count <- as.integer(read_stata_unsigned(connection, 2L, endian))
  observation_count <- read_stata_unsigned(connection, 4L, endian)
  if (variable_count < 1L || observation_count < 0) {
    stop("Invalid Stata dataset dimensions.", call. = FALSE)
  }

  dataset_label <- decode_stata_fixed_strings(
    readBin(connection, raw(), n = 81L),
    81L,
    1L,
    encoding
  )[[1L]]
  timestamp <- decode_stata_fixed_strings(
    readBin(connection, raw(), n = 18L),
    18L,
    1L,
    encoding
  )[[1L]]
  storage_types <- as.integer(readBin(connection, raw(), n = variable_count))
  valid_types <- storage_types %in% c(seq_len(244L), 251:255)
  if (length(storage_types) != variable_count || !all(valid_types)) {
    stop("Unsupported or incomplete Stata storage-type list.", call. = FALSE)
  }

  variable_names <- decode_stata_fixed_strings(
    readBin(connection, raw(), n = 33L * variable_count),
    33L,
    variable_count,
    encoding
  )
  readBin(connection, raw(), n = 2L * (variable_count + 1L))
  display_formats <- decode_stata_fixed_strings(
    readBin(connection, raw(), n = 49L * variable_count),
    49L,
    variable_count,
    encoding
  )
  value_label_names <- decode_stata_fixed_strings(
    readBin(connection, raw(), n = 33L * variable_count),
    33L,
    variable_count,
    encoding
  )
  variable_labels <- decode_stata_fixed_strings(
    readBin(connection, raw(), n = 81L * variable_count),
    81L,
    variable_count,
    encoding
  )

  repeat {
    expansion_type <- read_stata_unsigned(connection, 1L, endian)
    expansion_length <- read_stata_unsigned(connection, 4L, endian)
    if (expansion_type == 0 && expansion_length == 0) break
    remaining <- as.numeric(file.info(path)$size) - seek(connection, rw = "read")
    if (expansion_length > remaining) {
      stop("Invalid Stata expansion-field length.", call. = FALSE)
    }
    seek(connection, where = expansion_length, origin = "current", rw = "read")
  }

  data_start <- seek(connection, rw = "read")
  storage_widths <- ifelse(
    storage_types <= 244L,
    storage_types,
    c(`251` = 1L, `252` = 2L, `253` = 4L, `254` = 4L, `255` = 8L)[as.character(storage_types)]
  )
  storage_widths <- as.integer(storage_widths)
  record_width <- sum(storage_widths)
  data_end <- data_start + record_width * observation_count
  file_size <- as.numeric(file.info(path)$size)
  if (!is.finite(data_end) || data_end > file_size) {
    stop("Stata data block extends beyond the end of the file.", call. = FALSE)
  }

  value_label_length <- NA_real_
  if (file_size >= data_end + 4) {
    seek(connection, where = data_end, origin = "start", rw = "read")
    value_label_length <- read_stata_unsigned(connection, 4L, endian)
  }
  unexpected_trailing_payload <- isTRUE(
    !is.na(value_label_length) &&
      value_label_length == 0 &&
      file_size > data_end + 4
  )

  list(
    dataset_format = dataset_format,
    endian = endian,
    variable_count = variable_count,
    observation_count = observation_count,
    dataset_label = dataset_label,
    timestamp = timestamp,
    storage_types = storage_types,
    storage_widths = storage_widths,
    variable_names = variable_names,
    display_formats = display_formats,
    value_label_names = value_label_names,
    variable_labels = variable_labels,
    data_start = data_start,
    data_end = data_end,
    record_width = record_width,
    file_size = file_size,
    unexpected_trailing_payload = unexpected_trailing_payload,
    trailing_bytes = max(0, file_size - data_end)
  )
}

read_stata_numeric_column <- function(bytes, storage_type, count, endian) {
  if (count == 0L) return(numeric())
  connection <- rawConnection(bytes, open = "rb")
  on.exit(close(connection), add = TRUE)

  if (storage_type == 251L) {
    value <- readBin(connection, integer(), n = count, size = 1L, signed = TRUE, endian = endian)
    value[value > 100L] <- NA_integer_
    return(value)
  }
  if (storage_type == 252L) {
    value <- readBin(connection, integer(), n = count, size = 2L, signed = TRUE, endian = endian)
    value[value > 32740L] <- NA_integer_
    return(value)
  }
  if (storage_type == 253L) {
    value <- readBin(connection, integer(), n = count, size = 4L, signed = TRUE, endian = endian)
    value[as.numeric(value) > 2147483620] <- NA_integer_
    return(value)
  }
  if (storage_type == 254L) {
    value <- readBin(connection, double(), n = count, size = 4L, endian = endian)
    value[!is.finite(value) | value >= 1.701e38] <- NA_real_
    return(value)
  }
  if (storage_type == 255L) {
    value <- readBin(connection, double(), n = count, size = 8L, endian = endian)
    value[!is.finite(value) | value >= 8.988e307] <- NA_real_
    return(value)
  }
  stop("Unsupported numeric Stata storage type: ", storage_type, call. = FALSE)
}

read_stata_legacy_recovery <- function(path, config, layout = NULL) {
  encoding <- as.character(config$input$haven_encoding %||% "latin1")[[1L]]
  layout <- layout %||% inspect_stata_legacy_layout(path, encoding)
  n_max <- config$input$n_max %||% Inf
  rows_to_read <- if (is.null(n_max) || is.na(n_max) || is.infinite(n_max)) {
    layout$observation_count
  } else {
    min(layout$observation_count, max(0, as.integer(n_max)))
  }
  rows_to_read <- as.integer(rows_to_read)

  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  seek(connection, where = layout$data_start, origin = "start", rw = "read")
  data_size <- layout$record_width * rows_to_read
  data_bytes <- readBin(connection, raw(), n = data_size)
  if (length(data_bytes) != data_size) {
    stop("Could not read the complete valid Stata data block.", call. = FALSE)
  }

  offsets <- c(0L, head(cumsum(layout$storage_widths), -1L))
  columns <- vector("list", layout$variable_count)
  for (index in seq_len(layout$variable_count)) {
    width <- layout$storage_widths[[index]]
    starts <- 1 + offsets[[index]] + (seq_len(rows_to_read) - 1L) * layout$record_width
    byte_indices <- as.vector(t(outer(starts, seq.int(0L, width - 1L), `+`)))
    column_bytes <- data_bytes[byte_indices]
    storage_type <- layout$storage_types[[index]]

    if (storage_type <= 244L) {
      value <- decode_stata_fixed_strings(column_bytes, width, rows_to_read, encoding)
    } else {
      value <- read_stata_numeric_column(column_bytes, storage_type, rows_to_read, layout$endian)
    }

    if (nzchar(layout$variable_labels[[index]])) {
      attr(value, "label") <- layout$variable_labels[[index]]
    }
    if (nzchar(layout$display_formats[[index]])) {
      attr(value, "format.stata") <- layout$display_formats[[index]]
    }
    if (nzchar(layout$value_label_names[[index]])) {
      attr(value, "label.table") <- layout$value_label_names[[index]]
    }
    columns[[index]] <- value
  }

  variable_names <- layout$variable_names
  blank_names <- !nzchar(variable_names)
  variable_names[blank_names] <- paste0("variable_", which(blank_names))
  names(columns) <- variable_names
  data <- as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  if (nzchar(layout$dataset_label)) attr(data, "label") <- layout$dataset_label

  note <- paste0(
    "Recovered the valid legacy Stata ", layout$dataset_format,
    " data block with the base-R safety reader after detecting an unsafe or crashed native import. ",
    "Raw coded values, variable labels, and display formats were retained. Value-label mappings ",
    "from the malformed trailing region were not imported and require source-file review."
  )
  list(make_dataset_record(
    data = data,
    path = path,
    source_format = "dta",
    dataset_name = source_stem(path),
    import_notes = note,
    reader_package = "base R",
    reader_function = "read_stata_legacy_recovery"
  ))
}

read_haven_source <- function(path, format, config) {
  require_package("haven", paste(format, "input"))
  n_max <- config$input$n_max %||% Inf
  encoding <- config$input$haven_encoding %||% NULL

  reader_function <- NA_character_
  import_notes <- NA_character_
  data <- switch(
    format,
    dta = {
      reader_function <- "read_dta"
      haven::read_dta(path, encoding = encoding, n_max = n_max, .name_repair = "minimal")
    },
    sas7bdat = {
      reader_function <- "read_sas"
      catalog <- resolve_sas_catalog(path, config)
      if (!is.null(catalog)) {
        import_notes <- paste0(
          "SAS catalog used: ",
          normalizePath(catalog, winslash = "/", mustWork = TRUE)
        )
      }

      sas_args <- list(
        data_file = path,
        n_max = n_max,
        .name_repair = "minimal"
      )
      if (!is.null(catalog)) sas_args$catalog_file <- catalog
      if (!is.null(encoding)) sas_args$encoding <- encoding
      catalog_encoding <- config$input$sas_catalog_encoding %||% encoding
      if (!is.null(catalog_encoding)) sas_args$catalog_encoding <- catalog_encoding

      do.call(haven::read_sas, sas_args)
    },
    xpt = {
      reader_function <- "read_xpt"
      haven::read_xpt(path, n_max = n_max, .name_repair = "minimal")
    },
    sav = {
      reader_function <- "read_sav"
      haven::read_sav(path, encoding = encoding, user_na = TRUE, n_max = n_max, .name_repair = "minimal")
    },
    zsav = {
      reader_function <- "read_sav"
      haven::read_sav(path, encoding = encoding, user_na = TRUE, n_max = n_max, .name_repair = "minimal")
    },
    por = {
      reader_function <- "read_por"
      haven::read_por(path, user_na = TRUE, n_max = n_max, .name_repair = "minimal")
    },
    stop("Unsupported haven format: ", format, call. = FALSE)
  )

  list(make_dataset_record(
    data = data,
    path = path,
    source_format = format,
    dataset_name = source_stem(path),
    import_notes = import_notes,
    reader_package = "haven",
    reader_function = reader_function
  ))
}

read_haven_source_isolated <- function(path, format, config) {
  legacy_layout <- NULL
  if (identical(format, "dta")) {
    legacy_layout <- tryCatch(
      inspect_stata_legacy_layout(
        path,
        as.character(config$input$haven_encoding %||% "latin1")[[1L]]
      ),
      error = function(error) NULL
    )
    if (!is.null(legacy_layout) && isTRUE(legacy_layout$unexpected_trailing_payload)) {
      log_warn(
        "Detected ", format(legacy_layout$trailing_bytes, big.mark = ",", scientific = FALSE),
        " trailing byte(s) after an empty Stata value-label terminator; using the base-R safety reader."
      )
      return(read_stata_legacy_recovery(path, config, legacy_layout))
    }
  }

  repo_root <- find_repo_root(getwd())
  worker_script <- file.path(repo_root, "scripts", "read_haven_worker.R")
  if (!file.exists(worker_script)) stop("Haven worker script not found: ", worker_script, call. = FALSE)

  config_path <- tempfile(pattern = "dataflow-haven-config-", fileext = ".rds")
  result_path <- tempfile(pattern = "dataflow-haven-result-", fileext = ".rds")
  log_path <- tempfile(pattern = "dataflow-haven-log-", fileext = ".log")
  on.exit(unlink(c(config_path, result_path, log_path), force = TRUE), add = TRUE)
  saveRDS(config, config_path)

  old_directory <- getwd()
  on.exit(setwd(old_directory), add = TRUE)
  setwd(repo_root)
  arguments <- c(
    "--vanilla",
    file.path("scripts", "read_haven_worker.R"),
    "--input", normalizePath(path, winslash = "/", mustWork = TRUE),
    "--format", format,
    "--config", config_path,
    "--result", result_path
  )
  status <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    args = vapply(arguments, shQuote, character(1L)),
    stdout = log_path,
    stderr = log_path
  ))

  if (identical(as.integer(status), 0L) && file.exists(result_path)) {
    return(readRDS(result_path))
  }

  if (identical(format, "dta") && !is.null(legacy_layout)) {
    log_warn(
      "The isolated Haven Stata reader exited with status ", as.integer(status),
      "; continuing with the base-R legacy safety reader."
    )
    return(read_stata_legacy_recovery(path, config, legacy_layout))
  }

  worker_log <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character()
  worker_message <- paste(tail(worker_log[nzchar(worker_log)], 8L), collapse = " ")
  if (!nzchar(worker_message)) worker_message <- "No worker diagnostics were produced."
  stop(
    "The isolated ", format, " reader failed with status ", as.integer(status),
    ". ", worker_message,
    call. = FALSE
  )
}

read_rds_source <- function(path, config) {
  object <- readRDS(path)
  objects <- normalize_tabular_object(object, source_stem(path))
  lapply(names(objects), function(name) {
    data <- limit_rows(objects[[name]], config$input$n_max)
    make_dataset_record(
      data = data,
      path = path,
      source_format = "rds",
      dataset_name = name,
      source_object = name,
      reader_package = "base R",
      reader_function = "readRDS"
    )
  })
}

read_rdata_source <- function(path, config) {
  env <- new.env(parent = baseenv())
  loaded <- load(path, envir = env)
  selector <- selector_values(config$input$r_objects)
  selected <- select_named_items(loaded, selector, "R object(s)")

  records <- list()
  for (object_name in selected) {
    object <- get(object_name, envir = env, inherits = FALSE)
    normalized <- tryCatch(
      normalize_tabular_object(object, object_name),
      error = function(e) {
        log_warn("Skipping non-tabular R object '", object_name, "' in ", basename(path), ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(normalized)) next

    for (name in names(normalized)) {
      data <- limit_rows(normalized[[name]], config$input$n_max)
      records[[length(records) + 1L]] <- make_dataset_record(
        data = data,
        path = path,
        source_format = "rdata",
        dataset_name = name,
        source_object = object_name,
        reader_package = "base R",
        reader_function = "load"
      )
    }
  }
  if (length(records) == 0L) stop("No selected R objects were tabular.", call. = FALSE)
  records
}

read_json_source <- function(path, format, config) {
  require_package("jsonlite", "JSON input")
  encoding <- as.character(config$input$text_encoding %||% "UTF-8")

  if (format == "jsonl") {
    con <- open_text_connection(path, encoding = encoding)
    on.exit(close(con), add = TRUE)
    object <- jsonlite::stream_in(con, verbose = FALSE, flatten = TRUE)
    reader_function <- "stream_in"
  } else {
    if (grepl("\\.(gz|bz2|xz)$", tolower(path))) {
      con <- open_text_connection(path, encoding = encoding)
      on.exit(close(con), add = TRUE)
      text <- paste(readLines(con, warn = FALSE), collapse = "\n")
      object <- jsonlite::fromJSON(text, flatten = TRUE, simplifyDataFrame = TRUE)
    } else {
      object <- jsonlite::fromJSON(path, flatten = TRUE, simplifyDataFrame = TRUE)
    }
    reader_function <- "fromJSON"
  }

  objects <- normalize_tabular_object(object, source_stem(path))
  lapply(names(objects), function(name) {
    data <- limit_rows(objects[[name]], config$input$n_max)
    make_dataset_record(
      data = data,
      path = path,
      source_format = format,
      dataset_name = name,
      source_object = name,
      reader_package = "jsonlite",
      reader_function = reader_function
    )
  })
}

read_arrow_source <- function(path, format, config) {
  require_package("arrow", paste(format, "input; install optional dependencies with scripts/bootstrap.R --with-optional"))
  reader_function <- NA_character_
  read_ipc <- function(path) {
    tryCatch(
      {
        reader_function <<- "read_ipc_file"
        arrow::read_ipc_file(path, as_data_frame = TRUE)
      },
      error = function(file_error) {
        tryCatch(
          {
            reader_function <<- "read_ipc_stream"
            arrow::read_ipc_stream(path, as_data_frame = TRUE)
          },
          error = function(stream_error) {
            stop(
              "Could not read Arrow IPC as a file or stream. File error: ", conditionMessage(file_error),
              "; stream error: ", conditionMessage(stream_error), call. = FALSE
            )
          }
        )
      }
    )
  }
  data <- switch(
    format,
    parquet = {
      reader_function <- "read_parquet"
      arrow::read_parquet(path, as_data_frame = TRUE)
    },
    feather = {
      reader_function <- "read_feather"
      arrow::read_feather(path, as_data_frame = TRUE)
    },
    arrow = read_ipc(path),
    ipc = read_ipc(path),
    stop("Unsupported Arrow format: ", format, call. = FALSE)
  )
  data <- limit_rows(data, config$input$n_max)
  list(make_dataset_record(
    data = data,
    path = path,
    source_format = format,
    dataset_name = source_stem(path),
    reader_package = "arrow",
    reader_function = reader_function
  ))
}

read_pickle_source <- function(path, config) {
  if (!isTRUE(config$input$allow_unsafe_pickle)) {
    stop(
      "Python pickle input is disabled. Pickle files can execute arbitrary code when loaded. ",
      "Only for a trusted file, rerun with --allow-unsafe-pickle true.",
      call. = FALSE
    )
  }
  require_package("reticulate", "trusted Python pickle input")
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("reticulate could not find a usable Python installation.", call. = FALSE)
  }
  object <- reticulate::py_load_object(path, convert = TRUE)
  objects <- normalize_tabular_object(object, source_stem(path))
  lapply(names(objects), function(name) {
    data <- limit_rows(objects[[name]], config$input$n_max)
    make_dataset_record(
      data = data,
      path = path,
      source_format = "pickle",
      dataset_name = name,
      source_object = name,
      import_notes = "Loaded from a trusted Python pickle after explicit opt-in.",
      reader_package = "reticulate",
      reader_function = "py_load_object"
    )
  })
}

read_source_file <- function(path, config) {
  format <- detect_source_format(path)
  if (is.na(format)) stop("Unsupported file format: ", basename(path), call. = FALSE)

  switch(
    format,
    csv = read_delimited_source(path, format, config),
    tsv = read_delimited_source(path, format, config),
    txt = read_delimited_source(path, format, config),
    xls = read_excel_source(path, format, config),
    xlsx = read_excel_source(path, format, config),
    dta = read_haven_source_isolated(path, format, config),
    sas7bdat = read_haven_source_isolated(path, format, config),
    xpt = read_haven_source_isolated(path, format, config),
    sav = read_haven_source_isolated(path, format, config),
    zsav = read_haven_source_isolated(path, format, config),
    por = read_haven_source_isolated(path, format, config),
    rds = read_rds_source(path, config),
    rdata = read_rdata_source(path, config),
    json = read_json_source(path, format, config),
    jsonl = read_json_source(path, format, config),
    parquet = read_arrow_source(path, format, config),
    feather = read_arrow_source(path, format, config),
    arrow = read_arrow_source(path, format, config),
    ipc = read_arrow_source(path, format, config),
    pickle = read_pickle_source(path, config),
    stop("No reader implemented for format: ", format, call. = FALSE)
  )
}
