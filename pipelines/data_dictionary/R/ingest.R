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
      if (!is.null(catalog)) import_notes <- paste0("SAS catalog used: ", normalizePath(catalog, winslash = "/", mustWork = TRUE))
      haven::read_sas(
        path,
        catalog_file = catalog,
        encoding = encoding,
        catalog_encoding = config$input$sas_catalog_encoding %||% encoding,
        n_max = n_max,
        .name_repair = "minimal"
      )
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
    dta = read_haven_source(path, format, config),
    sas7bdat = read_haven_source(path, format, config),
    xpt = read_haven_source(path, format, config),
    sav = read_haven_source(path, format, config),
    zsav = read_haven_source(path, format, config),
    por = read_haven_source(path, format, config),
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
