#!/usr/bin/env Rscript

required <- c("yaml", "readr", "readxl", "haven", "jsonlite", "openxlsx")
missing <- required[
  !vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L))
]

if (length(missing) > 0L) {
  stop(
    "Missing required local R packages: ", paste(missing, collapse = ", "),
    ". Run Rscript --vanilla scripts/bootstrap.R once before using DataFlow.",
    call. = FALSE
  )
}

cat("runtime_check=PASS\n")
