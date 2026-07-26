html_table <- function(frame, id, max_rows = 5000L, empty_message = "No records.") {
  frame <- atomicize_data_frame(frame)
  if (!is.data.frame(frame) || nrow(frame) == 0L || ncol(frame) == 0L) {
    return(sprintf('<p class="empty">%s</p>', html_escape(empty_message)))
  }

  truncated <- nrow(frame) > max_rows
  shown <- if (truncated) frame[seq_len(max_rows), , drop = FALSE] else frame
  headers <- paste0("<th>", html_escape(names(shown)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(shown)), function(i) {
    values <- shown[i, , drop = FALSE]
    cells <- vapply(values, function(value) {
      text <- if (length(value) == 0L || is.na(value)) "" else as.character(value)
      paste0("<td>", html_escape(text), "</td>")
    }, character(1L))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1L))

  note <- if (truncated) {
    sprintf(
      '<p class="note">Showing the first %s of %s rows. Complete output is available in CSV, JSON, and Excel.</p>',
      format(max_rows, big.mark = ","), format(nrow(frame), big.mark = ",")
    )
  } else ""

  paste0(
    note,
    '<div class="table-wrap"><table id="', html_escape(id), '"><thead><tr>', headers,
    "</tr></thead><tbody>", paste0(rows, collapse = ""), "</tbody></table></div>"
  )
}

summary_card <- function(label, value) {
  paste0(
    '<div class="card"><div class="card-value">', html_escape(value),
    '</div><div class="card-label">', html_escape(label), "</div></div>"
  )
}

write_html_report <- function(results, path, config) {
  max_rows <- as.integer(config$output$html_max_rows_per_table %||% 5000L)
  summary <- results$summary
  dictionary <- results$data_dictionary
  variable_metadata <- results$variable_metadata
  issues <- results$quality_issues
  high_card <- if (nrow(issues) > 0L) issues[issues$issue_type == "high_cardinality_character", , drop = FALSE] else issues
  sensitive <- if (nrow(issues) > 0L) issues[issues$issue_type == "potential_sensitive_data", , drop = FALSE] else issues
  project_name <- as.character(config$metadata$project_name %||% "")[[1L]]
  run_comment <- as.character(config$metadata$run_comment %||% "")[[1L]]
  project_line <- if (nzchar(trimws(project_name))) {
    paste0('<p><strong>Project:</strong> ', html_escape(project_name), '</p>')
  } else ""
  comment_line <- if (nzchar(trimws(run_comment))) {
    paste0('<div class="run-comment"><strong>Run comment:</strong> ', html_escape(run_comment), '</div>')
  } else ""

  cards <- paste0(c(
    summary_card("Input files", as.character(summary$input_files[[1L]])),
    summary_card("Datasets", as.character(summary$datasets[[1L]])),
    summary_card("Dictionary fields", as.character(summary$dictionary_rows[[1L]])),
    summary_card("Missing definitions", as.character(summary$variables_missing_business_definition[[1L]])),
    summary_card("Quality flags", as.character(summary$quality_issues[[1L]])),
    summary_card("High-cardinality character fields", as.character(nrow(high_card)))
  ), collapse = "")

  generated <- results$run_metadata$completed_utc[[1L]] %||% results$run_metadata$started_utc[[1L]]
  html <- paste0(
'<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Technical Metadata Report</title>
<style>
:root { --ink:#172033; --muted:#5f6b7a; --line:#dfe4ea; --surface:#ffffff; --soft:#f5f7fa; --accent:#2457a7; --warn:#8b3e00; --good:#15603b; }
* { box-sizing:border-box; }
body { margin:0; font-family:system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif; color:var(--ink); background:var(--soft); line-height:1.45; }
header { padding:38px max(24px,5vw); background:var(--ink); color:white; }
header h1 { margin:0 0 8px; font-size:clamp(1.8rem,4vw,3rem); }
header p { margin:0; opacity:.82; }
.run-comment { margin-top:14px; padding:12px 14px; background:rgba(255,255,255,.12); border-radius:8px; max-width:900px; }
main { max-width:1500px; margin:0 auto; padding:28px max(18px,3vw) 60px; }
.cards { display:grid; grid-template-columns:repeat(auto-fit,minmax(180px,1fr)); gap:14px; margin:0 0 26px; }
.card { background:var(--surface); border:1px solid var(--line); border-radius:12px; padding:18px; box-shadow:0 3px 12px rgba(23,32,51,.05); }
.card-value { font-weight:750; font-size:1.75rem; color:var(--accent); }
.card-label { color:var(--muted); margin-top:3px; }
section { background:var(--surface); border:1px solid var(--line); border-radius:12px; padding:22px; margin:18px 0; box-shadow:0 3px 12px rgba(23,32,51,.04); }
h2 { margin:0 0 14px; font-size:1.35rem; }
h3 { margin:22px 0 10px; font-size:1.05rem; }
.callout { border-left:5px solid var(--accent); background:#eef5ff; padding:14px 16px; border-radius:8px; margin:12px 0 18px; }
.callout.warn { border-left-color:var(--warn); background:#fff7ed; }
.concepts { display:grid; grid-template-columns:repeat(auto-fit,minmax(300px,1fr)); gap:14px; }
.concept { border:1px solid var(--line); border-radius:10px; padding:16px; background:#fafbfd; }
.concept h3 { margin:0 0 7px; color:var(--accent); }
.note,.empty { color:var(--muted); font-size:.92rem; }
.controls { display:flex; gap:10px; align-items:center; margin:0 0 12px; flex-wrap:wrap; }
input[type=search] { min-width:min(440px,100%); padding:10px 12px; border:1px solid var(--line); border-radius:8px; font:inherit; }
.table-wrap { overflow:auto; border:1px solid var(--line); border-radius:8px; max-height:660px; }
table { border-collapse:separate; border-spacing:0; width:100%; font-size:.86rem; }
th,td { padding:8px 10px; border-bottom:1px solid var(--line); border-right:1px solid var(--line); text-align:left; vertical-align:top; white-space:nowrap; }
th { position:sticky; top:0; background:#eaf0f8; z-index:1; font-weight:700; }
tr:nth-child(even) td { background:#fafbfc; }
td:last-child,th:last-child { border-right:0; }
footer { color:var(--muted); text-align:center; padding:20px; }
code { background:#eef1f5; border-radius:4px; padding:1px 5px; }
</style>
</head>
<body>
<header><h1>Technical Metadata Report</h1>', project_line,
'<p>Generated ', html_escape(generated), ' by pipeline version ',
html_escape(results$run_metadata$pipeline_version[[1L]]), '.</p>', comment_line, '</header>
<main>
<div class="callout warn"><strong>Internal technical report.</strong> This comprehensive view includes operational, provenance, quality, and review metadata. For a concise external-facing document, use <a href="open_science_metadata_report.html">the open-science metadata report</a>.</div>
<div class="cards">', cards, '</div>
<section>
<h2>How the two products differ</h2>
<div class="concepts">
<div class="concept"><h3>Metadata</h3><p>Data about the source files, datasets, variables, provenance, administration, ingestion process, and observed quality. It is the underlying context that makes the source data understandable and governable.</p></div>
<div class="concept"><h3>Data dictionary</h3><p>The structured field-level reference that organizes selected metadata for review: names, labels, definitions, types, categorical decisions, values, missingness, and rules. After curation and approval, it can serve as the practical single source of truth.</p></div>
</div>
<p class="note">The dictionary uses metadata, but it is not identical to the full metadata package. The standalone <code>data_dictionary.xlsx/json</code> and <code>metadata.xlsx/json</code> files preserve this distinction.</p>
</section>
<section>
<h2>Character-to-categorical decision</h2>
<div class="callout warn">Plain character variables with more than <strong>',
html_escape(as.character(config$profiling$max_categorical_levels)),
'</strong> distinct nonblank values are reported as non-categorical. Their dictionary row includes the observed cardinality and the exact reason. Explicit factors and imported value-labelled fields remain categorical because their source metadata states that intent.</div>
<h3>High-cardinality character variables</h3>',
html_table(high_card, "high-cardinality", max_rows, "No high-cardinality character variables were found."),
'</section>
<section><h2>Data dictionary</h2>
<div class="controls"><label for="dictionary-search">Filter dictionary:</label><input id="dictionary-search" type="search" placeholder="Type a dataset, variable, label, type, definition, or reason"></div>',
html_table(dictionary, "dictionary", max_rows), '</section>
<section><h2>Documentation and review gaps</h2>', html_table(results$documentation_gaps, "documentation-gaps", max_rows, "No documentation gaps were identified."), '</section>
<section><h2>Dictionary field definitions</h2>', html_table(results$dictionary_schema, "dictionary-schema", max_rows), '</section>
<section><h2>Metadata coverage</h2>', html_table(results$metadata_coverage, "metadata-coverage", max_rows), '</section>
<section><h2>Administrative metadata</h2>', html_table(results$administrative_metadata, "administrative", max_rows), '</section>
<section><h2>Input inventory</h2>', html_table(results$input_inventory, "inputs", max_rows), '</section>
<section><h2>File metadata</h2>', html_table(results$file_metadata, "files", max_rows), '</section>
<section><h2>Dataset metadata</h2>', html_table(results$dataset_metadata, "datasets", max_rows), '</section>
<section><h2>Provenance metadata</h2>', html_table(results$provenance_metadata, "provenance", max_rows), '</section>
<section><h2>Detailed variable metadata</h2>', html_table(variable_metadata, "variable-metadata", max_rows), '</section>
<section><h2>Quality and metadata flags</h2>', html_table(issues, "issues", max_rows), '</section>
<section><h2>Potentially sensitive fields</h2>', html_table(sensitive, "sensitive", max_rows, "No sensitive-field patterns were flagged."), '</section>
<section><h2>Categorical levels</h2>', html_table(results$categorical_levels, "levels", max_rows), '</section>
<section><h2>Imported value labels</h2>', html_table(results$value_labels, "labels", max_rows), '</section>
<section><h2>Delimited-text parsing problems</h2>', html_table(results$import_problems, "import-problems", max_rows, "No parsing problems were reported."), '</section>
<section><h2>Supported input formats</h2>', html_table(results$supported_formats, "supported-formats", max_rows), '</section>
<section><h2>Errors</h2>', html_table(results$errors, "errors", max_rows, "No ingestion or profiling errors were recorded."), '</section>
</main>
<footer>Complete dictionary and metadata products are stored alongside this report.</footer>
<script>
(function(){
  var input=document.getElementById("dictionary-search");
  var table=document.getElementById("dictionary");
  if(!input||!table) return;
  input.addEventListener("input",function(){
    var q=input.value.toLowerCase();
    Array.prototype.forEach.call(table.tBodies[0].rows,function(row){
      row.style.display=row.textContent.toLowerCase().indexOf(q)>=0?"":"none";
    });
  });
})();
</script>
</body>
</html>')

  writeLines(html, con = path, useBytes = TRUE)
  invisible(path)
}

public_display_value <- function(x, fallback = "-") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return(fallback)
  value <- trimws(as.character(x[[1L]]))
  if (!nzchar(value) || identical(toupper(value), "NA")) fallback else value
}

public_humanize <- function(x) {
  value <- public_display_value(x)
  if (identical(value, "-")) return(value)
  value <- gsub("_", " ", value, fixed = TRUE)
  paste0(toupper(substr(value, 1L, 1L)), substr(value, 2L, nchar(value)))
}

public_integer <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return("-")
  value <- suppressWarnings(as.numeric(x[[1L]]))
  if (!is.finite(value)) return("-")
  trimws(format(round(value), big.mark = ",", scientific = FALSE))
}

public_missing_summary <- function(count, percent) {
  count_text <- public_integer(count)
  value <- if (is.null(percent) || length(percent) == 0L) NA_real_ else suppressWarnings(as.numeric(percent[[1L]]))
  percent_text <- if (is.finite(value)) paste0(format(round(value, 1L), nsmall = 1L, trim = TRUE), "%") else "-"
  if (identical(count_text, "-") && identical(percent_text, "-")) return("-")
  if (identical(count_text, "-")) return(percent_text)
  if (identical(percent_text, "-")) return(count_text)
  paste0(count_text, " (", percent_text, ")")
}

public_row_flag <- function(frame, column, index) {
  column %in% names(frame) && length(frame[[column]]) >= index && isTRUE(frame[[column]][[index]])
}

public_dictionary_table <- function(dictionary) {
  if (!is.data.frame(dictionary) || nrow(dictionary) == 0L) {
    return(data.frame(
      Variable = character(), Definition = character(), Type = character(),
      `Categories / values` = character(), Missing = character(), Status = character(),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(seq_len(nrow(dictionary)), function(index) {
    definition <- public_display_value(dictionary$business_definition[[index]])
    if (identical(definition, "-")) definition <- public_display_value(dictionary$label[[index]])

    disclosure_review <- public_row_flag(dictionary, "potential_identifier", index) ||
      public_row_flag(dictionary, "potential_sensitive", index)
    representation <- public_display_value(dictionary$value_representation[[index]])
    public_domain <- representation %in% c(
      "code = label mapping", "defined factor levels",
      "type-defined logical domain", "logical values", "observed categorical values"
    )
    values <- if (public_domain && !disclosure_review) {
      public_display_value(dictionary$value_label[[index]])
    } else {
      "-"
    }

    status <- character()
    if (identical(definition, "-")) status <- c(status, "Definition needed")
    if (disclosure_review) status <- c(status, "Disclosure review")
    if (length(status) == 0L) status <- "Definition available"

    type_value <- public_display_value(dictionary$measurement_level[[index]])
    if (identical(type_value, "-")) type_value <- public_display_value(dictionary$semantic_type[[index]])

    data.frame(
      Variable = public_display_value(dictionary$variable[[index]]),
      Definition = definition,
      Type = public_humanize(type_value),
      `Categories / values` = values,
      Missing = public_missing_summary(
        dictionary$n_missing[[index]],
        dictionary$percent_missing[[index]]
      ),
      Status = paste(status, collapse = "; "),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  bind_rows_fill(rows)
}

public_dataset_catalog <- function(dataset_metadata) {
  if (!is.data.frame(dataset_metadata) || nrow(dataset_metadata) == 0L) {
    return(data.frame(
      Dataset = character(), Records = character(), Variables = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(seq_len(nrow(dataset_metadata)), function(index) {
    title <- public_display_value(dataset_metadata$dataset_label[[index]])
    if (identical(title, "-")) title <- public_display_value(dataset_metadata$dataset_name[[index]])
    if (identical(title, "-")) title <- public_display_value(dataset_metadata$dataset_id[[index]])
    data.frame(
      Dataset = title,
      Records = public_integer(dataset_metadata$n_rows[[index]]),
      Variables = public_integer(dataset_metadata$n_columns[[index]]),
      stringsAsFactors = FALSE
    )
  })
  bind_rows_fill(rows)
}

public_metadata_grid <- function(administrative_metadata) {
  metadata <- if (is.data.frame(administrative_metadata) && nrow(administrative_metadata) > 0L) {
    administrative_metadata[1L, , drop = FALSE]
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  get_value <- function(name) {
    if (!(name %in% names(metadata))) return("-")
    public_display_value(metadata[[name]])
  }

  items <- c(
    "Project" = get_value("project_name"),
    "Description" = get_value("project_description"),
    "Author / citation" = get_value("author"),
    "License" = get_value("license"),
    "Access" = get_value("access_classification"),
    "Access conditions" = get_value("access_permissions"),
    "Tags" = get_value("tags")
  )
  paste0(
    '<dl class="release-grid">',
    paste0(
      '<div><dt>', html_escape(names(items)), '</dt><dd>',
      html_escape(unname(items)), '</dd></div>',
      collapse = ""
    ),
    "</dl>"
  )
}

public_dictionary_sections <- function(dictionary, dataset_metadata) {
  if (!is.data.frame(dictionary) || nrow(dictionary) == 0L) {
    return('<section><h2>Data dictionary</h2><p class="empty">No variables were available.</p></section>')
  }

  dataset_ids <- unique(as.character(dictionary$dataset_id))
  sections <- vapply(seq_along(dataset_ids), function(position) {
    dataset_id <- dataset_ids[[position]]
    selected <- as.character(dictionary$dataset_id) == dataset_id
    dataset_dictionary <- dictionary[selected, , drop = FALSE]
    title <- public_display_value(dataset_dictionary$dataset_name[[1L]])
    if (identical(title, "-")) title <- public_display_value(dataset_id)

    if (is.data.frame(dataset_metadata) && nrow(dataset_metadata) > 0L) {
      metadata_index <- match(dataset_id, as.character(dataset_metadata$dataset_id))
      if (!is.na(metadata_index)) {
        label <- public_display_value(dataset_metadata$dataset_label[[metadata_index]])
        if (!identical(label, "-")) title <- label
      }
    }

    table <- public_dictionary_table(dataset_dictionary)
    paste0(
      '<section class="dictionary-section">',
      '<div class="section-heading"><div><p class="eyebrow">Dataset</p><h2>',
      html_escape(title),
      '</h2></div><span class="row-count">',
      public_integer(nrow(table)),
      ' variables</span></div>',
      html_table(table, paste0("public-dictionary-", position), max_rows = max(1L, nrow(table))),
      "</section>"
    )
  }, character(1L))
  paste0(sections, collapse = "")
}

write_public_html_report <- function(results, path, config) {
  summary <- results$summary
  dictionary <- results$data_dictionary
  dataset_metadata <- results$dataset_metadata
  administrative_metadata <- results$administrative_metadata

  project_name <- public_display_value(config$metadata$project_name %||% "Open science data release")
  if (identical(project_name, "-")) project_name <- "Open science data release"
  generated <- public_display_value(
    results$run_metadata$completed_utc[[1L]] %||% results$run_metadata$started_utc[[1L]]
  )
  pipeline_version <- public_display_value(results$run_metadata$pipeline_version[[1L]])
  total_records <- if (is.data.frame(dataset_metadata) && nrow(dataset_metadata) > 0L) {
    sum(as.numeric(dataset_metadata$n_rows), na.rm = TRUE)
  } else {
    NA_real_
  }
  definition_gaps <- if (is.data.frame(summary) && nrow(summary) > 0L) {
    as.integer(summary$variables_missing_business_definition[[1L]])
  } else {
    0L
  }
  disclosure_flags <- if (is.data.frame(dictionary) && nrow(dictionary) > 0L) {
    sum(
      dictionary$potential_identifier %in% TRUE |
        dictionary$potential_sensitive %in% TRUE,
      na.rm = TRUE
    )
  } else {
    0L
  }
  needs_review <- definition_gaps > 0L || disclosure_flags > 0L
  status_title <- if (needs_review) "Draft — review before public release" else "Prepared for publication review"
  status_class <- if (needs_review) "readiness warn" else "readiness ready"
  status_detail <- paste0(
    public_integer(definition_gaps), " definition gap(s); ",
    public_integer(disclosure_flags), " field(s) flagged for disclosure review. ",
    "Flags are screening prompts and require human confirmation."
  )

  cards <- paste0(c(
    summary_card("Datasets", public_integer(summary$datasets[[1L]])),
    summary_card("Variables", public_integer(summary$variables[[1L]])),
    summary_card("Records", public_integer(total_records)),
    summary_card("Categorical fields", public_integer(summary$categorical_variables[[1L]]))
  ), collapse = "")

  html <- paste0(
'<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Open Science Metadata Report — ', html_escape(project_name), '</title>
<style>
:root { --ink:#18312b; --muted:#5b6b66; --line:#d8e2de; --surface:#ffffff; --soft:#f3f7f5; --accent:#176b55; --accent-soft:#e7f4ef; --warn:#8a4b08; --warn-soft:#fff4df; }
* { box-sizing:border-box; }
html { scroll-behavior:smooth; }
body { margin:0; color:var(--ink); background:var(--soft); font-family:Arial,Helvetica,sans-serif; line-height:1.5; }
header { padding:48px max(24px,6vw) 42px; background:linear-gradient(135deg,#14352d,#176b55); color:white; }
.kicker,.eyebrow { margin:0 0 7px; text-transform:uppercase; letter-spacing:.11em; font-size:.76rem; font-weight:700; }
header h1 { max-width:900px; margin:0; font-size:clamp(2rem,5vw,3.4rem); line-height:1.08; }
header .subtitle { max-width:760px; margin:14px 0 0; opacity:.86; font-size:1.05rem; }
.actions { display:flex; gap:10px; margin-top:24px; flex-wrap:wrap; }
button { appearance:none; border:0; border-radius:8px; background:white; color:var(--accent); padding:11px 16px; font:inherit; font-weight:700; cursor:pointer; }
button:hover,button:focus { outline:3px solid rgba(255,255,255,.3); }
main { max-width:1180px; margin:0 auto; padding:30px max(18px,3vw) 64px; }
.cards { display:grid; grid-template-columns:repeat(4,minmax(150px,1fr)); gap:12px; margin-bottom:18px; }
.card,section,.readiness { background:var(--surface); border:1px solid var(--line); border-radius:12px; box-shadow:0 3px 14px rgba(24,49,43,.05); }
.card { padding:17px; }
.card-value { color:var(--accent); font-weight:750; font-size:1.7rem; }
.card-label { color:var(--muted); margin-top:2px; }
.readiness { padding:18px 20px; margin:0 0 20px; border-left:6px solid var(--accent); }
.readiness.warn { border-left-color:var(--warn); background:var(--warn-soft); }
.readiness.ready { background:var(--accent-soft); }
.readiness h2 { margin:0 0 4px; font-size:1.08rem; }
.readiness p { margin:0; color:var(--muted); }
section { margin:18px 0; padding:22px; }
section h2 { margin:0 0 14px; font-size:1.35rem; }
.section-heading { display:flex; justify-content:space-between; gap:16px; align-items:flex-end; margin-bottom:14px; }
.section-heading h2 { margin:0; }
.section-heading .eyebrow { color:var(--accent); }
.row-count { color:var(--muted); white-space:nowrap; }
.release-grid { display:grid; grid-template-columns:repeat(2,minmax(0,1fr)); gap:0; margin:0; border:1px solid var(--line); border-radius:9px; overflow:hidden; }
.release-grid div { padding:12px 14px; border-bottom:1px solid var(--line); }
.release-grid div:nth-child(odd) { border-right:1px solid var(--line); }
.release-grid dt { color:var(--muted); font-size:.78rem; font-weight:700; text-transform:uppercase; letter-spacing:.05em; }
.release-grid dd { margin:3px 0 0; overflow-wrap:anywhere; }
.controls { display:flex; gap:10px; align-items:center; margin:0 0 12px; flex-wrap:wrap; }
input[type=search] { width:min(520px,100%); padding:10px 12px; border:1px solid var(--line); border-radius:8px; font:inherit; }
.table-wrap { overflow-x:auto; border:1px solid var(--line); border-radius:8px; }
table { width:100%; border-collapse:collapse; font-size:.88rem; }
th,td { padding:9px 10px; border-bottom:1px solid var(--line); text-align:left; vertical-align:top; }
th { background:#eaf3ef; font-weight:700; }
tr:last-child td { border-bottom:0; }
td:nth-child(1) { font-family:ui-monospace,SFMono-Regular,Menlo,monospace; font-size:.82rem; }
td:nth-child(2),td:nth-child(4) { white-space:normal; min-width:210px; }
.note,.empty { color:var(--muted); }
.method-note { font-size:.92rem; }
.method-note ul { margin-bottom:0; }
footer { padding:20px; color:var(--muted); text-align:center; font-size:.86rem; }
@media (max-width:760px) {
  .cards { grid-template-columns:repeat(2,minmax(0,1fr)); }
  .release-grid { grid-template-columns:1fr; }
  .release-grid div:nth-child(odd) { border-right:0; }
}
@page { size:A4 landscape; margin:11mm; }
@media print {
  :root { --ink:#000; --muted:#333; --line:#aaa; --surface:#fff; --soft:#fff; --accent:#000; --accent-soft:#fff; --warn:#000; --warn-soft:#fff; }
  body { background:white; font-size:9pt; }
  header { padding:0 0 8mm; background:white; color:black; border-bottom:2px solid black; }
  header h1 { font-size:24pt; }
  header .subtitle { margin-top:5px; }
  .no-print { display:none !important; }
  main { max-width:none; padding:7mm 0 0; }
  .cards { grid-template-columns:repeat(4,1fr); gap:4mm; }
  .card,section,.readiness { box-shadow:none; border-color:#aaa; }
  .card { padding:4mm; }
  .card-value { font-size:15pt; }
  section { padding:5mm; margin:5mm 0; break-inside:auto; }
  .release-grid { grid-template-columns:repeat(2,1fr); }
  .table-wrap { overflow:visible; border:0; }
  table { font-size:7.5pt; }
  thead { display:table-header-group; }
  tr { break-inside:avoid; page-break-inside:avoid; }
  th,td { padding:4px 5px; border:1px solid #bbb; }
  td:nth-child(2),td:nth-child(4) { min-width:0; }
  .dictionary-section { break-before:page; }
  .dictionary-section:first-of-type { break-before:auto; }
  footer { padding:4mm 0 0; }
}
</style>
</head>
<body>
<header>
<p class="kicker">Open Science Metadata Report</p>
<h1>', html_escape(project_name), '</h1>
<p class="subtitle">A concise, field-level guide to the datasets in this release. No record-level data are included.</p>
<div class="actions no-print"><button type="button" onclick="window.print()">Print / Save as PDF</button></div>
</header>
<main>
<div class="cards">', cards, '</div>
<div class="', status_class, '"><h2>', html_escape(status_title), '</h2><p>', html_escape(status_detail), '</p></div>
<section><h2>About this release</h2>', public_metadata_grid(administrative_metadata), '</section>
<section><h2>Dataset overview</h2>',
html_table(public_dataset_catalog(dataset_metadata), "public-datasets", max_rows = max(1L, nrow(dataset_metadata))),
'</section>
<section class="no-print"><h2>Find a variable</h2>
<div class="controls"><label for="public-search">Filter the dictionary:</label><input id="public-search" type="search" placeholder="Type a variable, definition, type, category, or status"></div>
<p class="note">The filter affects the on-screen view only. Printing includes the complete dictionary.</p></section>',
public_dictionary_sections(dictionary, dataset_metadata),
'<section class="method-note"><h2>How to read this report</h2>
<ul>
<li>A dash (<strong>-</strong>) means information was not supplied or does not apply.</li>
<li>Definitions copied from source labels are candidates for human review, not independently validated descriptions.</li>
<li>Observed categories describe this dataset; they are not automatically authoritative allowed-value rules.</li>
<li>Categories are withheld for fields heuristically flagged for identifier or disclosure review.</li>
<li>This public report intentionally excludes private file paths, file hashes, filesystem details, software diagnostics, run comments, representative values, and record-level data.</li>
</ul></section>
</main>
<footer>Generated ', html_escape(generated), ' with local pipeline version ', html_escape(pipeline_version), '.</footer>
<script>
(function(){
  var input=document.getElementById("public-search");
  if(!input) return;
  input.addEventListener("input",function(){
    var q=input.value.toLowerCase();
    Array.prototype.forEach.call(document.querySelectorAll(".dictionary-section tbody tr"),function(row){
      row.style.display=row.textContent.toLowerCase().indexOf(q)>=0?"":"none";
    });
  });
  window.addEventListener("beforeprint",function(){
    Array.prototype.forEach.call(document.querySelectorAll(".dictionary-section tbody tr"),function(row){
      row.dataset.screenDisplay=row.style.display;
      row.style.display="";
    });
  });
  window.addEventListener("afterprint",function(){
    Array.prototype.forEach.call(document.querySelectorAll(".dictionary-section tbody tr"),function(row){
      row.style.display=row.dataset.screenDisplay||"";
      delete row.dataset.screenDisplay;
    });
  });
})();
</script>
</body>
</html>')

  writeLines(html, con = path, useBytes = TRUE)
  invisible(path)
}
