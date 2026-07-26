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
<title>Data Dictionary and Metadata Report</title>
<style>
:root { --ink:#172033; --muted:#5f6b7a; --line:#dfe4ea; --surface:#ffffff; --soft:#f5f7fa; --accent:#2457a7; --warn:#8b3e00; --good:#15603b; }
* { box-sizing:border-box; }
body { margin:0; font-family:system-ui,-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif; color:var(--ink); background:var(--soft); line-height:1.45; }
header { padding:38px max(24px,5vw); background:var(--ink); color:white; }
header h1 { margin:0 0 8px; font-size:clamp(1.8rem,4vw,3rem); }
header p { margin:0; opacity:.82; }
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
<header><h1>Data Dictionary and Metadata Report</h1><p>Generated ', html_escape(generated), ' by pipeline version ',
html_escape(results$run_metadata$pipeline_version[[1L]]), '.</p></header>
<main>
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
