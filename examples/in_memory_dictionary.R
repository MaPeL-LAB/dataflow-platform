# Run from the repository root with: Rscript examples/in_memory_dictionary.R
source("R/common.R")
pipeline_dir <- file.path("pipelines", "data_dictionary", "R")
for (module in c("config.R", "utils.R", "classify.R", "profile.R", "dictionary.R")) {
  source(file.path(pipeline_dir, module))
}

analysis_data <- data.frame(
  person_name = sprintf("Person %02d", 1:12),
  status = rep(c("active", "inactive"), 6),
  approved = c(TRUE, FALSE, rep(NA, 10)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
attr(analysis_data$person_name, "label") <- "Participant name"
attr(analysis_data$status, "label") <- "Current participation status"

# Imported haven/labelled vectors carry a named `labels` attribute. This base-R
# example reproduces that structure without adding a package dependency.
analysis_data$consent <- rep(c(0, 1), 6)
attr(analysis_data$consent, "label") <- "Consent response"
attr(analysis_data$consent, "labels") <- c(No = 0, Yes = 1)
class(analysis_data$consent) <- c("labelled", class(analysis_data$consent))

dictionary <- func_dictionary(
  DATA = analysis_data,
  max_categorical_levels = 10,
  dataset_id = "analysis_data"
)

print(dictionary[, c(
  "label", "variable", "type", "value_label", "n_missing",
  "categorical_eligible", "value_domain_status", "documentation_status"
)], row.names = FALSE)
