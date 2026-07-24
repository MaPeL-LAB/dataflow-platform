#!/usr/bin/env Rscript

dir.create("examples/data", recursive = TRUE, showWarnings = FALSE)

people <- data.frame(
  person_id = sprintf("P%04d", 1:15),
  person_name = sprintf("Example Person %02d", 1:15),
  status = rep(c("active", "inactive", "pending"), 5),
  region = rep(c("Gauteng", "Western Cape", "KwaZulu-Natal"), 5),
  score = seq(3.5, 52.5, length.out = 15),
  comment = sprintf("Unique free-form observation number %d for profiling.", 1:15),
  email = c(sprintf("person%02d@example.org", 1:12), NA, NA, NA),
  visit_date = as.Date("2026-07-01") + 0:14,
  stringsAsFactors = FALSE
)

readr::write_csv(people, "examples/data/people.csv", na = "")
jsonlite::write_json(people, "examples/data/people.json", dataframe = "rows", pretty = TRUE, na = "null")
saveRDS(people, "examples/data/people.rds")
haven::write_dta(people, "examples/data/people.dta")
haven::write_xpt(people, "examples/data/people.xpt")
openxlsx::write.xlsx(
  list(people = people, lookup = unique(people[c("status", "region")])),
  "examples/data/example_workbook.xlsx",
  overwrite = TRUE
)
message("Synthetic examples created in examples/data")
