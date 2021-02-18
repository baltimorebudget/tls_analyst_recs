library(tidyverse)
library(officer)

template <- read_docx("inputs/fy22_template.docx") 

agency <- "Board of Elections"
service_id <- "100"
service_name <- "Fair Conduct"

doc <- template %>%
  body_replace_all_text("AGENCY_NAME", agency, fixed = TRUE) %>%
  body_replace_all_text("SERVICE_ID", service_id, fixed = TRUE)  %>%
  footers_replace_all_text("SERVICE_ID", service_id, fixed = TRUE) %>%
  body_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE) %>%
  footers_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE) %>%
  body_add_docx("inputs/fy22_template.docx")

print(doc, paste0("outputs/", agency, ".docx"))
