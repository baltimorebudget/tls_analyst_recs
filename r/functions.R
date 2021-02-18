subset_agency_data <- function(agency_id) {
  line_items %>%
    filter(`Agency ID` == agency_id)
}

fill_template <- function(agency_df) {
  
  agency_name <- unique(agency_df$`Agency Name`)
  service_id <- unique(agency_df$`Program Name`)
  service_name <- unique(agency_df$`Program Name`)

  template %>%
    body_replace_all_text("AGENCY_NAME", agency_name, fixed = TRUE) %>%
    body_replace_all_text("SERVICE_ID", service_id, fixed = TRUE)  %>%
    footers_replace_all_text("SERVICE_ID", service_id, fixed = TRUE) %>%
    body_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE) %>%
    footers_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE) %>%
    body_add_docx("inputs/fy22_template.docx") %>%
    print(paste0("outputs/FY22", agency, "Analyst Rec.docx"))
  
}
