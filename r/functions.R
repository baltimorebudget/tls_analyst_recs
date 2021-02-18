subset_agency_data <- function(agency_id) {
  line_items %>%
    filter(`Agency ID` == agency_id)
}

fill_template <- function(agency_df) {
  
  agency_name <- unique(agency_df$`Agency Name`)

  doc <- read_docx("inputs/fy22_template.docx")
  
  for (i in unique(agency_df$`Program ID`)) {
    
    service_df <- agency_df %>%
      filter(`Program ID` == i)
      
    service_name <- unique(service_df$`Program Name`)
    
    doc <- doc %>%
      body_replace_all_text(
        "SERVICE_ID", i, fixed = TRUE)  %>%
      footers_replace_all_text(
        "SERVICE_ID", i, fixed = TRUE) %>%
      footers_replace_all_text(
        "SERVICE_NAME", service_name, fixed = TRUE) %>%
      body_add_docx("inputs/fy22_template.docx") %>%
      body_replace_all_text("AGENCY_NAME", agency_name, fixed = TRUE)
      
  }
  
  doc %>%
    print(paste0("outputs/FY22 ", str_remove(agency_name, ":"), " Analyst Rec.docx"))
  
  message(str_remove(agency_name, ":"), " file exported.")
  
}
