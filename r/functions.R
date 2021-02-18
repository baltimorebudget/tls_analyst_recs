subset_agency_data <- function(agency_id) {
  line_items %>%
    filter(`Agency ID` == agency_id)
}

export_service_file <- function(agency_df) {
  
  agency_id <- unique(agency_df$`Agency ID`)
  agency_name <- unique(agency_df$`Agency Name`)

  for (i in unique(agency_df$`Program ID`)) {
    
    service_df <- agency_df %>%
      filter(`Program ID` == i)
      
    service_name <- unique(service_df$`Program Name`)
    
    # have to read the template in everytime since body_replace_all_text()
    # seems to 'set' the variables, even if the R obj isn't overwritten 
    
    doc <- read_docx("inputs/fy22_service_template.docx") %>%
      body_replace_all_text("SERVICE_ID", i, fixed = TRUE)  %>%
      footers_replace_all_text("SERVICE_ID", i, fixed = TRUE) %>%
      body_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE)  %>%
      footers_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE) %>%
      print(paste0("outputs/", agency_id, "/", i, ".docx"))
      
  }
  
  message(str_remove(agency_name, ":"), " service files exported.")
  
}


compile_agency_doc <- function(agency_id) {
  
  service_files <- list.files(paste0("outputs/", agency_id), full.names = TRUE)
  
  agency_name <- line_items %>%
    filter(`Agency ID` == agency_id) %>% 
    magrittr::extract2("Agency Name") %>%
    unique()
  
  doc <- read_docx("inputs/fy22_agency_template.docx") %>%
    body_replace_all_text("AGENCY_NAME", agency_name, fixed = TRUE) %>%
    body_add_break()
  
  for (i in service_files) {
    doc <- doc %>%
      body_add_docx(i) %>%
      body_add_break()
  }
  
  print(doc, paste0("outputs/FY22 ", str_remove(agency_name, ":"), " Analyst Rec.docx"))
  
  unlink(paste0("outputs/", agency_id), recursive = TRUE) # remove dir with intermediate outputs
  
  message(str_remove(agency_name, ":"), " agency file exported.")
}

