export_service_file <- function(agency_list) {
  
  agency_id <- unique(agency_list$line_item$`Agency ID`)
  agency_name <- unique(agency_list$line_item$`Agency Name`)
  
  for (i in unique(agency_list$line_item$`Service ID`)) {
    
    service_list <- agency_list %>%
      lapply(filter, `Service ID` == i)
      
    service_name <- unique(service_list$line_item$`Service Name`)
    
    use_empty_df <- function(df) {
      # for services that don't have general or other funds, use empty df to place a -
      if (nrow(df) == 0) {
        no_budget <- tibble(
          `FY21 Adopted` = "-",
          `FY22 CLS` = "-",
          `FY22 TLS` = "-")
        
        return(no_budget)
      } else {
        return(df)
      }
    }
    
    dollars <- list(
      gf = service_list$line_item %>%
        filter(`Fund Name` == "General") %>% 
        mutate_at(vars(starts_with("FY")), label_comma(accuracy = 1L)),
      of = service_list$line_item %>%
        filter(`Fund Name` == "Other") %>%
        mutate_at(vars(starts_with("FY")), label_comma(accuracy = 1L))) %>%
      lapply(use_empty_df)
  
    positions <- list(
      gf = service_list$positions %>%
        filter(`Fund Name` == "General") %>% 
        mutate_at(vars(starts_with("FY")), label_comma(accuracy = 1L)),
      of = service_list$positions %>%
        filter(`Fund Name` == "Other") %>%
        mutate_at(vars(starts_with("FY")), label_comma(accuracy = 1L))) %>%
      lapply(use_empty_df)
    
    # have to read the template in everytime since body_replace_all_text()
    # seems to 'set' the variables, even if the R obj isn't overwritten 
    
    doc <- read_docx("inputs/fy22_service_template.docx") %>%
      body_replace_all_text("SERVICE_ID", i, fixed = TRUE)  %>%
      body_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE) %>%
      body_replace_all_text(
        "GF_DOLLARS_PROJECTION", dollars$gf$`FY21 Adopted`, fixed = TRUE) %>%
      body_replace_all_text(
        "GF_DOLLARS_PLANNING_CLS", dollars$gf$`FY22 CLS`, fixed = TRUE) %>%
      body_replace_all_text(
        "GF_DOLLARS_PLANNING_TLS", dollars$gf$`FY22 TLS`, fixed = TRUE) %>%
      body_replace_all_text(
        "OF_DOLLARS_PROJECTION", dollars$of$`FY21 Adopted`, fixed = TRUE) %>%
      body_replace_all_text(
        "OF_DOLLARS_PLANNING_CLS", dollars$of$`FY22 CLS`, fixed = TRUE) %>%
      body_replace_all_text(
        "OF_DOLLARS_PLANNING_TLS", dollars$of$`FY22 TLS`, fixed = TRUE) %>%
      body_replace_all_text(
        "GF_POSITIONS_PROJECTION", positions$gf$`FY21 Adopted`, fixed = TRUE) %>%
      body_replace_all_text(
        "GF_POSITIONS_PLANNING_CLS", positions$gf$`FY22 CLS`, fixed = TRUE) %>%
      body_replace_all_text(
        "GF_POSITIONS_PLANNING_TLS", positions$gf$`FY22 TLS`, fixed = TRUE) %>%
      body_replace_all_text(
        "OF_POSITIONS_PROJECTION", positions$of$`FY21 Adopted`, fixed = TRUE) %>%
      body_replace_all_text(
        "OF_POSITIONS_PLANNING_CLS", positions$of$`FY22 CLS`, fixed = TRUE) %>%
      body_replace_all_text(
        "OF_POSITIONS_PLANNING_TLS", positions$of$`FY22 TLS`, fixed = TRUE) %>%
      body_replace_all_text("SERVICE_NAME", service_name, fixed = TRUE) %>%
      print(paste0("outputs/", agency_id, "/", i, ".docx"))
      
  }
  
  message(str_remove(agency_name, ":"), " service files exported.")
  
}


compile_agency_doc <- function(agency_id) {
  
  service_files <- list.files(paste0("outputs/", agency_id), full.names = TRUE)
  
  agency_name <- data$line_items %>%
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

