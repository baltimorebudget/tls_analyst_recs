import_data <- function(tls_line_item_path, tls_position_path, cls_position_file, projection_position_file) {
  
  data <- list(
    line_items = bbmR::get_last_mod(tls_line_item_path, ".xlsx") %>%
      readxl::read_xlsx() %>%
      set_names(rename_cols(.)),
    positions_fy21 = readxl::read_xlsx(projection_position_file) %>%
      set_names(rename_cols(.)),
    positions_fy22_cls = readxl::read_xlsx(cls_position_file) %>%
      set_names(rename_cols(.)),
    positions_fy22_tls = bbmR::get_last_mod(tls_position_path, ".xlsx") %>%
      readxl::read_xlsx() %>%
      set_names(rename_cols(.))) %>%
    lapply(mutate_at, vars(ends_with("ID")), as.character) %>%
    lapply(mutate,`Fund Name` = ifelse(`Fund Name` != "General", "Other", "General")) %>%
    lapply(group_by,`Agency ID`, `Agency Name`, `Service ID`, `Service Name`, `Fund Name`)
  
  data$line_items <- data$line_items %>%
    summarize_if(is.numeric, sum, na.rm = TRUE)
  
  data$positions_fy21 <- data$positions_fy21 %>%
    count(name = "FY21 Adopted") %>%
    ungroup() %>%
    select(-`Agency Name`, -`Service Name`)
  data$positions_fy22_cls <- data$positions_fy22_cls %>%
    count(name = "FY22 CLS") %>%
    ungroup() %>%
    select(-`Agency Name`, -`Service Name`)
  data$positions_fy22_tls <- data$positions_fy22_tls %>%
    count(name = "FY22 TLS") %>%
    ungroup()
  
  data$positions <- data$positions_fy22_tls %>%
    left_join(data$positions_fy22_cls) %>%
    left_join(data$positions_fy21)
  
  data[c("positions_fy21", "positions_fy22_cls", "positions_fy22_tls")] <- NULL
  
  return(data)
}

subset_agency_data <- function(agency_id, data_list) {
  agency_list <- data_list %>%
    lapply(filter, `Agency ID` == agency_id)
  
  return(agency_list)
}