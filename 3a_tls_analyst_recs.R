library(tidyverse)
library(officer)

source("r/functions.R")

line_items <- bbmR::get_last_mod(
  "G:/Fiscal Years/Fiscal 2022/Planning Year/6. TLS/1. Line Item Reports",
  ".xlsx") %>%
  readxl::read_xlsx() %>%
  mutate_at(vars(ends_with("ID")), as.character)

agencies <- unique(line_items$`Agency ID`)
agencies <- agencies[1:3]

sapply(paste0("outputs/", agencies), dir.create)
  
agencies %>%
  lapply(subset_agency_data) %>%
  lapply(export_service_file)

agencies %>%
  lapply(compile_agency_doc)
