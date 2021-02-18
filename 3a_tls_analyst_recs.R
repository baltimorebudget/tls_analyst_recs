library(tidyverse)
library(officer)

source("r/functions.R")

line_items <- bbmR::get_last_mod(
  "G:/Fiscal Years/Fiscal 2022/Planning Year/6. TLS/1. Line Item Reports",
  ".xlsx") %>%
  readxl::read_xlsx() %>%
  mutate_at(vars(ends_with("ID")), as.character)

sapply(paste0("outputs/", unique(line_items$`Agency ID`)), dir.create)

unique(line_items$`Agency ID`) %>%
  lapply(subset_agency_data) %>%
  lapply(export_service_file)
