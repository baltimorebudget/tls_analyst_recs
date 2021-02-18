library(tidyverse)
library(officer)

source("r/functions.R")

template <- read_docx("inputs/fy22_template.docx") 

line_items <- bbmR::get_last_mod(
  "G:/Fiscal Years/Fiscal 2022/Planning Year/6. TLS/1. Line Item Reports",
  ".xlsx") %>%
  readxl::read_xlsx()

unique(line_items$`Agency ID`) %>%
  lapply(subset_agency_data) %>%
  lapply(fill_template)