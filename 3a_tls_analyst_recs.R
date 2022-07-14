library(tidyverse)
library(officer)
library(magrittr)
library(scales)
library(bbmR)

source("r/prepare_data.R")
source("r/make_word_docs.R")

data <- import_data(
  tls_line_item_path = "G:/Fiscal Years/Fiscal 2022/Planning Year/6. TLS/1. Line Item Reports",
  tls_position_path = "G:/Fiscal Years/Fiscal 2022/Planning Year/6. TLS/2. Position Reports",
  cls_position_file = "G:/Fiscal Years/Fiscal 2022/Planning Year/1. CLS/2. Position Reports/PositionsSalariesOpcs_2020-10-26_CLS_FINAL.xlsx",
  projection_position_file = "G:/Fiscal Years/Fiscal 2021/Planning Year/7. Council/2. Position Reports/PositionsSalariesOpcs - COU_2020-06-17_FINAL.xlsx"
)

agencies <- unique(data$line_items$`Agency ID`)
agencies <- agencies[1:3]

sapply(paste0("outputs/", agencies), dir.create)
  
agencies %>%
  lapply(subset_agency_data, data) %>%
  lapply(export_service_file)

agencies %>%
  lapply(compile_agency_doc)
