library(shiny)
library(tidyverse)
library(DT)
#library(shinyWidgets)

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

serie_puna <- read_csv("/srv/DataDNMYE/puna/serie_puna.zip") %>% 
  mutate(across(2:6, ~ case_when(is.na(.) ~ "sin dato",
                                 TRUE ~ .)))

# config_select <- list(`actions-box` = TRUE,
#                       `deselect-all-text` = "Deseleccionar",
#                       `select-all-text` = "Todos",
#                       `none-selected-text` = "Sin selección",
#                       `live-search`=TRUE,
#                       `selected-text-format` = "count > 1",
#                       `count-selected-text` = "Múltiple"
# )