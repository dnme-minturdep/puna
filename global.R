library(shiny)
library(tidyverse)
library(DT)
library(waiter)

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

serie_puna <- read_csv("/srv/DataDNMYE/puna/serie_puna.zip") %>% 
  mutate(across(2:6, ~ case_when(is.na(.) ~ "Sin dato",
                                 TRUE ~ .)))
loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)