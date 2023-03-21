library(shiny)
library(tidyverse)
library(DT)
library(waiter)
library(arrow)
library(geoAr)
# library(comunicacion)
library(herramientas)
library(leaflet)
library(sf)
library(plotly)

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

serie_puna <- read_parquet("/srv/DataDNMYE/puna/serie_puna.parquet")

argentina <- read_sf("/srv/DataDNMYE/capas_sig/argentina.geojson") %>% 
  mutate(name_iso = str_replace(name_iso, "Tierra del Fuego", "Tierra del Fuego, Antártida e Islas del Atlántico Sur"))
  
loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)