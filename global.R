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

serie_puna <- read_parquet("/srv/DataDNMYE/puna/serie_puna.parquet") %>% 
  mutate(provincia = case_when(provincia == "Tierra del Fuego, Antartida E Islas Del Atlantico Sur" ~ "Tierra del Fuego",
                               provincia == "Cordoba" ~ "Córdoba",
                               provincia == "Neuquen" ~ "Neuquén",
                               provincia == "Tucuman" ~ "Tucumán",
                               provincia == "Entre Rios" ~ "Entre Ríos",
                               provincia == "Ciudad Autonoma de Buenos Aires" ~ "Ciudad Autónoma de Buenos Aires",
                               provincia == "Rio Negro" ~ "Río Negro",
                               TRUE ~ provincia))

# argentina <- read_sf("/srv/DataDNMYE/capas_sig/")
  
loading_screen <- tagList(
  h3("Cargando...", style = "color:gray;"),
  img(src = "https://tableros.yvera.tur.ar/recursos/logo_mintur_color.png", height = "200px")
)