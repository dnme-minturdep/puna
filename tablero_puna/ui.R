library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)

fluidPage(
    
    tags$head(
        tags$style(HTML("
      h1 {
      font-family: 'Encode Sans Normal', Helvetica;
      font-size: 26px;
      color: black;
      font-weight: bold;
      }
      h3 {
      font-family: 'Encode Sans Normal';
      font-size: 20px;
      font-weight: bold;
      color: #37BBED;
      }"))),
    
    fluidRow(column(11,
                    titlePanel(h1("TABLERO DE CONSULTAS PUNA"))),
             column(1,div(id = "img-id",
                          tags$a(img(src = "https://upload.wikimedia.org/wikipedia/commons/8/8e/Ministerio_de_Turismo_y_Deportes_arg.png",
                                     width = 100),href="https://www.yvera.tur.ar/estadistica/",target = '_blank'
                          )))
    ),
    
    helpText(h3("Filtros")),
    fluidRow(
        column(3,pickerInput("anio",
                             "Año:",
                             unique(serie_puna$anio),
                             selected = 2020, 
                             multiple =TRUE,
                             options = config_select)),
        
        column(3,pickerInput("ruta",
                             "Ruta Natural:",
                             unique(serie_puna$ruta_natural),
                             multiple =TRUE,
                             options = config_select)),
        
        column(3,pickerInput("region",
                             "Región:",
                             unique(serie_puna$region),
                             multiple =TRUE,
                             options = config_select)),
        
        column(3,pickerInput("provincia",
                             "Provincia:",
                             unique(serie_puna$provincia),
                             multiple =TRUE,
                             options = config_select))
    ),
    fluidRow(
        column(3,pickerInput("depto",
                             "Departamento/partido:",
                             unique(serie_puna$departamento_partido),
                             multiple =TRUE,
                             options = config_select)),
        
        column(3,pickerInput("localidad",
                             "Localidad:",
                             unique(serie_puna$localidad),
                             multiple =TRUE,
                             options = config_select)),
        
        column(3,pickerInput("clasificacion",
                             "Clasificación:",
                             unique(serie_puna$clasificacion_mintur),
                             multiple =TRUE,
                             options = config_select)),
        
        column(3,pickerInput("tipo",
                             "Tipo:",
                             unique(serie_puna$tipo),
                             multiple =TRUE,
                             options = config_select))
    ),
    
    helpText(h3("Agrupamiento")),
    fluidRow(
        column(4,
               selectInput("agrup", "Mostrar por:", choices = c("Año",
                                                                "Ruta natural",
                                                                "Región",
                                                                "Provincia",
                                                                "Departamento/partido",
                                                                "Localidad", 
                                                                "Clasificación",
                                                                "Tipo"),
                           selected = "Provincia", multiple = TRUE)
        )),
    
    tabPanel('PUNA', DT::dataTableOutput('tabla'))
)
