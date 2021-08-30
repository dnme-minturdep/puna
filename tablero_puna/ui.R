navbarPage(title = div(  #### NavBar #####
                         div(
                           id = "img-id",
                           tags$a(img(src = "https://upload.wikimedia.org/wikipedia/commons/8/8e/Ministerio_de_Turismo_y_Deportes_arg.png",
                                      width = 100),href="https://www.yvera.tur.ar/estadistica/",target = '_blank'
                           )),
                         "TABLERO DE CONSULTAS PUNA", id = "title"),
           id="navbar",
           position = "fixed-top",
           windowTitle = "PUNA", 
           collapsible = TRUE,
           tabPanel("SERIE",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        )),
                    br(),
                    fluidPage(
                      fluidRow(
        column(3,selectInput("anio",
                             "Año:",
                             choices = c("Todos",unique(serie_puna$anio)),
                             selected = 2020, 
                             multiple = TRUE)),
        
        column(3,selectInput("ruta",
                             "Ruta Natural:",
                             choices = c("Todos",unique(serie_puna$ruta_natural)),
                             multiple =TRUE)),
        
        column(3,selectInput("region",
                             "Región:",
                             choices = c("Todos",unique(serie_puna$region)),
                             multiple =TRUE)),
        
        column(3,selectInput("provincia",
                             "Provincia:",
                             choices = c("Todos",unique(serie_puna$provincia)),
                             multiple =TRUE))
    ),
    fluidRow(
        column(3,selectInput("depto",
                             "Departamento/partido:",
                             choices = c("Todos",unique(serie_puna$departamento_partido)),
                             multiple =TRUE)),
        
        column(3,selectInput("localidad",
                             "Localidad:",
                             choices = c("Todos",unique(serie_puna$localidad)),
                             multiple =TRUE)),
        
        column(3,selectInput("clasificacion",
                             "Clasificación:",
                             choices = c("Todos",unique(serie_puna$clasificacion_mintur)),
                             multiple =TRUE)),
        
        column(3,selectInput("tipo",
                             "Tipo:",
                             choices = c("Todos",unique(serie_puna$tipo)),
                             multiple =TRUE))
    ),
    
    br(),
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
))
