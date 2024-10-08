shinyUI(
  
  navbarPage(title = div(  #### NavBar #####
                           div(
                             id = "img-id",
                             tags$a(img(src = "https://tableros.yvera.tur.ar/recursos/logo_sinta.png",
                                        width = 100),href="https://www.yvera.tur.ar/sinta/",target = '_blank'
                             )),
                           icon("hotel"),"PUNA", id = "title", class = "navbar1"),
             id="navbar",
             position = "fixed-top",
             windowTitle = "Alojamientos - Turismo Argentina", 
             collapsible = TRUE,
             header = includeCSS("styles.css"),
             
             tabPanel("RESUMEN",
                      
                      tags$head(includeHTML("/srv/DataDNMYE/login_shiny/puna.html")),
                      
                    div(id="container-info",
                        
                        br(),
                        h5(tags$p("El tablero del Padrón Único Nacional de Alojamiento (PUNA) presenta datos agregados sobre la oferta de alojamiento en Argentina. En la pestaña ", tags$b("TABLERO"),
                                  "puede conocer por unidad territorial (región, provincia, etc.) y categoría de los alojamientos, indicadores de cantidad de establecimientos, plazas, unidades y habitaciones. Para más información sobre la fuente de dato diríjase a la solapa de ",tags$b("METODOLOGÍA."))),
                        br(),
                        
                        fluidRow(
                          
                          column(width = 5, 
                                 h4("Plazas por provincia - año 2023"),
                                 leafletOutput("mapa_arg", height = 520)
                          ),
                          
                          column(width = 7, plotlyOutput("graph_clas", height = 500)
                                 
                          )
                        
                        ),
                        br()
                        )
                      
             ),
             
             tabPanel("TABLERO",
                      useWaiter(),
                      waiter_show_on_load(html = loading_screen, color = "white"),
                      
                    div(id="container-info",
                          fluidPage(
                            h3("FILTROS"),
                            
                            h5("Los siguientes comandos permiten filtrar los datos."),
                            
                            fluidRow(
                              column(3,selectInput("anio",
                                                   "Año:",
                                                   choices = c("Todos",unique(serie_puna$anio)),
                                                   selected = max(serie_puna$anio), 
                                                   multiple = TRUE)),
                              
                              column(3,selectInput("ruta",
                                                   "Región Natural:",
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
                              
                              column(3,selectizeInput("localidad",
                                                      "Localidad:",
                                                      choices = NULL,
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
                            
                            h3("VISUALIZACIÓN"),
                            h5("Selecciona el nivel de apertura con que se visualizan los datos."),
                            
                            fluidRow(
                              column(4,
                                     selectInput("agrup", "Mostrar por:", choices = c("Año",
                                                                                      "Región natural",
                                                                                      "Región",
                                                                                      "Provincia",
                                                                                      "Departamento/partido",
                                                                                      "Localidad", 
                                                                                      "Clasificación",
                                                                                      "Tipo"),
                                                 selected = "Año", multiple = TRUE)
                              )),
                            
                            dataTableOutput('tabla'),
                            fluidRow(downloadButton("downloadExcel","Descargar en excel"),
                                     downloadButton("downloadCSV","Descargar en csv")),
                            br()
                          ))
             ),
             
             tabPanel("METODOLOGÍA",
                      
                      div(id="container-info",
                          
                          h3("NOTAS TÉCNICAS"),
                          br(),
                          h4("   El Padrón Único Nacional de Alojamiento (PUNA) reúne todos los establecimientos de alojamiento turístico de la Argentina, 
                en el que se detallan las principales variables de cada uno, nutrido a partir de diferentes fuentes de información."), 
                          h4("   Estas fuentes de información son: los organismos provinciales y municipales de turismo que a través de su área de fiscalización de servicios turísticos 
                elabora el padrón de alojamiento de su provincia o localidad; el INDEC a través de la Encuesta de Ocupación Hotelera y fuentes secundarias especializadas en turismo relevadas desde la DNMYE - MINTURDEP."),
                          
                          h4(" Los datos publicados acá, por su propia naturaleza, se encuentran en revisión continua y deben ser tomados como provisorios."),
                          
                          br(),
                          h3 ("DEFINICIONES Y CONCEPTOS"),
                          br(),
                          h4(tags$ul(
                            tags$p(tags$b(" • AÑO"),": esta variable representa el año al que pertenece el Padrón."),
                            tags$p(tags$b(" • REGIÓN NATURAL"),": La Argentina está dividida en 17 regiones naturales que te invitan a conocer lugares, circuitos y experiencias en todo el país.",
                                   "Debido a la naturaleza de los datos, existen registros que no tienen una Ruta Natural asignada. En estos casos figura 'Sin dato'."), 
                            tags$p(tags$b(" • REGIÓN"),":  refiere a la región turística en la que se encuentra el establecimiento. 
                        El país está dividido en 7 regiones: Buenos Aires, Córdoba, CABA, Cuyo, Litoral, Norte y Patagonia."),  
                            tags$p(tags$b(" • PROVINCIA"),": provincia donde se encuentra el establecimiento."),  
                            tags$p(tags$b(" • DEPARTAMENTO / PARTIDO"),": ubicación del establecimiento dentro de la provincia teniendo en cuenta la división política
                        de la provincia en Departamentos/Partidos."),
                            tags$p(tags$b(" • LOCALIDAD"),": ubicación geográfica del establecimiento dentro de la provincia, según la localidad en la que está ubicado."),
                            tags$p(tags$b(" • CLASIFICACIÓN"),": clasificación propia elaborada a partir de la reagrupación de categorías de alojamiento. La Clasificación MINTURDEP 
                 se elaboró teniendo en cuenta  la ", tags$a(href="http://servicios.infoleg.gob.ar/infolegInternet/anexos/230000-234999/231822/norma.htm", 
                                                             "Ley Nacional de Turismo N° 18.828"),
                                   "y las recomendaciones de la ", 
                                   tags$a(href="https://www.e-unwto.org/doi/book/10.18111/9789213612385", 
                                          " Organización Mundial de Turismo."),
                            tags$p(tags$b(" • TIPO"),": se refiere al tipo al que pertenece la categoría del establecimiento. El MINTURDEP tiene cuatro tipos: hoteleros, parahoteleros, otros colectivos y privados.")))),
                          
                          br(),
                          h3 ("INDICADORES"),
                          br(),
                          h4(tags$ul(
                            tags$p(tags$b(" • ESTABLECIMIENTOS"),": se refiere a la cantidad de establecimientos que conforman los distintos alojamientos turísticos."),
                            tags$p(tags$b(" • UNIDADES"),": son los departamentos, cabañas o bungalows equipados que un establecimiento ofrece, además de otorgar algunos de los servicios de hotelería."),
                            tags$p(tags$b(" • HABITACIONES"),": se refiere al número de habitaciones que tienen los establecimientos."),
                            tags$p(tags$b(" • PLAZAS"),": se refiere a la cantidad de plazas que tienen los establecimientos. Una plaza equivale al espacio que ocupa una persona: por ejemplo, una cama matrimonial cuenta como dos plazas."),
                            br()
                          ))
                          
                      )),
             
             footer = includeHTML("/srv/shiny-server/recursos/shiny_footer.html")
  )
)
