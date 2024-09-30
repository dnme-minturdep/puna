function(input, output, session) {

    
    ## RESUMEN
    output$mapa_arg <- renderLeaflet({
        
        data <- serie_puna %>%
            filter(anio == max(anio)) %>% 
            group_by(provincia) %>% 
            summarise(establecimientos = sum(establecimientos, na.rm = T),
                      plazas = sum(plazas, na.rm = T)) %>% 
            ungroup() %>% 
            left_join(argentina, by = c("provincia"="name_iso")) %>% 
            mutate(hexfill = colorQuantile("BuPu", n = 9,
                                           domain = plazas)(plazas)) %>% 
            st_as_sf()
        
        labs <- sprintf(
            paste0(data$provincia, "<br>Establecimientos: ", data$establecimientos, "<br>Plazas: ", data$plazas)
        ) %>% lapply(htmltools::HTML)
            

        
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png",
                     attribution = "IGN") %>%
            addPolygons(data = data,
                         color = ~ hexfill,
                         label = labs,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "bold"),
                            textsize = "15px",
                            direction = "auto"),
                        fillOpacity = .8, 
                        weight = .5)
        
    })
    
    
    ## Gráfico clasificación
    output$graph_clas <- renderPlotly({
        ggplotly(serie_puna %>%
                     filter(anio == 2020) %>% 
                     group_by(tipo, clasificacion_mintur) %>% 
                     summarise(establecimientos = sum(establecimientos, na.rm = T)) %>% 
                     ungroup() %>%
                     mutate(tipo = fct_relevel(tipo, c("Hoteleros","Parahoteleros",
                                                       "Otros colectivos")),
                            clasificacion_mintur = fct_reorder(clasificacion_mintur, establecimientos)) %>% 
            ggplot() +
                geom_col(aes(clasificacion_mintur, 
                             establecimientos, fill = tipo,
                             text = paste("Clasificación: ", clasificacion_mintur,
                                          "<br>Establecimientos: ", establecimientos))) +
                labs(title = "Establecimientos según tipo y clasificación - año 2023", 
                     x = "", y = "", fill = "") +
                coord_flip() +
                # scale_fill_dnmye(palette = "cualitativa") +
                theme_minimal() +
                theme(plot.title = element_text(family = "Roboto", hjust = -0.8)), 
            tooltip = "text"
            )
    
    })
    
    ## TABLERO
    puna <- reactive({
        req(input$anio)
        if (input$anio == "Todos") {
            puna <- serie_puna    
        } else {
            puna <- serie_puna[serie_puna$anio %in% input$anio,  ]
        }
    })
    
    observeEvent(puna(), {
        updateSelectInput(session, inputId = "ruta", 
                          choices = c("Todos",sort(unique(puna()$ruta_natural))),
                          selected = "Todos"
        )
    })
    
    ruta <- reactive({
        req(input$ruta)
        if (input$ruta == "Todos") {
            ruta <- puna()    
        } else {
            ruta <- puna()[puna()$ruta_natural %in% input$ruta,  ]
        }
    })
    
    observeEvent(ruta(), {
        updateSelectInput(session, inputId = "region", 
                          choices = c("Todos",sort(unique(ruta()$region))),
                          selected = "Todos")
    })
    
    region <- reactive({
        req(input$region)
        if (input$region == "Todos") {
            region <- ruta()   
        } else {
            region <- ruta()[ruta()$region %in% input$region,  ]
        }
    })
    
    observeEvent(region(), {
        updateSelectInput(session, inputId = "provincia", 
                          choices = c("Todos",sort(unique(region()$provincia))),
                          selected = "Todos")
    })
    
    provincias <- reactive({
        req(input$provincia)
        if (input$provincia == "Todos") {
            provincias <- region()
        } else {
            provincias <- region()[region()$provincia %in% input$provincia,  ]
        }
    })
    
    observeEvent(provincias(), {
        updateSelectInput(session, inputId = "depto", 
                          choices = c("Todos",sort(unique(provincias()$departamento_partido))),
                          selected = "Todos")
    })  
    
    departamentos <- reactive({
        req(input$depto)
        if (input$depto == "Todos") {
            departamentos <- provincias()
        } else {
            departamentos <- provincias()[provincias()$departamento_partido %in% input$depto,  ]
        }
    })
    
    observeEvent(departamentos(), {
        updateSelectizeInput(session, inputId = "localidad", 
                             choices = c("Todos",sort(unique(departamentos()$localidad))),
                             selected = "Todos", server = TRUE)
    })
    
    localidades <- reactive({
        req(input$localidad)
        if (input$localidad == "Todos") {
            localidades <- departamentos()
        } else {
            localidades <- departamentos()[departamentos()$localidad %in% input$localidad,  ]
        }
    })
    
    observeEvent(localidades(), {
        updateSelectInput(session, inputId = "clasificacion", 
                          choices = c("Todos",sort(unique(localidades()$clasificacion_mintur))),
                          selected = "Todos")
    })
    
    clasificacion <- reactive({
        req(input$clasificacion)
        if (input$clasificacion == "Todos") {
            clasificacion <- localidades()
        } else {
            clasificacion <- localidades()[localidades()$clasificacion_mintur %in% input$clasificacion,  ]
        }
    })
    
    observeEvent(clasificacion(), {
        updateSelectInput(session, inputId = "tipo", 
                          choices = c("Todos",sort(unique(clasificacion()$tipo))),
                          selected = "Todos")
    })
    
    tipo <- reactive({
        req(input$tipo)
        if (input$tipo == "Todos") {
            tipo <- clasificacion()
        } else {
            tipo <- clasificacion()[clasificacion()$tipo %in% input$tipo,  ]
        }
    })
    
    
    data_final <- reactive({
        
        tipo() %>% rename("Año" = anio,
                          "Región natural" = ruta_natural,
                          "Región" = region,
                          "Provincia" = provincia,
                          "Departamento/partido" = departamento_partido,
                          "Localidad" = localidad,
                          "Clasificación" = clasificacion_mintur,
                          "Tipo" = tipo) %>%
            group_by_at(.vars = c("Año", input$agrup)) %>%
            summarise("Establecimientos" = round(sum(establecimientos)),
                      "Unidades" = round(sum(unidades, na.rm = T)),
                      "Habitaciones" = round(sum(habitaciones, na.rm = T)),
                      "Plazas" = round(sum(plazas, na.rm = T))
            ) %>% 
            ungroup()
    })
    
    output$tabla <- DT::renderDataTable(
        server = F,
        datatable(#extensions = 'Buttons',
                      options = list(lengthMenu = c(10, 25, 50), pageLength = 10, 
                                     dom = 'lfrtipB'
                                     # buttons = list('copy', 
                                     #                list(
                                     #                    extend = 'collection',
                                     #                    buttons = list(list(extend = 'csv', filename = "puna"),
                                     #                                   list(extend = 'excel', filename = "puna")),
                                     #                    text = 'Download'
                                     #                ))
                                     ),
                      data_final(),
                      rownames= FALSE
        ) %>% 
            formatRound(columns = c(
                "Establecimientos",
                "Plazas",
                "Habitaciones",
                "Unidades"),
                mark = ".",
                digits = 0)
    )
    
    waiter_hide()
    
    output$downloadExcel <- downloadHandler(
        filename = function() {
            "puna.xlsx"
        },
        content = function(file) {
            writexl::write_xlsx(data_final(), file)
        }
    )
    
    output$downloadCSV <- downloadHandler(
        filename = function() {
            "puna.csv"
        },
        content = function(file) {
            write_csv(data_final(), file)
        }
    )
}
