function(input, output, session) {

    puna <- reactive({
        req(input$anio)
        if (input$anio == "Todos") {
        puna <- serie_puna    
        } else {
        puna <- serie_puna[serie_puna$anio %in% input$anio,  ]
        }
    })
    
    observeEvent(puna(), {
        if (input$anio == "Todos") {
            updateSelectInput(session, inputId = "ruta", 
                              choices = c("Todos",sort(unique(serie_puna$ruta_natural))),
                              selected = "Todos"
                              )
        } else {
            updateSelectInput(session, inputId = "ruta", 
                              choices = c("Todos",sort(unique(puna()$ruta_natural))),
                              selected = "Todos"
                              )
        }
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
        if (input$ruta == "Todos") {
            updateSelectInput(session, inputId = "region", 
                              choices = c("Todos",sort(unique(serie_puna$region))),
                              selected = "Todos")
        } else {
            updateSelectInput(session, inputId = "region", 
                          choices = c("Todos",sort(unique(ruta()$region))))
            }
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
        if (input$region == "Todos") {
            updateSelectInput(session, inputId = "provincia", 
                              choices = c("Todos",sort(unique(serie_puna$provincia))),
                              selected = "Todos")
        } else {
            updateSelectInput(session, inputId = "provincia", 
                              choices = c("Todos",sort(unique(region()$provincia))))
        }
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
        if (input$provincia == "Todos") {
            updateSelectInput(session, inputId = "depto", 
                              choices = c("Todos",sort(unique(serie_puna$departamento_partido))),
                              selected = "Todos")
        } else {
            updateSelectInput(session, inputId = "depto", 
                              choices = c("Todos",sort(unique(provincias()$departamento_partido))))
        }
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
        if (input$depto == "Todos") {
            updateSelectInput(session, inputId = "localidad", 
                              choices = c("Todos",sort(unique(serie_puna$localidad))),
                              selected = "Todos")
        } else {
            updateSelectInput(session, inputId = "localidad", 
                              choices = c("Todos",sort(unique(departamentos()$localidad))))
        }
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
        if (input$localidad == "Todos") {
            updateSelectInput(session, inputId = "clasificacion", 
                              choices = c("Todos",sort(unique(serie_puna$clasificacion_mintur))),
                              selected = "Todos")
        } else {
            updateSelectInput(session, inputId = "clasificacion", 
                              choices = c("Todos",sort(unique(localidades()$clasificacion_mintur))))
        }
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
        if (input$clasificacion == "Todos") {
            updateSelectInput(session, inputId = "tipo", 
                              choices = c("Todos",sort(unique(serie_puna$tipo))),
                              selected = "Todos")
        } else {
            updateSelectInput(session, inputId = "tipo", 
                              choices = c("Todos",sort(unique(clasificacion()$tipo))))
        }
    })
    
    tipo <- reactive({
        req(input$tipo)
        if (input$tipo == "Todos") {
            tipo <- clasificacion()
        } else {
            tipo <- clasificacion()[clasificacion()$tipo %in% input$tipo,  ]
        }
    })
    
    output$tabla <- DT::renderDataTable(
        server = F,
        DT::datatable(extensions = 'Buttons', 
                      options = list(dom = 'frtipB',
                                     buttons = list('copy', 
                                                    list(
                                                        extend = 'collection',
                                                        buttons = list(list(extend = 'csv', filename = "puna"),
                                                                       list(extend = 'excel', filename = "puna")),
                                                        text = 'Download'
                                                    ))),
                      tipo() %>% rename("Año" = anio,
                                        "Ruta natural" = ruta_natural,
                                        "Región" = region,
                                        "Provincia" = provincia,
                                        "Departamento/partido" = departamento_partido,
                                        "Localidad" = localidad,
                                        "Clasificación" = clasificacion_mintur,
                                        "Tipo" = tipo) %>%
                          group_by_at(.vars = c(input$agrup)) %>%
                          summarise("Establecimientos" = round(sum(establecimientos)),
                                    "Plazas" = round(sum(plazas))),
                      rownames= FALSE)
    )

}
