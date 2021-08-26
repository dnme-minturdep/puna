function(input, output, session) {

    puna <- reactive(
        serie_puna[serie_puna$anio %in% input$anio,  ]
    )
    
    observeEvent(puna(), {
        updatePickerInput(session, inputId = "ruta", 
                          choices = sort(unique(puna()$ruta_natural)),
                          selected = sort(unique(puna()$ruta_natural)))
    })
    
    ruta <- reactive({
        ruta <- puna()[puna()$ruta_natural %in% input$ruta,  ]
    })
    
    observeEvent(ruta(), {
        updatePickerInput(session, inputId = "region", 
                          choices = sort(unique(ruta()$region)),
                          selected = sort(unique(ruta()$region)))
    })
    
    region <- reactive({
        region <- ruta()[ruta()$region %in% input$region,  ]
    })
    
    observeEvent(region(), {
        updatePickerInput(session, inputId = "provincia", 
                          choices = sort(unique(region()$provincia)),
                          selected = sort(unique(region()$provincia)))
    })
    
    provincias <- reactive({
        provincias <- region()[region()$provincia %in% input$provincia,  ]
    })
    
    observeEvent(provincias(), {
        updatePickerInput(session, inputId = "depto", 
                          choices = sort(unique(provincias()$departamento_partido)),
                          selected = sort(unique(provincias()$departamento_partido)))
    })  
    
    departamentos <- reactive({
        departamentos <- provincias()[provincias()$departamento_partido %in% input$depto,  ]
    })
    
    observeEvent(departamentos(), {
        updatePickerInput(session, inputId = "localidad", 
                          choices = sort(unique(departamentos()$localidad)),
                          selected = sort(unique(departamentos()$localidad)))
    })
    
    localidades <- reactive({
        localidades <- departamentos()[departamentos()$localidad %in% input$localidad,  ]
    })
    
    observeEvent(localidades(), {
        updatePickerInput(session, inputId = "clasificacion", 
                          choices = sort(unique(localidades()$clasificacion_mintur)),
                          selected = sort(unique(localidades()$clasificacion_mintur)))
    })
    
    clasificacion <- reactive({
        clasificacion <- localidades()[localidades()$clasificacion_mintur %in% input$clasificacion,  ]
    })
    
    observeEvent(clasificacion(), {
        updatePickerInput(session, inputId = "tipo", 
                          choices = sort(unique(clasificacion()$tipo)),
                          selected = sort(unique(clasificacion()$tipo)))
    })
    
    tipo <- reactive({
        tipo <- clasificacion()[clasificacion()$tipo %in% input$tipo,  ]
    })
    
    output$tabla <- DT::renderDataTable(server = F,
                                        DT::datatable(
                                            extensions = 'Buttons', options = list(
                                                dom = 'frtipB',
                                                buttons =
                                                    list('copy', list(
                                                        extend = 'collection',
                                                        buttons = list(list(extend = 'csv', filename = "puna"),
                                                                       list(extend = 'excel', filename = "puna")),
                                                        text = 'Download'
                                                    ))
                                            ),
                                            
                                            tipo() %>%
                                                rename("Año" = anio,
                                                       "Ruta natural" = ruta_natural,
                                                       "Región" = region,
                                                       "Provincia" = provincia,
                                                       "Departamento/partido" = departamento_partido,
                                                       "Localidad" = localidad,
                                                       "Clasificación" = clasificacion_mintur,
                                                       "Tipo" = tipo
                                                ) %>%
                                                group_by_at(.vars = c(input$agrup)) %>%
                                                summarise("Establecimientos" = round(sum(establecimientos)),
                                                          "Plazas" = round(sum(plazas))),
                                            rownames= FALSE
                                        )
    )

}
