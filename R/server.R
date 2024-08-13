function(input, output, session) {
  
 
  data(supersapiens)
   
  output$zones_UI <- renderUI({
    tagList(
      # All your styles will go here
     # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
      #tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
      #tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
      #tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: red}")),
      #tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: green}")),
      
      wellPanel(
        style = paste0('background: ', Zones()['A', 'Color'], ';'),
        sliderInput('zoneA', "Zone A",
        min = Zones()['A', 'Min'],
        max = Zones()['E', 'Max'],
        value = Zones()['A', c('Min','Max')])
        ),
      wellPanel(
        style = paste0('background: ', Zones()['B', 'Color'], ';'),
        sliderInput('zoneB', "Zone B",
        min = Zones()['A', 'Min'], 
        max = Zones()['E', 'Max'],
        value = Zones()['B', c('Min','Max')])
        ),
      wellPanel(
        style = paste0('background: ', Zones()['C', 'Color'], ';'),
        sliderInput('zoneC', "Zone C",
        min = Zones()['A', 'Min'], 
        max = Zones()['E', 'Max'],
        value = Zones()['C', c('Min','Max')])
        ),
      wellPanel(
        style =paste0('background: ', Zones()['D', 'Color'], ';'),
        sliderInput('zoneD', "Zone D",
        min = Zones()['A', 'Min'], 
        max = Zones()['E', 'Max'],
        value = Zones()['D', c('Min','Max')])
        ),
        wellPanel(
          style = paste0('background: ', Zones()['E', 'Color'], ';'),
          sliderInput('zoneE', "Zone E",
        min = Zones()['A', 'Min'], 
        max = Zones()['E', 'Max'],
        value = Zones()['E', c('Min','Max')])
        )
      )
  })
  
  
  observe({
    updateSliderInput(session, 'zoneB', value = c(input$zoneA[2], input$zoneB[2]))
    updateSliderInput(session, 'zoneC', value = c(input$zoneB[2], input$zoneC[2]))
    updateSliderInput(session, 'zoneD', value = c(input$zoneC[2], input$zoneD[2]))
    updateSliderInput(session, 'zoneE', value = c(input$zoneD[2], input$zoneE[2]))
 
  })
  
  
  
  viewzones <- reactive({
    list(
    list(
      #label = list(text = "xxxx"),
      color = Zones()['A', 'Color'],
      from = input$zoneA[1],
      to = input$zoneA[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = Zones()['B', 'Color'],
      from = input$zoneB[1],
      to = input$zoneB[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = Zones()['C', 'Color'],
      from = input$zoneC[1],
      to = input$zoneC[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = Zones()['D', 'Color'],
      from = input$zoneD[1],
      to = input$zoneD[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = Zones()['E', 'Color'],
      from = input$zoneE[1],
      to = input$zoneE[2]
    )
  )
})

  output$showRawData <- renderHighchart({
    hc <- view_RawData(supersapiens$glycemie) 
    
    if(input$showzones) 
     hc <- hc %>% hc_yAxis(plotBands = viewzones())
    showHR <- TRUE
    showAlt <- TRUE
    
      for(i in seq(length(supersapiens_fit))){
        if (showHR)
          hc <- hc %>% 
            hc_add_series(supersapiens_fit[[i]]$heart.rate, 
              type = "spline", 
              color = 'red',
              yAxis = 1)
        
        if (showAlt)
          for(i in seq(length(supersapiens_fit)))
            hc <- hc %>% 
              hc_add_series(supersapiens_fit[[i]]$enhanced.altitude, 
                type = "spline", 
                color = 'grey',
                yAxis = 2)
        }


    hc
    
  })
  
  output$pga <- renderHighchart({
    view_pga(supersapiens_pga) %>%
      hc_yAxis(
        min = min(supersapiens_pga[,2:6]), 
        max = max(supersapiens_pga[,2:6]), 
        title = list(text = "Glycémie (mg/dl)"),
        plotBands = viewzones()
      )
  })
  
  output$meanPerDay_UI <- renderHighchart({
    view_MeanPerDay(supersapiens_meanPerDay)
  })
  
  
  output$meanPerHour_UI <- renderHighchart({
    view_MeanPerHour(supersapiens_meanPerHour)
  })
  

  output$wholeRushes <- renderHighchart({
    view_wholeRushes(supersapiens)
  })
  
  output$variancePerDay_UI <- renderHighchart({
    view_VariancePerDay(supersapiens_variancePerDay)
  })
  
  output$variancePerHour_UI <- renderHighchart({
    view_VariancePerHour(supersapiens_variancePerHour)
  })

  output$timeInGlucoseZones <- renderHighchart({
    view_timeInGlucoseZones(supersapiens_timeInZones)
  })
  
  
  output$heatmapPerHour <- renderHighchart({
    view_heatmapPerHour(supersapiens_heatmapPerHour)
  })
  
  
  # output$chooseFitFile <- renderUI({
  #   path <- system.file('extdata/Fit', package = 'mySuperSapiens')
  #   
  #   ll.fit <- list.files(path)
  #   ll.fit <- ll.fit[grepl('-record.csv', ll.fit)]
  #   
  #   selectInput('filerecord', 'FIT record',
  #     choices = ll.fit)
  # })
  

  show <- function(df){
  
  highchart(type = "stock") %>%
    hc_add_series(df, type = "spline", color = 'blue') %>%
    hc_add_theme(hc_theme(chart = list(backgroundColor = 'lightgrey'))) %>%
    hc_add_dependency(name = "modules/annotations.js") %>%
    
    hc_xAxis(
      labels = list(format = '{value:%Y/%m/%d %H:%M}'),
      options = list(
        timezoneOffset = 2
      ),
      plotBands = list(
        list(
          label = list(text = "2024-08-02"),
          color = "rgba(100, 0, 0, 0.05)",
          from = datetime_to_timestamp(as.Date("2024-08-02", tz = "Europe/Paris")),
          to = datetime_to_timestamp(as.Date("2024-08-03", tz = "Europe/Paris"))
        ),
        list(
          label = list(text = "2024-08-04"),
          color = "rgba(100, 0, 0, 0.05)",
          from = datetime_to_timestamp(as.Date("2024-08-04", tz = "Europe/Paris")),
          to = datetime_to_timestamp(as.Date("2024-08-05", tz = "Europe/Paris"))
        ),
        list(
          label = list(text = "2024-08-06"),
          color = "rgba(100, 0, 0, 0.05)",
          from = datetime_to_timestamp(as.Date("2024-08-06", tz = "Europe/Paris")),
          to = datetime_to_timestamp(as.Date("2024-08-07", tz = "Europe/Paris"))
        ),
        list(
          label = list(text = "2024-08-08"),
          color = "rgba(100, 0, 0, 0.05)",
          from = datetime_to_timestamp(as.Date("2024-08-08", tz = "Europe/Paris")),
          to = datetime_to_timestamp(as.Date("2024-08-09", tz = "Europe/Paris"))
        ),
        list(
          label = list(text = "2024-08-10"),
          color = "rgba(100, 0, 0, 0.05)",
          from = datetime_to_timestamp(as.Date("2024-08-10", tz = "Europe/Paris")),
          to = datetime_to_timestamp(as.Date("2024-08-11", tz = "Europe/Paris"))
        ),
        list(
          label = list(text = "2024-08-12"),
          color = "rgba(100, 0, 0, 0.05)",
          from = datetime_to_timestamp(as.Date("2024-08-12", tz = "Europe/Paris")),
          to = datetime_to_timestamp(as.Date("2024-08-13", tz = "Europe/Paris"))
        ),
        list(
          label = list(text = "2024-08-14"),
          color = "rgba(100, 0, 0, 0.05)",
          from = datetime_to_timestamp(as.Date("2024-08-14", tz = "Europe/Paris")),
          to = datetime_to_timestamp(as.Date("2024-08-15", tz = "Europe/Paris"))
        )
      )
    ) %>%
    hc_yAxis(
      min = min(as.numeric(df$glycemie)) - 20,
      max = max(as.numeric(df$glycemie)) + 20,
      title = list(text = "Glycémie (mg/dl)"),
     plotBands = NULL
    )
  }
  
}

