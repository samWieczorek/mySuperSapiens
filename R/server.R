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
  GetRawData <- reactive({
    highchart(type = "stock") %>%
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
          )
        )
      ) %>%
      hc_yAxis(
        min = min(supersapiens$glycemie) - 20, 
        max = max(supersapiens$glycemie) + 20, 
        title = list(text = "Glycémie (mg/dl)"),
        # plotLines = list(
        #   list(
        #     #label = list(text = "Limite hypoglycémie"),
        #     color = "#000000",
        #     width = 0.5,
        #     value = 70
        #   ),
        #   list(
        #     #label = list(text = "Limite hyperglycémie"),
        #     color = "#000000",
        #     width = 0.5,
        #     value = 250
        #   )
        # ),
        plotBands = NULL
      ) %>%
      hc_add_series(supersapiens$glycemie, type = "spline", color = 'blue') %>%
      hc_annotations(
        list(
          labelOptions = list(
            backgroundColor = 'rgba(255,255,255,0.6)',
            verticalAlign = 'top',
            y = 15
          ),
          
          labels = 
            
            apply(tags, 1, function(x){
              
              list(
                point = list(
                  xAxis = 0,
                  yAxis = 0,
                  x = datetime_to_timestamp(as.POSIXct(unname(x['startdate']), format="%Y-%m-%d %H:%M")),
                  y = 200
                ),
                text = unname(x['tag.description'])
              )
              
            })
        ))
  })
  
  output$showRawData <- renderHighchart({
   
    if(input$showzones) 
      hc <- GetRawData() %>% hc_yAxis(plotBands = viewZones())
    else
      GetRawData()
      
  })
  
  output$pga <- renderHighchart({
    view_pga()
  })
  
  output$meanPerDay_UI <- renderHighchart({
    
    hc <- supersapiens_meanPerDay %>% 
      hchart(
        'column', 
        hcaes(x = day, y = source)) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(Zones()[, 'Max'], colors = Zones()[, 'Color'])
        )
    hc
  })
  
  
  output$meanPerHour_UI <- renderHighchart({
    
    hc <- supersapiens_meanPerHour %>% 
      hchart(
        'column', 
        hcaes(x = 'hour', y = 'source')) %>%
      hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(Zones()[, 'Max'], colors = Zones()[, 'Color'])
        )
    
    
    hc
    
  })
  
  
  
  output$wholeRushes <- renderHighchart({
    hc <- highchart(type = "stock") %>%
      hc_add_series(supersapiens$rushes.pos, type = 'column', color = 'blue') %>%
      hc_add_series(supersapiens$rushes.neg, type = 'column', color = 'red') %>%
      #hc_add_series(glycemie$rushes.pos, type = 'areaspline', color = 'lightblue') %>%
      #hc_add_series(glycemie$rushes.neg, type = 'areaspline', color = 'orange') %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_xAxis(
        labels = list(format = '{value:%Y/%m/%d %H:%M}'),
        options = list(
          timezoneOffset = 2
        )
      )
    
    hc
    
  })
  
  
  
  output$variancePerDay_UI <- renderHighchart({
    
    colfunc <- colorRampPalette(c("#f95c75", "#653fc6"))
    colorseq <- c(70, 90, 140, 250, 300)
    
    
    hc <- supersapiens_variancePerDay %>% 
      hchart(
        'column', 
        hcaes(x = 'day', y = 'source')) %>%
      #hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(Zones()[, 'Max'],
          colors = Zones()[, 'Color']
        ))
    hc
  })
  
  
  
  output$variancePerHour_UI <- renderHighchart({
    
    colfunc <- colorRampPalette(c("#f95c75", "#653fc6"))
    colorseq <- c(70, 90, 140, 250, 300)
    
    
    hc <- supersapiens_variancePerHour %>% 
      hchart(
        'column', 
        hcaes(x = 'hour', y = 'source')) %>%
      #hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(Zones()[, 'Max'],
          colors = Zones()[, 'Color']
        ))
    hc
  })
  
  
  
  output$timeInGlucoseZones <- renderHighchart({
    
    
    per <- supersapiens_timeInZones$percentage

    #colnames(per) <- c('< 70 mg/dl', '70-90 mg/dl','90-140 mg/dl','> 140 mg/dl')
    n <- nrow(per) * ncol(per)
    df <- data.frame(
      day = NULL, 
      zone = NULL, 
      percentage = NULL)
    
    for (i in seq(nrow(per)))
      for (j in seq(ncol(per)))
        df <- rbind(df, c(rownames(per)[i], colnames(per)[j], 100*as.numeric(per[i, j])))
    
    
    df <- as.data.frame(df)
    colnames(df) <- c('day', 'zone', 'percentage')
    df[,'percentage'] <- as.numeric(df[,'percentage'])
    
    
    hc <- df %>% 
      hchart(
        'column', 
        hcaes(x = 'day', y = 'percentage', group = 'zone'),
        stacking = "normal"
      ) %>%
      hc_colors(Zones()[, 'Color']) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black')))
    
    
    hc
  })
  
  
  output$heatmapPerHour <- renderHighchart({
    
    hc <- highchart() %>%
      hc_add_series(data = supersapiens_heatmapPerHour, type = 'heatmap', 
        hcaes(x = hour, y = day, value = source), 
        name = "Median Price",
        dataLabels = list(enabled = FALSE)) %>%
      hc_colorAxis(
        dataClasses = color_classes(Zones()[, 'Max'],
          colors = Zones()[, 'Color']
        ))
    
    hc
    
  })
}

