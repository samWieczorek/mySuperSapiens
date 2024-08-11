function(input, output, session) {
  
 
  data(glycemie)
  
  output$zones_UI <- renderUI({
    tagList(
      # All your styles will go here
     # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
      #tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
      #tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
      #tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: red}")),
      #tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: green}")),
      
      wellPanel(
        style = paste0('background: ', zones['A', 'Color'], ';'),
        sliderInput('zoneA', "Zone A",
        min = zones['A', 'Min'],
        max = zones['E', 'Max'],
        value = zones['A', c('Min','Max')])
        ),
      wellPanel(
        style = paste0('background: ', zones['B', 'Color'], ';'),
        sliderInput('zoneB', "Zone B",
        min = zones['A', 'Min'], 
        max = zones['E', 'Max'],
        value = zones['B', c('Min','Max')])
        ),
      wellPanel(
        style = paste0('background: ', zones['C', 'Color'], ';'),
        sliderInput('zoneC', "Zone C",
        min = zones['A', 'Min'], 
        max = zones['E', 'Max'],
        value = zones['C', c('Min','Max')])
        ),
      wellPanel(
        style =paste0('background: ', zones['D', 'Color'], ';'),
        sliderInput('zoneD', "Zone D",
        min = zones['A', 'Min'], 
        max = zones['E', 'Max'],
        value = zones['D', c('Min','Max')])
        ),
        wellPanel(
          style = paste0('background: ', zones['E', 'Color'], ';'),
          sliderInput('zoneE', "Zone E",
        min = zones['A', 'Min'], 
        max = zones['E', 'Max'],
        value = zones['E', c('Min','Max')])
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
      color = zones['A', 'Color'],
      from = input$zoneA[1],
      to = input$zoneA[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = zones['B', 'Color'],
      from = input$zoneB[1],
      to = input$zoneB[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = zones['C', 'Color'],
      from = input$zoneC[1],
      to = input$zoneC[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = zones['D', 'Color'],
      from = input$zoneD[1],
      to = input$zoneD[2]
    ),
    list(
      #label = list(text = "xxxx"),
      color = zones['E', 'Color'],
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
        min = min(glycemie) - 20, 
        max = max(glycemie) + 20, 
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
      hc_add_series(glycemie$source, type = "spline", color = 'blue') %>%
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
                text = unname(x['source'])
              )
              
            })
        ))
  })
  
  output$showRawData <- renderHighchart({
   
    if(input$showzones) 
      hc <- GetRawData() %>% hc_yAxis(plotBands = viewzones())
    else
      GetRawData()
      
  })
  
  output$pga <- renderHighchart({
    hc <- highchart() %>%
      hc_add_series(
        pga,
        type = "spline",
        hcaes(x = hour, y = Q50),
        name = "Quantiles",
        id = "fit", ## this is for link the arearange series to this one and have one legend
        lineWidth = 3,
        color = '#085792',
        showInLegend = TRUE
      ) %>% 
      hc_add_series(
        pga,
        type = "arearange",
        name = "Q25_75",
        hcaes(x = hour, low = Q25, high = Q75),
        linkedTo = "fit", ## here we link the legends in one.
        showInLegend = TRUE,
        color = "#a2b5d4",  ## put a semi transparent color
        zIndex = -3 ## this is for put the series in a back so the points are showed first
      ) %>% 
      hc_add_series(
        pga,
        type = "arearange",
        name = "Q5_95",
        hcaes(x = hour, low = Q5, high = Q95),
        linkedTo = "fit", ## here we link the legends in one.
        showInLegend = TRUE,
        color = "#dfe3ed",  ## put a semi transparent color
        zIndex = -3 ## this is for put the series in a back so the points are showed first
      ) %>%
      hc_yAxis(
        min = min(pga[,2:6]), 
        max = max(pga[,2:6]), 
        title = list(text = "Glycémie (mg/dl)"),
        plotBands = viewzones()
      ) %>%
      hc_plotOptions(
        series = list(
          marker = list(
            enabled = FALSE
          )
        )
      ) %>%
      hc_xAxis(
        labels = list(format = '{value:%H:%M}')
      )
        

    hc
  })
  
  output$meanPerDay_UI <- renderHighchart({
    
    hc <- meanPerDay %>% 
      hchart(
        'column', 
        hcaes(x = day, y = source)) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(zones[, 'Max'], colors = zones[, 'Color'])
        )
    hc
  })
  
  
  output$meanPerHour_UI <- renderHighchart({
    
    hc <- meanPerHour %>% 
      hchart(
        'column', 
        hcaes(x = 'hour', y = 'source')) %>%
      hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(zones[, 'Max'], colors = zones[, 'Color'])
        )
    
    
    hc
    
  })
  
  
  
  output$wholeRushes <- renderHighchart({
    hc <- highchart(type = "stock") %>%
      hc_add_series(glycemie$rushes.pos, type = 'column', color = 'blue') %>%
      hc_add_series(glycemie$rushes.neg, type = 'column', color = 'red') %>%
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
    
    
    hc <- variancePerDay %>% 
      hchart(
        'column', 
        hcaes(x = 'day', y = 'source')) %>%
      #hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(zones[, 'Max'],
          colors = zones[, 'Color']
        ))
    hc
  })
  
  
  
  output$variancePerHour_UI <- renderHighchart({
    
    colfunc <- colorRampPalette(c("#f95c75", "#653fc6"))
    colorseq <- c(70, 90, 140, 250, 300)
    
    
    hc <- variancePerHour %>% 
      hchart(
        'column', 
        hcaes(x = 'hour', y = 'source')) %>%
      #hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
      hc_colorAxis(
        dataClasses = color_classes(zones[, 'Max'],
          colors = zones[, 'Color']
        ))
    hc
  })
  
  
  
  output$timeInGlucoseZones <- renderHighchart({
    
    
    per <- timeInZones$percentage

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
      hc_colors(zones[, 'Color']) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'black')))
    
    
    hc
  })
  
  
  output$heatmapPerHour <- renderHighchart({
    
    hc <- highchart() %>%
      hc_add_series(data = heatmapPerHour, type = 'heatmap', 
        hcaes(x = hour, y = day, value = source), 
        name = "Median Price",
        dataLabels = list(enabled = FALSE)) %>%
      hc_colorAxis(
        dataClasses = color_classes(zones[, 'Max'],
          colors = zones[, 'Color']
        ))
    
    hc
    
  })
}

