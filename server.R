function(input, output, session) {
  
  test <- BuildData()
  tags <- BuildData()$Tags
  
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
        min = min(test$Glycemie) - 20, 
        max = max(test$Glycemie) + 20, 
        title = list(text = "Glycémie (mg/dl)"),
        plotLines = list(
          list(
            #label = list(text = "Limite hypoglycémie"),
            color = "#000000",
            width = 0.5,
            value = 70
          ),
          list(
            #label = list(text = "Limite hyperglycémie"),
            color = "#000000",
            width = 0.5,
            value = 250
          )
        ),
        plotBands = NULL
      ) %>%
      hc_add_series(test$Glycemie, type = "spline", color = 'blue') %>%
      hc_colorAxis(minColor = "#FFFFFF", maxColor = "#434348",
        type = "logarithmic") %>%
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
                  x = datetime_to_timestamp(as.POSIXct(unname(x['date']), format="%Y-%m-%d %H:%M")),
                  y = 200
                ),
                text = unname(x['source'])
              )
              
            })
        ))
  })
  
  output$showRawData <- renderHighchart({
   
    if(input$showzones) 
      hc <- GetRawData() %>% hc_yAxis(plotBands = viewzones)
    else
      GetRawData()
      
  })
  
  
  output$meanPerDay <- renderHighchart({
    df <- GetMeanPerDay(titi)
    df[,'source'] <- round(df[,'source'], 2)
    
    hc <- df %>% 
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
  
  
  output$variancePerDay <- renderHighchart({
    
    df <- GetVariancePerDay(test$Glycemie)
    df[,'source'] <- round(df[,'source'], 2)
    
    colfunc <- colorRampPalette(c("#f95c75", "#653fc6"))
    colorseq <- c(70, 90, 140, 250, 300)
    
    
    hc <- df %>% 
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
  
  
  output$timeInGlucoseZones <- renderHighchart({
    
    
    per <- GetTimeInGlucoseZones(test$Glycemie)$percentage

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
    
    df <- zoo::fortify.zoo(test$Glycemie)
    df$hour <- format(df$Index, "%H")
    df$day <- format(df$Index, "%Y-%m-%d")
    res2 <- aggregate(source~ day + hour, df, mean)
    res2[, 'source'] <- round(res2[, 'source'], 1)
    res2 <- cbind(res2, zone = unlist(lapply(res2[, 'source'], function(x) GetZone(x))))
    
    
    hc <- highchart() %>%
      hc_add_series(data = res2, type = 'heatmap', 
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

