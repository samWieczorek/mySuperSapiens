function(input, output, session) {
  
  test <- BuildData()
  
  
  
  
  output$showRawData <- renderHighchart({
   
    
    hc <-
      highchart(type = "stock") %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'lightgrey'))) %>%
      hc_add_dependency(name = "modules/annotations.js") %>%
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
        )
        # plotBands = list(
        #   list(
        #     #label = list(text = "xxxx"),
        #     color = "rgba(100, 0, 0, 0.1)",
        #     from = 70,
        #     to = 250
        #   )
        # )
        ) %>%
      hc_xAxis(
        labels = list(format = '{value:%d/%m/%Y %H:%M}'),
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
      hc_add_series(test$Glycemie, type = "spline") %>%
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
                  x = datetime_to_timestamp(as.POSIXct(unname(x['date']), format="%d-%m-%Y %H:%M")),
                  y = 200
                ),
                text = unname(x['source'])
              )
              
            })
       ))
    
      hc
  })
  
  
  output$meanPerDay <- renderHighchart({
    # data <- GetMeanPerDay()
    # hc <-
    #   highchart(type = "stock") %>%
    #   
  })
  
  
  output$variancePerDay <- renderHighchart({
  })
  
  
  output$timeInGlucoseZones <- renderHighchart({
  })
  
}

