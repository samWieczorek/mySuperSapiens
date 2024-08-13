#' @title Profil Glucidique Ambulatoire
#' @export
view_pga <- function(df){
  hc <- highchart() %>%
  hc_add_series(
    df,
    type = "spline",
    hcaes(x = hour, y = Q50),
    name = "Quantiles",
    id = "fit", ## this is for link the arearange series to this one and have one legend
    lineWidth = 3,
    color = '#085792',
    showInLegend = TRUE
  ) %>% 
  hc_add_series(
    df,
    type = "arearange",
    name = "Q25_75",
    hcaes(x = hour, low = Q25, high = Q75),
    linkedTo = "fit", ## here we link the legends in one.
    showInLegend = TRUE,
    color = "#a2b5d4",  ## put a semi transparent color
    zIndex = -3 ## this is for put the series in a back so the points are showed first
  ) %>% 
  hc_add_series(
    df,
    type = "arearange",
    name = "Q5_95",
    hcaes(x = hour, low = Q5, high = Q95),
    linkedTo = "fit", ## here we link the legends in one.
    showInLegend = TRUE,
    color = "#dfe3ed",  ## put a semi transparent color
    zIndex = -3 ## this is for put the series in a back so the points are showed first
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
}


#' @title View Mean per Day 
#' @export
view_MeanPerDay <- function(df){
  hc <- df %>% 
    hchart(
      'column', 
      hcaes(x = day, y = glycemie)) %>%
    hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
    hc_colorAxis(
      dataClasses = color_classes(Zones()[, 'Max'], colors = Zones()[, 'Color'])
    )
  hc
}


#' @title View Mean per Hour 
#' @export
view_MeanPerHour <- function(df){
hc <- df %>% 
  hchart('column', 
    hcaes(x = hour, y = glycemie)) %>%
  hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
  hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
  hc_colorAxis(
    dataClasses = color_classes(Zones()[, 'Max'], colors = Zones()[, 'Color'])
  )
hc
}



#' @title View variance per day 
#' @export
view_VariancePerDay <- function(df){
  colfunc <- colorRampPalette(c("#f95c75", "#653fc6"))
  colorseq <- c(70, 90, 140, 250, 300)
  
  
  hc <- df %>% 
    hchart('column', 
      hcaes(x = day, y = glycemie)) %>%
    #hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
    hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
    hc_colorAxis(
      dataClasses = color_classes(Zones()[, 'Max'],
        colors = Zones()[, 'Color']
      ))
  hc
}


#' @title View variance per hour 
#' @export
view_VariancePerHour <- function(df){
  colfunc <- colorRampPalette(c("#f95c75", "#653fc6"))
  colorseq <- c(70, 90, 140, 250, 300)
  
  hc <- df %>% 
    hchart(
      'column', 
      hcaes(x = hour, y = glycemie)) %>%
    #hc_colors(c("#ff384C", "#006AFE", "#3f9AFE", "#6301AD")) %>%
    hc_add_theme(hc_theme(chart = list(backgroundColor = 'black'))) %>%
    hc_colorAxis(
      dataClasses = color_classes(Zones()[, 'Max'],
        colors = Zones()[, 'Color']
      ))
  hc
}


#' @title View time spent in glucose zones 
#' @export
view_timeInGlucoseZones <- function(df){
  
  per <- df$percentage
  
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
}

#' @title View heatmap per hour 
#' @export
view_heatmapPerHour <- function(df){
hc <- highchart() %>%
  hc_add_series(data = df, type = 'heatmap', 
    hcaes(x = hour, y = day, value = glycemie), 
    name = "Median Price",
    dataLabels = list(enabled = FALSE)) %>%
  hc_colorAxis(
    dataClasses = color_classes(Zones()[, 'Max'],
      colors = Zones()[, 'Color']
    ))

hc
}


#' @title View whole rushes 
#' @export
view_wholeRushes <- function(df){
hc <- highchart(type = "stock") %>%
  hc_add_series(df$rushes.pos, type = 'column', color = 'blue') %>%
  hc_add_series(df$rushes.neg, type = 'column', color = 'red') %>%
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
}


#' @title View raw data 
#' @export
Build_hc_RawData <- function(){

 
  hc <- highchart(type = "stock") %>%
    hc_add_series(supersapiens$glycemie, type = "spline", color = 'blue') %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = 'lightgrey'))) %>%
      hc_add_dependency(name = "modules/annotations.js") %>%
    hc_yAxis_multiples(
      list(title = list(text = "Glycemie"), opposite = FALSE),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Hear rate")),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Altitude"))) %>%
    
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
      ) 
      # hc_yAxis(
      #   min = min(as.numeric(df$glycemie)) - 20,
      #   max = max(as.numeric(df$glycemie)) + 20,
      #   title = list(text = "Glycémie (mg/dl)"),
      #   # plotLines = list(
      #   #   list(
      #   #     #label = list(text = "Limite hypoglycémie"),
      #   #     color = "#000000",
      #   #     width = 0.5,
      #   #     value = 70
      #   #   ),
      #   #   list(
      #   #     #label = list(text = "Limite hyperglycémie"),
      #   #     color = "#000000",
      #   #     width = 0.5,
      #   #     value = 250
      #   #   )
      #   # ),
      #   plotBands = NULL
      # ) 

  #     # hc_annotations(
  #     #   list(
  #     #     labelOptions = list(
  #     #       backgroundColor = 'rgba(255,255,255,0.6)',
  #     #       verticalAlign = 'top',
  #     #       y = 15
  #     #     ),
  #     #     
  #     #     labels = 
  #     #       
  #     #       apply(df$tags, 1, function(x){
  #     #         
  #     #         list(
  #     #           point = list(
  #     #             xAxis = 0,
  #     #             yAxis = 0,
  #     #             x = datetime_to_timestamp(as.POSIXct(unname(x['date']), format="%Y-%m-%d %H:%M")),
  #     #             y = 200
  #     #           ),
  #     #           text = unname(x['tag.description'])
  #     #         )
  #     #         
  #     #       })
  #     #   ))
  
  
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
}