#' @title Profil Glucidique Ambulatoire
#' @export
view_hc_pga <- function(df){
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
view_timeInGlucoseZones <- function(df, hr){
  
  df <- df$percentage
  per <- apply(df, 2, function(x) as.numeric(100 * x))
  rownames(per) <- xts::dimnames.xts(df)[[1]]
  per <- as.data.frame(per)

  hr <- hr / 60
  
  
  hc <- highchart() %>% 
    hc_chart(type = "column") %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    #hc_xAxis(categories = rownames(per)) %>%
    hc_yAxis_multiples(
      list(title = list(text = "Glycemie (% de temps)"), opposite = FALSE),
      list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Zones d'entrainement (duree en minutes)"))) %>%
    hc_add_series(name = Zones()['E', 'Info'],
      data = per$E,
      stack = "glucose",
      yAxis = 0,
      color = Zones()['E', 'Color']) %>%
    hc_add_series(name = Zones()['D', 'Info'],
      data = per$D,
      stack = "glucose",
      yAxis = 0,
      color = Zones()['D', 'Color']) %>%
    hc_add_series(name = Zones()['C', 'Info'],
      data = per$C,
      stack = "glucose",
      yAxis = 0,
      color = Zones()['C', 'Color']) %>%
    hc_add_series(name = Zones()['B', 'Info'],
      data = per$B,
      stack = "glucose",
      yAxis = 0,
      color = Zones()['B', 'Color']) %>%
    hc_add_series(name = Zones()['A', 'Info'],
      data = per$A,
      stack = "glucose",
      yAxis = 0,
      color = Zones()['A', 'Color']) %>%
    
    hc_add_series(name = zones_HR()['E', 'Info'],
      data = hr$E,
      stack = "HR",
      yAxis = 1,
      color = zones_HR()['E', 'Color']) %>%
    hc_add_series(name = zones_HR()['D', 'Info'],
      data = hr$D,
      stack = "HR",
      yAxis = 1,
      color = zones_HR()['D', 'Color'])%>%
    hc_add_series(name = zones_HR()['C', 'Info'],
      data = hr$C,
      stack = "HR",
      yAxis = 1,
      color = zones_HR()['C', 'Color']) %>%
    hc_add_series(name = zones_HR()['B', 'Info'],
      data = hr$B,
      stack = "HR",
      yAxis = 1,
      color = zones_HR()['B', 'Color']) %>%
    hc_add_series(name = zones_HR()['A', 'Info'],
      data = hr$A,
      stack = "HR",
      yAxis = 1,
      color = zones_HR()['A', 'Color']) %>%

    hc_add_theme(hc_theme_ft())
  
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
Build_hc_RawData <- function(df, fit){

 
  hc <- highchart(type = "stock") %>%
    hc_add_series(df$glycemie, type = "spline", color = 'blue') %>%
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

  
  showHR <- TRUE
  showAlt <- TRUE
  
  for(i in seq(length(fit))){
    if (showHR)
      hc <- hc %>% 
        hc_add_series(fit[[i]]$heart.rate, 
          type = "spline", 
          color = 'red',
          yAxis = 1)
    
    if (showAlt)
      for(i in seq(length(fit)))
        hc <- hc %>% 
          hc_add_series(fit[[i]]$enhanced.altitude, 
            type = "spline", 
            color = 'grey',
            yAxis = 2)
  }
  
  hc
}



#' @export
AddAnnotations <- function(hc, df.tags){
  hc <- hc %>% 
    hc_annotations(
      list(
      
        labelOptions = list(
          backgroundColor = 'black',
          verticalAlign = 'top',
          y = 15
          #shape = "connector"
          #align = "right",
          #justify = FALSE,
          #crop = TRUE
          #type= 'crookedLine'
          
          # style = list(
          #   fontSize = "0.8em",
          #   textOutline = "1px white"
          # )
        ),
        
        labels = lapply(index(df.tags), function(x){
          list(
            point = list(
              xAxis = 0,
              yAxis = 0,
              x = datetime_to_timestamp(x),
              y = 50
            ),
            text = as.character(df.tags[x]$tags)
          )
        })
      ))
  
  hc
  
}