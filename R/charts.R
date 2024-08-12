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
  hc_yAxis(
    min = min(df[,2:6]), 
    max = max(df[,2:6]), 
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
}


#' @title View variance per hour 
#' @export
view_VariancePerHour <- function(df){
  colfunc <- colorRampPalette(c("#f95c75", "#653fc6"))
  colorseq <- c(70, 90, 140, 250, 300)
  
  hc <- df %>% 
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
    hcaes(x = hour, y = day, value = source), 
    name = "Median Price",
    dataLabels = list(enabled = FALSE)) %>%
  hc_colorAxis(
    dataClasses = color_classes(Zones()[, 'Max'],
      colors = Zones()[, 'Color']
    ))

hc
}