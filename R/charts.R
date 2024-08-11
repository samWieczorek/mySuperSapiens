#' @title Profil Glucidique Ambulatoire
#' @export
view_pga <- function(pga){
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
}