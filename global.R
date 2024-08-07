library(shiny)
library(shinydashboard)
library(highcharter)
library(xts)

hcGopts <- getOption("highcharter.global")
hcGopts$useUTC <- FALSE
options(highcharter.global = hcGopts)


zones <- data.frame(
  Min = c(0, 70, 90, 140, 250),
  Max = c(70, 90, 140, 250, 300),
  Info = c('Hypoglycemie',
    'Etat adaptatif', 
    'Recharge glucidique optimale', 
    'Etat d’inflammation latent', 
    'Hyperglycemie'),
  Color = c("#273b76", "#6832c0", "#567df2", "#c281c8", "#fd5878")
)
rownames(zones) <- LETTERS[seq(5)]

viewzones <- list(
  list(
    #label = list(text = "xxxx"),
    color = zones['A', 'Color'],
    from = zones['A', 'Min'],
    to = zones['A', 'Max']
  ),
  list(
    #label = list(text = "xxxx"),
    color = zones['B', 'Color'],
    from = zones['B', 'Min'],
    to = zones['B', 'Max']
  ),
  list(
    #label = list(text = "xxxx"),
    color = zones['C', 'Color'],
    from = zones['C', 'Min'],
    to = zones['C', 'Max']
  ),
  list(
    #label = list(text = "xxxx"),
    color = zones['D', 'Color'],
    from = zones['D', 'Min'],
    to = zones['D', 'Max']
  ),
  list(
    #label = list(text = "xxxx"),
    color = zones['E', 'Color'],
    from = zones['E', 'Min'],
    to = zones['E', 'Max']
  )
)