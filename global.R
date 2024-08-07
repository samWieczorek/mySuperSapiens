library(shiny)
library(shinydashboard)
library(highcharter)
library(xts)

hcGopts <- getOption("highcharter.global")
hcGopts$useUTC <- FALSE
options(highcharter.global = hcGopts)


zones <- data.frame(
  Min = c(0, 70, 91, 141),
  Max = c(70, 90, 140, 300),
  Info = c('Recuperation ralentie', 'Etat adaptatif', 'Recharge glucidique optimale', 'Etat d’inflammation latent')
)
rownames(zones) <- LETTERS[seq(4)]