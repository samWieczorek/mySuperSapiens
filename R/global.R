library(shiny)
library(shinydashboard)
library(highcharter)
library(xts)
library(shinyWidgets)
library(dplyr)


hcGopts <- getOption("highcharter.global")
hcGopts$useUTC <- FALSE
options(highcharter.global = hcGopts)




