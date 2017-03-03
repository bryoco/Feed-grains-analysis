library('dplyr')
library('ggplot2')
library("shiny")
library("stringr")


source("my_ui.R")
source("my_server.R")

shinyApp(ui = my.ui, server = server)
