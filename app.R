###### Setup ######
rm(list = ls())

# load packages
library("tidyverse")
library("rjson")
library("pdftools")
library("data.table")
library("rms")
library("zoo")
library("shiny")
library("shinydashboard")

# load functions and shinyApp server + graphical user interface
source("f_data.R") # function to obtain and process (mainly NL) Covid-19 data 
source("f_pdf_rivm_test.R") # function to extract test data from RIVM report
source("f_figure.R") # function to make Figures based on (mainly NL) Covid-19 data 
source("f_server.R")
source("ui.R") # source shiny graphical user interface

##### Running the App #####
shinyApp(ui = ui, server = f_server)

##### RUNNING FROM GitHub #####
# shiny::runGitHub("Covid19_NL_R_shiny", "Bram-R")

##### DEPLOY APP TO Shinyapps.io #####
# library(rsconnect)
# rsconnect::deployApp(getwd())
