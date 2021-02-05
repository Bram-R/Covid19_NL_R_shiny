Sys.setlocale("LC_TIME", "Dutch") # set to Dutch locale (to get Dutch month names) for this session
options(scipen = 999)

library("data.table")
library("rms")
library("forecast")
library("zoo")
library("tidyverse")
library("rjson")
library("pdftools")
library("magick")
library("shiny")
library("shinydashboard")

rm(list = ls())

source("f_data.R") # function to obtain and process (mainly NL) Covid-19 data 
source("f_pdf_rivm_test.R") # function to extract test data from RIVM report
# source("f_trend.R") # function to estimate and extrapolate trends over time 
source("f_figure.R") # function to make Figures based on (mainly NL) Covid-19 data 
source("f_server.R")

# source shiny graphical user interface
source("ui.R")

# generic
date_start <- as.Date("2020-06-01","%Y-%m-%d")

# obtain and process data
dat <- f_data() # obtain data
df <- f_data_man(dat = dat, date_start = date_start) # manipulate data

###### SAVE R ENVIRONMENT ######
save(dat, file = "Data/dat.RDATA") # raw data
save(df, file = "Data/df.RDATA") # processed data

##### Running the App #####
runApp(
  appDir = shinyApp(ui = ui, server = f_server),
  launch.browser = TRUE
)

##### Running the from GitHub #####
runGitHub("Covid19_NL_R_shiny", "Bram-R")
