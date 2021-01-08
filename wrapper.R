Sys.setlocale("LC_TIME", "Dutch") # set to Dutch locale (to get Dutch month names) for this session
options(scipen = 999)
packages <- c("data.table", "rms", "forecast", "zoo", "tidyverse", "rjson", "pdftools")
suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE)) # load packages
rm(list = ls())

source("f_data.R") # function to obtain and manipulate NL Covid-19 data 
source("f_pdf_rivm_test.R") # function to extract test data from RIVM report
source("f_trend.R") # function to estimate and extrapolate trends over time 
source("f_figure.R") # function to make Figures based on NL Covid-19 data 

# generic
date_start <- as.Date("2020-6-1") #as.Date("2020-3-15") #minimal 2 days after RIVM data starts 
lbls <- format(seq(date_start, Sys.Date() + 30, by = "4 week"), "%e %b")

# obtain and manipulate data
dat <- f_data() # obtain data
df <- f_data_man(dat = dat, date_start = date_start) # manipulate data

# make figures (stored in Figures folder)
f_figure(df = df, date_start = date_start)

###### SAVE R ENVIRONMENT ######
save.image(file = "Figures/COVID19.RData") 
