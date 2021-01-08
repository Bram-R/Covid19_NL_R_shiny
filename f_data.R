f_data <- function(date_start = as.Date("2020-6-1")) {

  ###### RETRIEVE AND COMBINE INPUT DATA ######
  # RIVM
  dat_RIVM <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv") 
  dat_RIVM_R <- fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json", simplify = TRUE)
  dat_RIVM_test <- f_pdf_rivm_test("https://www.rivm.nl/sites/default/files/2021-01/COVID-19_WebSite_rapport_wekelijks_20210105_1254.pdf")
  # https://www.rivm.nl/coronavirus-covid-19/actueel/wekelijkse-update-epidemiologische-situatie-covid-19-in-nederland
  # vergelijk dat_RIVM_test met RIVM rapport
  dat_RIVM_nursery <- fread("https://data.rivm.nl/covid-19/COVID-19_verpleeghuizen.csv") 
  
  # NICE
  dat_NICE_IC_C <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative", simplify = TRUE)
  dat_NICE_IC_I <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/", simplify = TRUE)
  dat_NICE_IC_B <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/", simplify = TRUE)
  dat_NICE_Hosp_C <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/", simplify = TRUE)
  dat_NICE_Hosp_I <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/", simplify = TRUE)
  dat_NICE_Hosp_B <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/", simplify = TRUE)
  
  # LCPS
  dat_LCPS <- data.frame(fread("https://lcps.nu/wp-content/uploads/covid-19.csv"))
  # Uitleg LCPS data: https://lcps.nu/datafeed/
  
  # OWiD
  dat_OWiD <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv?raw=true") #https://github.com/owid/covid-19-data/tree/master/public/data
  # OWiD codebook: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-codebook.csv
  # https://doi.org/10.1038/s41597-020-00688-8
  
  # CBS
  dat_CBS <- fread("https://opendata.cbs.nl/CsvDownload/csv/83474NED/UntypedDataSet?dl=41CFE")
  dat_CBS_prov <- fread("https://opendata.cbs.nl/CsvDownload/csv/37230ned/UntypedDataSet?dl=433DC")
  
  # Create input data list
  dat <- list(RIVM = dat_RIVM, RIVM_R = dat_RIVM_R, RIVM_R = dat_RIVM_R, RIVM_test = dat_RIVM_test, 
              RIVM_nursery = dat_RIVM_nursery, NICE_IC_C = dat_NICE_IC_C, NICE_IC_I = dat_NICE_IC_I, 
              NICE_IC_B = dat_NICE_IC_B, NICE_Hosp_C = dat_NICE_Hosp_C, NICE_Hosp_I = dat_NICE_Hosp_I, 
              NICE_Hosp_B = dat_NICE_Hosp_B, LCPS = dat_LCPS,OWiD = dat_OWiD, CBS = dat_CBS, 
              CBS_prov = dat_CBS_prov)
  rm(dat_RIVM, dat_RIVM_R, dat_RIVM_test, dat_RIVM_nursery, dat_NICE_IC_C, dat_NICE_IC_I, dat_NICE_IC_B, 
     dat_NICE_Hosp_C, dat_NICE_Hosp_I, dat_NICE_Hosp_B, dat_LCPS, dat_OWiD, dat_CBS, dat_CBS_prov) # Clean workspace
  
  ###### MANIPULATE INPUT DATA ######
  # RIVM
  dat$RIVM_R <- data.frame(rbindlist(dat$RIVM_R, fill = TRUE)) 
  dat$RIVM_R$Date <- as.Date(dat$RIVM_R$Date)
  dat$RIVM_R$Rt_low <- as.numeric(dat$RIVM_R$Rt_low)
  dat$RIVM_R$Rt_avg <- as.numeric(dat$RIVM_R$Rt_avg)
  dat$RIVM_R$Rt_up <- as.numeric(dat$RIVM_R$Rt_up)
  dat$RIVM_R$population <- as.factor(dat$RIVM_R$population)
  
  # NICE
  dat$NICE_IC_C <- data.frame(rbindlist(dat$NICE_IC_C, fill = TRUE)) 
  dat$NICE_IC_I <- data.frame(rbindlist(dat$NICE_IC_I[[1]], fill = TRUE), rbindlist(dat$NICE_IC_I[[2]], fill = TRUE) [, 2])
  dat$NICE_IC_B <- data.frame(rbindlist(dat$NICE_IC_B, fill = TRUE)) 
  dat$NICE_Hosp_C <- data.frame(rbindlist(dat$NICE_Hosp_C, fill = TRUE)) 
  dat$NICE_Hosp_I <- data.frame(rbindlist(dat$NICE_Hosp_I[[1]], fill = TRUE), rbindlist(dat$NICE_Hosp_I[[2]], fill = TRUE) [, 2])
  dat$NICE_Hosp_B <- data.frame(rbindlist(dat$NICE_Hosp_B, fill = TRUE)) 
  
  # LCPS
  dat$LCPS$Datum <- as.Date(dat$LCPS$Datum, tryFormats = c("%d-%m-%Y"))
  dat$LCPS <- dat$LCPS[order(dat$LCPS$Datum), ]
  row.names(dat$LCPS) <- NULL
  
  ###### DATA CALCULATIONS ######
  # RIVM
  COV <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, data = dat$RIVM)
  COV_limb <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, 
                        data = subset(dat$RIVM, Province == "Limburg"))
  Death <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, data = dat$RIVM)
  Death_limb <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, 
                          data = subset(dat$RIVM, Province == "Limburg"))
  Nurs <- aggregate(formula = Total_infected_locations_reported ~ Date_of_statistic_reported, FUN = sum, data = dat$RIVM_nursery)
  Nurs_I <- aggregate(formula = Total_new_infected_locations_reported ~ Date_of_statistic_reported, FUN = sum, data = dat$RIVM_nursery)
  COV_Rt <- subset(dat$RIVM_R, dat$RIVM_R$population == levels(dat$RIVM_R$population)[2])
  
  ###### CREATE DATAFRAMES ######
  # I = incidentie, C = cumulatieve incidentie, B = bezetting, A = huidig aantal, _rel = per 100, 000
  # Populatie
  Population <- data.frame(NLD = tail(as.numeric(dat$CBS$`Bevolking aan het eind van de periode (aantal)`), n = 1), 
                           Limb = tail(as.numeric(dat$CBS_prov[dat$CBS_prov$`Regio's` == "Limburg (PV)"]$'Bevolking aan het einde van de periode (aantal)'), n = 1)
  )
  
  # Infectie cijfers; gemelde patienten / aantal positieve testen / Rt
  COV <- data.frame(C = as.numeric(COV$Total_reported), 
                    I = pmax(COV$Total_reported - shift(COV$Total_reported, n = 1, fill = 0, type = "lag"), 0), 
                    C_limb = as.numeric(COV_limb$Total_reported), 
                    I_limb = pmax(COV_limb$Total_reported - shift(COV_limb$Total_reported, n = 1, fill = 0, type = "lag"), 0), 
                    date = as.Date(COV$Date_of_report)
  )
  COV <- data.frame(COV, 
                    I_rel = COV$I / Population$NLD * 100000, 
                    I_rel_limb = COV$I_limb / Population$Limb * 100000, 
                    I_3d = rollsumr(COV$I, k = 3, fill = NA), 
                    I_3d_limb = rollsumr(COV$I_limb, k = 3, fill = NA), 
                    I_3d_rel = rollsumr(COV$I, k = 3, fill = NA) / Population$NLD * 100000, 
                    I_3d_rel_limb = rollsumr(COV$I_limb, k = 3, fill = NA) / Population$Limb * 100000, 
                    I_7d = rollsumr(COV$I, k = 7, fill = NA), 
                    I_7d_limb = rollsumr(COV$I_limb, k = 7, fill = NA), 
                    I_7d_rel = rollsumr(COV$I, k = 7, fill = NA) / Population$NLD * 100000, 
                    I_7d_rel_limb = rollsumr(COV$I_limb, k = 7, fill = NA) / Population$Limb * 100000
  )
  COV <- subset(COV, COV$date >= date_start) # Select data from start date 
  
  COV_test <- data.frame(I_pos_7d = dat$RIVM_test$positief, 
                         I_total_7d = dat$RIVM_test$totaal, 
                         prop_pos = dat$RIVM_test$prop_pos, 
                         date = dat$RIVM_test$datum_tot, 
                         I_pos_7d_rel = dat$RIVM_test$positief / Population$NLD * 100000
  ) 
  COV_test <- subset(COV_test, COV_test$date >= date_start) # Select data from start date 
  
  COV_Rt <- data.frame(R = COV_Rt$Rt_avg, 
                       R_lo = COV_Rt$Rt_low, 
                       R_up = COV_Rt$Rt_up, 
                       date = as.Date(COV_Rt$Date)
  ) 
  COV_Rt <- subset(COV_Rt, COV_Rt$date >= date_start) # Select data from start date 
  COV_Rt <- drop_na(COV_Rt, c("R_lo", "R_up")) # drop NAs for min and max (to prevent problems with polygon())
  
  # Ziekenhuisopnames (excl IC)
  # NICE
  Hosp <- data.frame(C = dat$NICE_Hosp_C[, 2], 
                     I = dat$NICE_Hosp_I[, 2] + dat$NICE_Hosp_I[, 3], 
                     B = dat$NICE_Hosp_B[, 2], 
                     date = as.Date(dat$NICE_Hosp_I[, 1])
  )
  Hosp <- data.frame(Hosp, 
                     I_3d = rollsumr(Hosp$I, k = 3, fill = NA), 
                     B_3d = rollsumr(Hosp$B, k = 3, fill = NA)
  )
  Hosp <- subset(Hosp, Hosp$date >= date_start) # Select data from start date 
  Hosp <- subset(Hosp, Hosp$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)
  
  # LCPS (bezetting)
  Hosp_LCPS <- data.frame(B = as.numeric(dat$LCPS$Kliniek_Bedden), 
                          date = dat$LCPS$Datum, 
                          B_3d = rollsumr(as.numeric(dat$LCPS$Kliniek_Bedden), k = 3, fill = NA),
                          B_7d = rollsumr(as.numeric(dat$LCPS$Kliniek_Bedden), k = 7, fill = NA)
  )
  Hosp_LCPS <- subset(Hosp_LCPS, Hosp_LCPS$date >= date_start) # Select data from start date 
  #Hosp_LCPS <- subset(Hosp_LCPS, Hosp_LCPS$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)
  
  # IC opnames 
  # NICE
  IC <- data.frame(C = dat$NICE_IC_C[, 2], 
                   I = dat$NICE_IC_I[, 2] + dat$NICE_IC_I[, 3], 
                   B = dat$NICE_IC_B[, 2], 
                   date = as.Date(dat$NICE_IC_I[, 1])
  )
  IC <- data.frame(IC, 
                   I_3d = rollsumr(IC$I, k = 3, fill = NA), 
                   B_3d = rollsumr(IC$B, k = 3, fill = NA),
                   I_7d = rollsumr(IC$I, k = 7, fill = NA), 
                   B_7d = rollsumr(IC$B, k = 7, fill = NA)
  )
  IC <- subset(IC, IC$date >= date_start) # Select data from start date 
  IC <- subset(IC, IC$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)
  
  # LCPS (bezetting)
  IC_LCPS <- data.frame(B = as.numeric(dat$LCPS$IC_Bedden_COVID), 
                        B_non_covid = as.numeric(dat$LCPS$IC_Bedden_Non_COVID), 
                        B_total = as.numeric(dat$LCPS$IC_Bedden_COVID) + as.numeric(dat$LCPS$IC_Bedden_Non_COVID), 
                        date = dat$LCPS$Datum)
  IC_LCPS <- data.frame(IC_LCPS, 
                        B_3d = rollsumr(IC_LCPS$B, k = 3, fill = NA), 
                        B_non_covid_3d = rollsumr(IC_LCPS$B_non_covid, k = 3, fill = NA), 
                        B_total_3d = rollsumr(IC_LCPS$B_total, k = 3, fill = NA),
                        B_7d = rollsumr(IC_LCPS$B, k = 7, fill = NA), 
                        B_non_covid_7d = rollsumr(IC_LCPS$B_non_covid, k = 7, fill = NA), 
                        B_total_7d = rollsumr(IC_LCPS$B_total, k = 7, fill = NA)
  )
  IC_LCPS <- subset(IC_LCPS, IC_LCPS$date >= date_start) # Select data from start date 
  #IC_LCPS <- subset(IC_LCPS, IC_LCPS$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)
  
  # Verpleeghuislocaties
  Nurs <- data.frame(A = as.numeric(Nurs$Total_infected_locations_reported), 
                     I = as.numeric(Nurs_I$Total_new_infected_locations_reported), 
                     date = as.Date(Nurs$Date_of_statistic_reported)
  )
  Nurs <- data.frame(Nurs, 
                     A_3d = rollsumr(Nurs$A, k = 3, fill = NA),
                     A_prop = Nurs$A / 2451, #uitgaande van 2451 Verpleeghuislocaties in NL (ligt ergens tussen 2451 - 2459)
                     I_3d = rollsumr(Nurs$I, k = 3, fill = NA),
                     I_7d = rollsumr(Nurs$I, k = 7, fill = NA)
  )
  Nurs <- subset(Nurs, Nurs$date >= date_start) # Select data from start date 
  
  # Sterfte
  Death <- data.frame(C = as.numeric(Death$Deceased), 
                      I = pmax(Death$Deceased - shift(Death$Deceased, n = 1, fill = 0, type = "lag"), 0), 
                      C_limb = as.numeric(Death_limb$Deceased), 
                      I_limb = pmax(Death_limb$Deceased - shift(Death_limb$Deceased, n = 1, fill = 0, type = "lag"), 0), 
                      date = as.Date(Death$Date_of_report)
  )
  Death <- data.frame(Death, 
                      I_rel = Death$I / Population$NLD * 100000, 
                      I_rel_limb = Death$I_limb / Population$Limb * 100000, 
                      I_3d = rollsumr(Death$I, k = 3, fill = NA), 
                      I_3d_limb = rollsumr(Death$I_limb, k = 3, fill = NA), 
                      I_3d_rel = rollsumr(Death$I, k = 3, fill = NA) / Population$NLD * 100000, 
                      I_3d_rel_limb = rollsumr(Death$I_limb, k = 3, fill = NA) / Population$Limb * 100000, 
                      I_7d = rollsumr(Death$I, k = 7, fill = NA), 
                      I_7d_limb = rollsumr(Death$I_limb, k = 7, fill = NA),
                      I_7d_rel = rollsumr(Death$I, k = 7, fill = NA) / Population$NLD * 100000, 
                      I_7d_rel_limb = rollsumr(Death$I_limb, k = 7, fill = NA) / Population$Limb * 100000
  )
  Death <- subset(Death, Death$date >= date_start) # Select data from start date 
  
  # Internationaal
  Int <- data.frame(continent = as.factor(dat$OWiD$continent), 
                    iso = as.factor(dat$OWiD$iso_code), 
                    country = as.factor(dat$OWiD$location), 
                    population = dat$OWiD$population, 
                    I_COV = pmax(dat$OWiD$population * dat$OWiD$new_cases_per_million / 1000000, 0), 
                    I_COV_smooth = pmax(dat$OWiD$population * dat$OWiD$new_cases_smoothed_per_million / 1000000, 0), 
                    I_COV_rel = pmax(dat$OWiD$new_cases_per_million / 10, 0), 
                    I_COV_rel_smooth = pmax(dat$OWiD$new_cases_smoothed_per_million / 10, 0), 
                    I_test_pos_rel = pmax(dat$OWiD$new_tests_per_thousand * dat$OWiD$positive_rate * 100, 0), 
                    prop_test_pos = pmax(dat$OWiD$positive_rate, 0), 
                    stringency_index = dat$OWiD$stringency_index, 
                    GDP = dat$OWiD$gdp_per_capita, 
                    LE = dat$OWiD$life_expectancy, 
                    date = as.Date(dat$OWiD$date)
  )
  Int <- subset(Int, Int$continent == "Europe") # Select Europe
  Int <- subset(Int, Int$LE >= 80) # Select countries with life expectancy above or equal to 80 
  Int<- subset(Int, Int$population >= 1000000) # Select countries with population >= 1 mln 
  Int <- droplevels(Int)
  Int <- subset(Int, Int$date >= date_start - 14)
  Int <- data.frame(Int, 
                    I_3d = rollsumr(Int$I_COV, by = Int$country, k = 3, fill = NA), 
                    I_3d_rel = rollsumr(Int$I_COV, by = Int$country, k = 3, fill = NA) / Int$population * 100000, 
                    I_7d = rollsumr(Int$I_COV, by = Int$country, k = 7, fill = NA), 
                    I_7d_rel = rollsumr(Int$I_COV, by = Int$country, k = 7, fill = NA) / Int$population * 100000
  )
  Int <- subset(Int, Int$date >= date_start) # Select data from start date 
  
  row.names(Hosp) <- row.names(IC) <- NULL
  rm(COV_limb, Death_limb, Nurs_I) # clean workspace
  
  ###### TRENDLIJN ######
  pred_COV_I <- f_trend(x = COV$I, time = 7, span = 0.25)$pred
  pred_COV_I_rel <- pred_COV_I
  pred_COV_I_rel[, -1] <- pred_COV_I_rel[, -1] / Population$NLD * 100000
  pred_COV_I_limb <- f_trend(x = COV$I_limb, time = 7, span = 0.25)$pred
  pred_COV_I_rel_limb <- pred_COV_I_limb
  pred_COV_I_rel_limb[, -1] <- pred_COV_I_rel_limb[, -1] / Population$Limb * 100000
  
  pred_Hosp_I <- f_trend(x = Hosp$I, time = 7, span = 0.25)$pred
  pred_Hosp_B <- f_trend(x = Hosp$B, time = 7, span = 0.25)$pred
  pred_IC_I <- f_trend(x = IC$I, time = 7, span = 0.25)$pred
  pred_IC_B <- f_trend(x = IC$B, time = 7, span = 0.25)$pred
  
  pred_Hosp_LCPS_B <- f_trend(x = Hosp_LCPS$B, time = 7, span = 0.25)$pred
  pred_IC_LCPS_B <- f_trend(x = IC_LCPS$B, time = 7, span = 0.25)$pred
  pred_IC_LCPS_B_non_covid <- f_trend(x = IC_LCPS$B_non_covid, time = 7, span = 0.25)$pred
  pred_IC_LCPS_B_total <- pred_IC_LCPS_B
  pred_IC_LCPS_B_total[, -1] <- pred_IC_LCPS_B[, -1] + pred_IC_LCPS_B_non_covid[, -1]
  pred_Hosp_IC_LCPS_B_total_cov <- pred_IC_LCPS_B
  pred_Hosp_IC_LCPS_B_total_cov[, -1] <- pred_IC_LCPS_B[, -1] + pred_Hosp_LCPS_B[, -1]
  
  # Create prediction data list
  pred <- list(COV_I = pred_COV_I, COV_I_rel = pred_COV_I_rel, COV_I_limb = pred_COV_I_limb,
               COV_I_rel_limb = pred_COV_I_rel_limb, Hosp_I = pred_Hosp_I, Hosp_B = pred_Hosp_B,
               IC_I = pred_IC_I, IC_B = pred_IC_B, Hosp_LCPS_B = pred_Hosp_LCPS_B, 
               IC_LCPS_B = pred_IC_LCPS_B, IC_LCPS_B_non_covid = pred_IC_LCPS_B_non_covid, 
               IC_LCPS_B_total = pred_IC_LCPS_B_total, Hosp_IC_LCPS_B_total_cov = pred_Hosp_IC_LCPS_B_total_cov)
  rm(pred_COV_I, pred_COV_I_rel, pred_COV_I_limb, pred_COV_I_rel_limb, pred_Hosp_I, pred_Hosp_B, pred_IC_I, pred_IC_B,
     pred_Hosp_LCPS_B, pred_IC_LCPS_B, pred_IC_LCPS_B_non_covid, pred_IC_LCPS_B_total, pred_Hosp_IC_LCPS_B_total_cov) # Clean workspace
  
  ###### Output   ###### 
  df <- list(COV = COV, COV_Rt = COV_Rt, COV_test = COV_test, 
             Hosp = Hosp, Hosp_LCPS = Hosp_LCPS, IC = IC, IC_LCPS = IC_LCPS,
             Nurs = Nurs, Death = Death, Int = Int, Population = Population, pred = pred)
  
  return(list(dat = dat , df = df))
}