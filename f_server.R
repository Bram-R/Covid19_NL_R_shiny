# Description: Shiny server function

f_server <- function(input, output){

  # generic
  date_start <- as.Date("2020-06-01","%Y-%m-%d")
  
  # obtain and process data
  dat <- f_data() # obtain data
  df <- f_data_process(dat = dat, date_start = date_start) # process data
  
  #-- SAVE R ENVIRONMENT --#
  # save(dat, file = "Data/dat.RDATA") # raw data
  # save(df, file = "Data/df.RDATA") # processed data
  
  #-- Update plots when event (change in start date) is observed --#
  observeEvent(input$SI_date_start, ignoreNULL = FALSE, {
    output$SO_1_Incidentie_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 1))
    output$SO_2_Incidentie_NL_per100000 <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 2))
    output$SO_3_Perc_test_pos_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 3))
    output$SO_4_Rt_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 4))
    output$SO_5_Opnames_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 5))
    output$SO_6_ICopnames_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 6))
    output$SO_7_ICbezetting_cov_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 7))
    output$SO_8_ICbezetting_noncov_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 8))
    output$SO_9_Ziekenhuisbezetting_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 9))
    output$SO_10_Verpleeghuislocaties_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 10))
    output$SO_11_Sterfte_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 11))
    output$SO_12_Incidentie_INT <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 12))
    output$SO_13_Perc_test_pos_INT <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 13))
  }) # observe event end
  
  #-- Create downloadable files --#
  output$SO_download_raw_data <- downloadHandler(
    filename = function() {
      paste("dat_", Sys.Date(), ".RDATA", sep="")
    },
    content = function(file) {
      save(dat, file = file) # raw data
    }
  ) # close downloadHandler
  
  output$SO_download_processed_data <- downloadHandler(
    filename = function() {
      paste("df_", Sys.Date(), ".RDATA", sep="")
    },
    content = function(file) {
      save(df, file = file) # processed data
    }
  ) # close downloadHandler
} # server end
