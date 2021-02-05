# Description: Shiny server function

f_server <- function(input, output){   
  # when event is observed ...
  observeEvent(input$SI_date_start,       
               ignoreNULL = FALSE, {
                 
                 #-- Refresh data  --#
                 # obtain and manipulate data
                 # dat <- f_data() # obtain data
                 # df <- f_data_man(dat = dat, date_start = as.Date("2020-06-01","%Y-%m-%d")) # manipulate data
                 
                 # make figures (stored in Figures folder)
                 # f_figure(df, date_start = input$SI_date_start) 
                 
                 # render plot repeatedly updates.
                 output$SO_1_incidentie_NL <- renderPlot(f_figure(df, date_start = input$SI_date_start, figure = 1))
                 
                 # when date is adjusted button pressed ...
                 # observeEvent(input$SI_date_start,       
                 #              ignoreNULL = FALSE, {
                 #                
                 #                # make figures (stored in Figures folder)
                 #                f_figure(df, date_start = input$SI_date_start) 
                 #                
                 #              }) # Observe event end
                 
               }) # Observe event end
  
  
  
} # Server end
