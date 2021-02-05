# Description: Shiny graphical user interface

ui <- dashboardPage( # creates shiny dashboard (https://rstudio.github.io/shinydashboard/structure.html)
  
  ##### Header #####
  dashboardHeader( # open header
    title = "",
    #titleWidth = 0,
    tags$li(class = "dropdown", tags$a(
      "Covid-19 NL in Shiny"
    ))
  ), # close header
  
  ##### Sidebar panel #####
  dashboardSidebar( # open sidebar panel
    sidebarMenu(
      menuItem("Besmettingen", tabName = "besmettingen", icon = icon("bar-chart-o")),
      menuItem("Data", tabName = "data", icon = icon("list-alt"))
    )
    
  ), # close sidebar panel
  
  ##### Body #####
  dashboardBody( # open body
    tabItems(
      
      # open tab
      tabItem(tabName = "besmettingen",

              # # input type slider
              sliderInput(inputId = "SI_date_start",
                          label = "Start date",
                          min = as.Date("2020-06-01","%Y-%m-%d"),
                          max = as.Date((Sys.Date() - 7),"%Y-%m-%d"),
                          value = as.Date("2020-06-01", "%Y-%m-%d"),
                          timeFormat = "%Y-%m-%d"),

              # # action button refreshes  data when pressed
              # actionButton(inputId = "refresh_data",     
              #              label   = "Refresh data"),
              
              # figures
              fluidRow( # input in fluidrow
                tabBox( # open tabbox
                  selected = "COVID-19 Incidentie / dag",
                  tabPanel(title = "COVID-19 Incidentie / dag", # heading
                           plotOutput(outputId = "SO_1_incidentie_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", "RIVM (https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv)"
                  ) # close tabPanel
                ) # close tabbox
              ) # close fluidrow
      ), # close tab
      
      # open tab 
      tabItem(tabName = "data",
              h2("data")
      ) # close tab
      
    ) # close tabItems
    
  ) # close body    
  
) # close UI dashboard
