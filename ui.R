# Description: Shiny graphical user interface

ui <- dashboardPage( # creates shiny dashboard (https://rstudio.github.io/shinydashboard/structure.html)
  
  ##### Header #####
  dashboardHeader( # open header
    title = "",
    #titleWidth = 0,
    tags$li(class = "dropdown", tags$a(
      "Covid-19 NL in R Shiny"
    ))
  ), # close header
  
  ##### Sidebar panel #####
  dashboardSidebar( # open sidebar panel
    sidebarMenu(
      menuItem("Besmettingen", tabName = "besmettingen", icon = icon("bar-chart-o")),
      menuItem("Besmettingen internationaal", tabName = "besmettingen_int", icon = icon("bar-chart-o")),
      menuItem("Opnames", tabName = "opnames", icon = icon("bar-chart-o")),
      menuItem("Bezetting", tabName = "bezetting", icon = icon("bar-chart-o")),
      menuItem("Overige", tabName = "overige", icon = icon("bar-chart-o")),
      menuItem("Data", tabName = "data", icon = icon("list-alt"))
    ),
    # input type slider
    sliderInput(inputId = "SI_date_start",
                label = "Start datum",
                min = as.Date("2020-06-01","%Y-%m-%d"),
                max = as.Date((Sys.Date() - 7),"%Y-%m-%d"),
                value = as.Date("2020-06-01", "%Y-%m-%d"),
                timeFormat = "%Y-%m-%d")
  ), # close sidebar panel
  
  ##### Body #####
  dashboardBody( # open body
    tabItems(

      ##### Tab 1 #####
      # open tab
      tabItem(tabName = "besmettingen",
              fluidRow( # input in fluidrow
                tabBox( # open tabbox
                  selected = "COVID-19 aantal nieuwe gemelde patienten",
                  tabPanel(title = "COVID-19 aantal nieuwe gemelde patienten", # heading
                           plotOutput(outputId = "SO_1_Incidentie_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "COVID-19 aantal nieuwe gemelde patienten",
                  tabPanel(title = "COVID-19 aantal nieuwe gemelde patienten", # heading
                           plotOutput(outputId = "SO_2_Incidentie_NL_per100000") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv"),
                           br(), a("CBS", href = "https://opendata.cbs.nl/CsvDownload/csv/83474NED/UntypedDataSet?dl=41CFE"),
                           br(), a("CBS", href = "https://opendata.cbs.nl/CsvDownload/csv/37230ned/UntypedDataSet?dl=433DC")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "COVID-19 % positieve testen",
                  tabPanel(title = "COVID-19 % positieve testen", # heading
                           plotOutput(outputId = "SO_3_Perc_test_pos_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://www.rivm.nl/coronavirus-covid-19/actueel/wekelijkse-update-epidemiologische-situatie-covid-19-in-nederland")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "COVID-19 reproductie index",
                  tabPanel(title = "COVID-19 reproductie index", # heading
                           plotOutput(outputId = "SO_4_Rt_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json")
                  ) # close tabPanel
                ) # close tabbox
              ) # close fluidrow
      ), # close tab
      
      ##### Tab 2 #####
      # open tab
      tabItem(tabName = "besmettingen_int",
              fluidRow( # input in fluidrow
                tabBox( # open tabbox
                  selected = "COVID-19 Incidentie / dag per 100,000",
                  tabPanel(title = "COVID-19 Incidentie / dag per 100,000", # heading
                           plotOutput(outputId = "SO_12_Incidentie_INT") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv"),
                           br(), a("CBS", href = "https://opendata.cbs.nl/CsvDownload/csv/83474NED/UntypedDataSet?dl=41CFE"),
                           br(), a("CBS", href = "https://opendata.cbs.nl/CsvDownload/csv/37230ned/UntypedDataSet?dl=433DC"),
                           br(), a("OWiD", href = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv?raw=true")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "COVID-19 % positieve testen",
                  tabPanel(title = "COVID-19 % positieve testen", # heading
                           plotOutput(outputId = "SO_13_Perc_test_pos_INT") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://www.rivm.nl/coronavirus-covid-19/actueel/wekelijkse-update-epidemiologische-situatie-covid-19-in-nederland"),
                           br(), a("OWiD", href = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv?raw=true")
                  ) # close tabPanel
                ) # close tabbox
              ) # close fluidrow
      ), # close tab
      
      ##### Tab 3 #####
      # open tab
      tabItem(tabName = "opnames",
              fluidRow( # input in fluidrow
                tabBox( # open tabbox
                  selected = "COVID-19 ziekenhuisopnames exclusief IC",
                  tabPanel(title = "COVID-19 ziekenhuisopnames exclusief IC", # heading
                           plotOutput(outputId = "SO_5_Opnames_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("NICE", href = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "COVID-19 IC opnames",
                  tabPanel(title = "COVID-19 IC opnames", # heading
                           plotOutput(outputId = "SO_6_ICopnames_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("NICE", href = "https://www.stichting-nice.nl/covid-19/public/new-intake/")
                  ) # close tabPanel
                ) # close tabbox
              ) # close fluidrow
      ), # close tab
      
      ##### Tab 4 #####
      # open tab
      tabItem(tabName = "bezetting",
              fluidRow( # input in fluidrow
                tabBox( # open tabbox
                  selected = "COVID-19 IC bedden bezetting",
                  tabPanel(title = "COVID-19 IC bedden bezetting", # heading
                           plotOutput(outputId = "SO_7_ICbezetting_cov_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("LCPS", href = "https://lcps.nu/wp-content/uploads/covid-19.csv"),
                           br(), a("NICE", href = "https://www.stichting-nice.nl/covid-19/public/intake-count/")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "IC bedden bezetting",
                  tabPanel(title = "IC bedden bezetting", # heading
                           plotOutput(outputId = "SO_8_ICbezetting_noncov_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("LCPS", href = "https://lcps.nu/wp-content/uploads/covid-19.csv")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "COVID-19 ziekenhuisbedden bezetting",
                  tabPanel(title = "COVID-19 ziekenhuisbedden bezetting", # heading
                           plotOutput(outputId = "SO_9_Ziekenhuisbezetting_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("LCPS", href = "https://lcps.nu/wp-content/uploads/covid-19.csv")
                  ) # close tabPanel
                ) # close tabbox
              ) # close fluidrow
      ), # close tab
      
      ##### Tab 5 #####
      # open tab
      tabItem(tabName = "overige",
              fluidRow( # input in fluidrow
                tabBox( # open tabbox
                  selected = "COVID-19 verpleeghuislocaties",
                  tabPanel(title = "COVID-19 verpleeghuislocaties", # heading
                           plotOutput(outputId = "SO_10_Verpleeghuislocaties_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://data.rivm.nl/covid-19/COVID-19_verpleeghuizen.csv")
                  ) # close tabPanel
                ), # close tabbox
                tabBox( # open tabbox
                  selected = "COVID-19 sterfte",
                  tabPanel(title = "COVID-19 sterfte", # heading
                           plotOutput(outputId = "SO_11_Sterfte_NL") # plotOutput from server
                  ), # close tabPanel
                  tabPanel("Bronvermelding", 
                           a("RIVM", href = "https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv")
                  ) # close tabPanel
                ) # close tabbox
              ) # close fluidrow
      ), # close tab
      
      ##### Tab 6 #####
      # open tab 
      tabItem(tabName = "data",
              h1("Download data gebruikt voor de figuren"),
              fluidRow( # input in fluidrow
                # download buttons 
                box( # open box
                  title = "Download data", status = "primary", solidHeader = TRUE,
                downloadButton(outputId = "SO_download_raw_data", # downloads raw data when pressed
                               label = "Download raw data"),
                downloadButton(outputId = "SO_download_processed_data", # downloads processed data when pressed
                               label = "Download processed data")
                ) # close box
              ) # close fluidrow
      ) # close tab
    ) # close tabItems
  ) # close body    
) # close UI dashboard
