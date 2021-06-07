#install.packages("shinyWidgets")
#install.packages("shinydashboardPlus")
#install.packages("shinydashboard")
#install.packages("ECharts2Shiny")
#devtools::install_github("RinteRface/shinydashboardPlus")

library(shiny)
#library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(ECharts2Shiny)
library(ggplot2)
library(leaflet)
library(timevis)

loadEChartsLibrary()
####################################HEADER#######################################
header <- dashboardHeader(title = "China Floods", titleWidth = 220)
####################################SIDEBAR#######################################

sidebar <- dashboardSidebar(width = 220,
  sidebarMenu(id="tabs",
    menuItem("Home", tabName = "Home", icon = icon("fas fa-home"), selected = TRUE),
    menuItem("Map", icon = icon("globe"), tabName = "Map", badgeColor = "green"),
    menuItem("Trends", icon = icon("fas fa-chart-line"), tabName = "Trends", badgeColor = "green"),
    menuItem("Frequency", icon = icon("fas fa-chart-pie"), tabName = "Frequency", badgeColor = "green"),
    menuItem("Causes", icon = icon("fas fa-poll-h"), tabName = "Causes", badgeColor = "green")
  )
)
####################################BODY#######################################
body <- dashboardBody(

  tabItems(
    tabItem(tabName = "Home", 
            box(width = 12,
            title = "Welcome to the China Flood Statistics page!",
            h5("The purpose of this website is to raise awareness to all communities who live in areas that are prone to floods and to government officials (within China and outside of China) to take meaningful actions on flood management."),
            h5("As you browse through this website, you will see insights of flood data collected over 30 years from 1985 to 2014."),
            h5("According to Wikipedia, China is the third largest country in the world. It has many rivers and lakes that span across the country and to other countries."),
            h5("They are useful for the development of Chinese civilisation throughout history. However, they are also deadly."),
            
            tags$img(src='1.jpg'),
            h5("Image source: ", tags$a(href="https://vnexplorer.net/massive-flooding-in-south-china-three-gorges-sanxia-dam-at-risk-of-collapse-any-time-a202056415.html","vnexplorer.net"))
              )
            ),
##---------------------------------------MAP PAGE---------------------------------##
    ## Map layout
    tabItem(tabName = "Map",
            h2("Areas Impacted by Floods from 1985 to 2014"),
            h5("Note that the floods occur mainly around the rivers basins in particularly Central and Southern China."),
            
            
            leafletOutput("m", width = "90%", height = 700),
            
            h5("*Tip:"),
            h5("Zoom in and pan to discover more and different locations of China that were flooded."),
            h5("Click on the red dots to reveal information about the flood event.")
            
    ),    

##--------------------------------------TRENDS PAGE-------------------------------##
    # Line + bar chart overlay layout
    tabItem(tabName = "Trends",
            fluidPage(
            h2("What happened between 1985-2014?"),
#--------------Do you know box----#
            box(width = 12,
                title = "Do you know that from 1985 to 2014, the total lives affected by flood are...", status = "info",
                column(width=3,
                       descriptionBlock(
                         header = "Dead", text = "29,477"
                       )),
                column(width=3, 
                       descriptionBlock(
                         header = "Displaced", text = "84,613,229"
                       )),
                column(width=3,
                       descriptionBlock(
                         header = "Total Flood Days", text = 3590
                       )),
                column(width=3,
                       descriptionBlock(
                         header = "Total Area Affected(sqkm)", text = "61,265,645"
                       ))
            ),
#---------------Timeline-------------#
            box(width = 12,
                title = "Timeline of Chinese Government Actions on Flood Management",
                fluidPage(
                  timevisOutput("timeline")
                )
            ),
            
            
            box(width = 12,
                title= "What is the trend?",
            fluidRow(boxPad(width = 12, 
                            
              radioButtons("linecol",
                           h5("The number of dead, displaced and duration of flood have gradually decreased over the years."),
                           choices = list("Dead" = "dead",
                                          "Displaced" = "displaced",
                                          "Duration(Days)" = "duration"
                           )
              )
            )
            ),
            fluidRow(mainPanel(plotOutput("plot")
            )
            )
    )
    )
),
              
##---------------------------------------FREQUENCY PAGE-------------------------------##
    #Pie charts layout
    tabItem(tabName = "Frequency",
            h2("How Frequent do Floods Occur in China?"),
            h4("Frequency of Floods"),
            h5("The pie charts below show the number of floods from 1985 to 2014 by severity, magnitude and main cause."),
            h6("*Tip: "),
            h6("1. Hover over the pie chart wedges for precise frequency and percentage."),
            h6("2. Click on the legends to filter."),
            fluidRow(
              # We MUST load the ECharts javascript library in advance
              loadEChartsLibrary(),
              box(title = "Occurrences by Flood Severity",width = 10, closable = FALSE,collapsible = TRUE,
                  column(width = 7,
                         tags$div(id="piechart1", style="width:100%;height:400px;"),
                         deliverChart(div_id = "piechart1")), 
                  descriptionBlock(header = "Class 1", text = "Large flood events: significant damage to structures or agriculture; fatalities; and/or 1-2 decades-long reported interval since the last similar event."),
                  descriptionBlock(header = "Class 1.5", text = "Large events: greater than 20 yr but less than 100 year recurrence interval, and/or a local recurrence interval of at 10-20 yr."),
                  descriptionBlock(header = "Class 2", text = "Extreme events: with an estimated recurrence interval greater than 100 years")
              )
              
              
            ), 
            
            fluidRow(
              box(title = "Occurrences by Magnitude", width = 10,closable = FALSE,collapsible = TRUE,
                  column(width = 7,
                         tags$div(id="piechart2", style="width:100%;height:400px;"),
                         deliverChart(div_id = "piechart2")),
                  descriptionBlock(header = "How is Flood Magnitude calculated?", text = "Flood Magnitude =LOG(Duration*Severity*Affected Area)")
                  
                  
              )
              
            ),
            
            fluidRow(
              box(title = "Occurrences by Main Causes",width = 10,closable = FALSE,collapsible = TRUE,
                  column(width = 7,
                         tags$div(id="piechart3", style="width:101%;height:600px;"),
                         deliverChart(div_id = "piechart3")),
                  descriptionBlock(header = "How many flood causes are there? ", text = "There are 11 flood causes around the world. In China, there 7 causes of flood.")
                        )
                  )
            ),

##---------------------------------------CAUSES PAGE-------------------------------##
#Bar chart layout
tabItem(tabName = "Causes",
        h2("What is the deadliest cause of flood?"),
        fluidPage(
          box(width = 12,
              title = "Do you know that...", status = "info",
              h5("According to ", tags$a(href="https://en.wikipedia.org/wiki/List_of_dams_and_reservoirs_in_China", "Wikipedia")),
              h5("China constructed a total of 22,104 dams for hydroelectric power generation, flood control, irrigation, drought mitigation, navigation and tourism. The oldest one dates back to 256 BC.")
          )
          ),
        fluidPage(
             h5("Tip: Hover over the bar for details."),
          box(width = 12,
          
          # We HAVE TO to load the ECharts javascript library in advance
          loadEChartsLibrary(),
          
          tags$div(id="test", style="width:80%;height:600px;", direction = "horizontal"),
          deliverChart(div_id = "test")
        )
        
        )

          )
                     
          
        )
                  

        
        )

    

#######################################################################################
ui = dashboardPage(skin = "black", title = "China Flood Statistics", header, sidebar, body)