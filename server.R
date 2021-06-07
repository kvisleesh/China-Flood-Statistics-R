## loading datasets---------------------------------------------------##
data = read.csv("flood_china.csv")
data1 = read.csv("flood_china_agg.csv")
data_cause <- read.csv("flood_china_cause.csv")

## preparing data to plot deadliest bar chart---------------------------------------------------##
data_cause_dead <- data.frame(data_cause[,2])
names(data_cause_dead) <- c("Main Cause")
row.names(data_cause_dead) <- c("Dam/Levy, break or release", "Ice jam/break-up", "Snowmelt", "Torrential Rain", "Tropical Cyclone", "Tropical Storm", "Typhoon")


## preparing data to plot line & bar chart overlay------------------------------------##
dead <- data1$Dead
displaced <- data1$Displaced
duration <- data1$Duration.Days

dat <- function(selection) {
  return (switch(selection,
                 "dead" = ggplot(data1) + 
                   geom_col(aes(x = Year, y = No..of.Floods, fill = "No. of Floods")) + 
                   geom_smooth(aes(x = Year, y = Dead/100, color="Dead"), group = 1)+
                   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Dead"))+
                   scale_fill_manual(name = NULL, values = c("No. of Floods" = "blue")) +
                   scale_color_manual(name = NULL, values = c("Dead" = "red")) + 
                   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
                   theme_minimal(),
                 
                 "displaced" = ggplot(data1) + 
                   geom_col(aes(x = Year, y = No..of.Floods, fill = "No. of Floods")) +
                   geom_smooth(aes(x = Year, y = Displaced/100000, color="Displaced in 100,000s"), group = 1)+
                   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Displaced in 100,000s"))+
                   scale_fill_manual(name = NULL, values = c("No. of Floods" = "blue")) +
                   scale_color_manual(name = NULL, values = c("Displaced in 100,000s" = "red")) + 
                   theme_minimal(),
                 
                 "duration" = ggplot(data1) + 
                   geom_col(aes(x = Year, y = No..of.Floods, fill = "No. of Floods")) +
                   geom_smooth(aes(x = Year, y = Duration.Days./10, color="Duration(Days)"), group = 1)+
                   scale_y_continuous(sec.axis = sec_axis(~./3, name = "Duration(Days)"))+
                   scale_fill_manual(name = NULL, values = c("No. of Floods" = "blue")) +
                   scale_color_manual(name = NULL, values = c("Duration(Days)" = "red"))+
                   theme_minimal()
                 
                 ))
}

## preparing data to plot timeline
data_timeline <- data.frame(
  content = c("Introduced Rules and Regulation of Flood Prevention" ,"Introduced Regulation of River Course Management","Flood Mitigation shifted from flood control to flood management" ,"Water Law (1988) was revised","Asian Development Bank prepare national flood management strategy", "Ministry of Water Resources prepare national flood management strategy","Completion of Three Gorges Dam (Body)"),
  start   = c(1991, 1998,2002, 2003,2004,2005, 2006)
)

## preparing data to plot pie charts ------------------------------------------------------##
a <- aggregate(data.frame(count = data$Severity),       # Apply aggregate function
               list(value = data$Severity),
               length)

s <- c(rep("Class 1", a[1,2]),
       rep("Class 1.5", a[2,2]),
       rep("Class 2", a[3,2]))

b <- aggregate(data.frame(count = data$Magnitude.Scale),       # Apply aggregate function
               list(value = data$Magnitude.Scale),
               length)

ms <- c(rep("Medium", b[1,2]),
        rep("Low", b[2,2]),
        rep("High", b[3,2])
)

c <- aggregate(data.frame(count = data$Main.Cause),       # Apply aggregate function
               list(value = data$Main.Cause),
               length)

mc <- c(rep(c[1,1], c[1,2]),
        rep(c[2,1], c[2,2]),
        rep(c[3,1], c[3,2]),
        rep(c[4,1], c[4,2]),
        rep(c[5,1], c[5,2]),
        rep(c[6,1], c[6,2]),
        rep(c[7,1], c[7,2])
)
#----------------------------------------------------------------------------------------#

server = function(input, output, session) {
  # The currently selected tab from the first box
  output$tabset1Selected <- renderText({
    input$tabset1
  })
  
#render Map-------------------------------------------------------------------------------------###
  output$m <- renderLeaflet({
    m <- data %>% 
      leaflet() %>%
      setView(lng=103, lat=36, zoom=3.8 ) %>% #view and zoom level is set to default focusing map of china
      #selection of map tiles
      addProviderTiles(providers$Esri.DeLorme, group = "DeLorme") %>%
      addProviderTiles(providers$CartoDB.Voyager, group = "CartoDB.Voyager") %>%
      addProviderTiles(providers$Esri.OceanBasemap, group = "OceanBasemap") %>%
      addLayersControl(baseGroups = c("OceanBasemap","CartoDB.Voyager","De Lorme")) %>%
      
      #plot of individual flood events on the map
      addCircleMarkers(
        lng = data$Longitude, 
        lat=data$Latitude, 
        weight = 3,
        radius = data$Magnitude,
        fillOpacity = 1,
        fillColor = "#E63636",
        color = "#E63636",
        
        #click dots for more details
        popup= ~paste0("Flood Details", "<br/>Date: ", data$Began,"<br/>Duration(Days): ", data$Duration.Days, "<br/>Dead: ", data$Dead, "<br/>Displaced: ", data$Displaced, "<br/>Magnitude: ", data$Magnitude, "<br/>Area Affected(sqkm): ", data$Affected.sqkm., "<br/>Detailed Location: ", data$Detailed.Location)
        ,clusterOptions = markerClusterOptions()
        )
  })
  
  #Render line + bar chart overlay--------------------------------------------------------------------###
  output$plot<-renderPlot({
    dat(input$linecol)},height = 400,width = 600)
  
  
  # Render pie charts--------------------------------------------------------------------------------###
  renderPieChart(div_id = "piechart1",
                 data = s)  
  renderPieChart(div_id = "piechart2",
                 data = ms) 
  renderPieChart(div_id = "piechart3",
                 data = mc) 
  
  #Render side bar menu items------------------------------------------------------------------------###
  # this is to open the Home default page. However, it is a bug that has not been fixed by Shiny.
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Home", tabName="Home", icon = icon("home"))
      #, menuItem("Menu item2", tabName="m2", icon = icon("database"))
    )
  })
  isolate({updateTabItems(session, "tabs", "Welcome")}) 
  
  # render ECharts2Shiny bar chart-----------------------------------------###
  renderBarChart(div_id = "test", grid_left = '0.5%',font.size.legend = 14, theme = "shine",grid_bottom = "5%", 
                 data = data_cause_dead, direction = "horizontal", stack_plot = TRUE, axis.y.name = "Dead", font.size.axis.x = 14)
  
  #Render timeline##---------------------------------------------------------------###
  output$timeline <- renderTimevis({
    timevis(data_timeline, 
            zoomFactor = 0.2
            )
  })
}
