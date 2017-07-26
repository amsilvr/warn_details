# Setup

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(stringr)
library(sf)
library(leaflet)
library(DT)
# Download Shapefiles
#
if (!exists("alert_tally")) {
  source("CMAS_Clean_shiny.R", echo = TRUE)
}
countyshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
if (!dir.exists("data")) {dir.create("data")}
if (!file.exists("data/county_shape_file.zip")) {
  download.file(countyshapes_url
                , destfile = "data/county_shape_file.zip")}

  c_shp <- unzip("data/county_shape_file.zip", exdir = "data")

# Read the file with sf and add the proper crs code for this projection

counties_sf <- read_sf(c_shp[grep("shp$", c_shp)]) %>%
  as.data.frame() %>% #to fix July 25 problem with the join.sf methods
  left_join(state_iso) %>%
  inner_join(lsad_lookup()) %>%
    select(STATEFP, COUNTYFP, GEOID, NAME, description, iso_3166_2, geometry) %>%
  st_sf() %>%
  st_transform('+proj=longlat +datum=WGS84')

# counties_sf$NAME <-
#   str_replace_all(counties_sf$NAME, pattern = "Ã±",replacement = "ñ") %>%
#   str_replace_all("Ã¡",replacement = "á") %>%
#   str_replace_all("Ã¼",replacement = "ñ") %>%
#   str_replace_all("Ã³",replacement = "ó") %>%
#   str_replace_all("Ã",replacement = "í")

bins <- c(0, 1, 3, 5, 10, 20, 30, 40, 80, 210)
pal <- colorBin("YlOrRd", domain = NULL, bins = bins, pretty = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = shinytheme('darkly'),
   # Application title
  tags$style(type = "text/css",
          "html,
             body {width:100%;height:100%;text-align:left;}
             .selectize-input { font-size: 32px; background-color:aqua; }
             .selectize-dropdown { font-size: 25px; line-height: 30px; }
             .control-label { font-size: 32px; color: white!important;}
             .shiny-input-container {background-color:black;}
             .content::-webkit-scrollbar {display: none;}
             .h {text-align:center!important;}"),

     column(5, offset = 1, #Title and instructions
          h1("WARN Alerts by County"),
          h3("Mouse over map for more info")
          ),

    column(5, offset = 1, # HTML Selector menu
        selectInput(inputId = "alertType" , label = "Which Alert Type?"
                              ,choices = c("Total" = "Total"
                                           ,"AMBER Alert" = "AMBER"
                                           ,"Flash Flood" = "FlashFlood"
                                           ,"Hurricane" = "Hurricane"
                                           ,"Tornado" = "Tornado"
                                           ,"Other" = "Other")
        )
    ),
    #### Instructions ####
    fluidRow(column(10, offset = 1,
        p("The map below is a county-by county list
            of Wireless Emergency Alert (WEA)
            messages backed up by the PBS WARN system.
            Mouse over a county to see how many alerts
            PBS WARN has protected since May, 2014.
            Select a type of alert to filter the list.
            Click on a county to see a list of all
            WARN alerts passed by PBS and your local
            Non-Commercial Station."),
        p("Don't forget to explore Alaska, Hawaii, and Puerto Rico!")
    )
    ),

  # Show choropleth of selected alerts
  fluidRow(
    column(10, offset = 1,
        leafletOutput("map", height = "600px")
        )
  ),#/fluidRow

  #### list of alerts ####
  fluidRow(
    column(width = 10, offset = 1,
         h4(textOutput("type", inline = TRUE),
            "Warnings:",
            textOutput("county_name", inline = TRUE)
            )),
    column(width = 10, offset = 1,
         DT::dataTableOutput("events")
    )
  )
)
# end ui


allCounties <- left_join(counties_sf, alert_tally)
allCounties[is.na(allCounties)] <- 0


# Define server logic required to draw a choropleth
server <- function(input, output, session) {
  output$type <- renderText(input$alertType)

# Reactive variable fd containing (f)iltered (d)ata
fd <- reactive({
    #browser()
  allCounties %>%
            mutate_(inst = input$alertType)
      })
# Reactive variable containing click_data
  click_data <- reactiveValues(clickedMarker = NULL)

# Default County Name
  output$county_name <- renderText("Full Country")
  output$events <- renderText("Please select a county and alert type.")

 # Base Map
output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    setView(lng = -93.85, lat = 44, zoom = 4)
})

observeEvent(input$alertType, {
    leafletProxy('map') %>%
    clearShapes() %>%
    addPolygons(data = fd()
                , group = input$alertType
                , layerId = ~GEOID
                , stroke = FALSE
                , label = ~paste0("<strong>"
                                  ,NAME
                                  ," "
                                  ,description #lookup table for lsad
                                  ,", "
                                  ,iso_3166_2
                                  ,":</strong><br />"
                                  ,inst
                                  ," "
                                  ,input$alertType
                                  ," WEA Messages") %>%
                  lapply(htmltools::HTML)
                , labelOptions = labelOptions(style = list(
                      "color" = "#2b3e50",
                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                      "text-align" = "left",
                      "font-size" = "17px",
                      "border-color" = "rgba(0,0,0,0.5)"))
                , fillOpacity = .6
                , smoothFactor = .5
                , fillColor = ~pal(inst)
                , highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 1,
                      bringToFront = FALSE)
        )
})
# Store the Map Boundaries on screen
 observeEvent(input$map1_bounds, {
   proxy <- leafletProxy("map") %>%
     setView(input$map1_bounds)
 })

 # Re-title the legend
 observeEvent(input$alertType, {
      proxy <- leafletProxy("map", data = fd()) %>%
        clearControls() %>%
        addLegend(pal = pal
                , values = ~inst
                , opacity = .5
                , title = paste0("Number of ",input$alertType," WEAs")
                , position = "bottomleft")
    })

 # store the clicked county
 observeEvent(input$map_shape_click, {
    click_data$clickedShape <- input$map_shape_click
  })

 # Get the county and state name for that GEOID
     observeEvent(input$map_shape_click, {
       output$county_name <- renderText({
         loc_id = click_data$clickedShape$id %>%
               print()
         filter(allCounties, GEOID == loc_id) %>%
         select(NAME, description, iso_3166_2) %>%
         st_set_geometry(NULL) %>%
         paste(collapse = " ")
       })
    })


 #Create a table with all the events of type in that geoid
   output$events <- renderDataTable({
     county_events = click_data$clickedShape$id %>%
     # print()
     # print(input$alertType)
     #### What are we looking to put in our table?
     if (is.null(click_data$clickedShape$id)) { ## Full Country
         if (input$alertType == "Total") { ## Full country, all alerts (default)
                 left_join(fips_msg, msg2)  %>%
                 arrange(desc(rec_time)) %>%
                 transmute(`Alert Received` =
                               paste0(month(rec_time,label = TRUE, abbr = TRUE)
                                      , " ", day(rec_time),", "
                                      , year(rec_time) )
                           , `Message Text` = wea
                           , `Affected Areas` = areas)
         } else { ## Full country, chose a category
         fips_msg %>%
            left_join(msg2) %>%
            filter(type == input$alertType) %>%
            arrange(desc(rec_time)) %>%
            transmute(`Alert Received` =
                           paste0(month(rec_time,label = TRUE, abbr = TRUE)
                                  , " ", day(rec_time),", "
                                  , year(rec_time) )
                       , `Message Text` = wea
                       , `Affected Areas` = areas)}
     } else { ## Single County, all alerts
         if (input$alertType == "Total") {
            filter(fips_msg, GEOID == county_events) %>%
            left_join(msg2)  %>%
                 arrange(desc(rec_time)) %>%
                 transmute(`Alert Received` =
                               paste0(month(rec_time,label = TRUE, abbr = TRUE)
                                      , " ", day(rec_time),", "
                                      , year(rec_time) )
                           , `Message Text` = wea
                           , `Affected Areas` = areas)
        } else { ## Single County Alert Category chosen
           filter(fips_msg, GEOID == county_events) %>%
           left_join(msg2) %>%
           filter(type == input$alertType) %>%
                 arrange(desc(rec_time)) %>%
                 transmute(`Alert Received` =
                               paste0(month(rec_time,label = TRUE, abbr = TRUE)
                                      , " ", day(rec_time),", "
                                      , year(rec_time) )
                           , `Message Text` = wea
                           , `Affected Areas` = areas) }
        } %>%
###### Place Output into datatable ######
         datatable(rownames = FALSE,
             options = list(
             initComplete = JS(
                 "function(settings, json) {",
                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                 "}"),
             pageLength = 5,
             lengthMenu = c(5, 10, 15, 20)
             )
        )}
    )
}







# Run the application
shinyApp(ui = ui, server = server)

