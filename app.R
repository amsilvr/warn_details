# Setup

library(shiny)
library(tidyverse)
library(lubridate)
library(stringr)
library(sf)
library(leaflet)
library(DT)

## Centers for all states

state_list <- tibble(name = state.name
                     , ctrx = state.center$x
                     , ctry = state.center$y)

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

bins <- c(1,5,10,15,20,25,30,40,50,210)#even distribution
#bins <- c(1, 24, 47, 71, 93, 117, 140, 164, 187, 210) # even intervals
pal <- colorBin("YlOrRd",
                domain = NULL,
                bins = bins,
                pretty = TRUE,
                na.color = "#fefefe")

# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = shinytheme('darkly'),
   # Application title
  tags$style(type = "text/css",
          "html,
             body {width:100%;height:100%;text-align:left;}
             /*.selectize-input { font-size: 32px; background-color:aqua; }
               .selectize-dropdown { font-size: 25px; line-height: 30px; }
               .control-label { font-size: 32px; color: white!important;}
               .shiny-input-container {background-color:black;}*/
             .content::-webkit-scrollbar {display: none;}
             .h {text-align:center!important;}
             td { vertical-aligh:top!important; }"),

     column(5, offset = 1, #Title and instructions
          h1("WARN Alerts by County")
          ),

    column(5, offset = 1, # HTML Selector menu
        h4(selectInput(inputId = "alertType" , label = "Alert Type"
                              ,choices = c("Total" = "Total"
                                           ,"AMBER Alert" = "AMBER"
                                           ,"Flash Flood" = "FlashFlood"
                                           ,"Hurricane" = "Hurricane"
                                           ,"Tornado" = "Tornado"
                                           ,"Other" = "Other")
        ))
    ),
    #### Instructions ####
    fluidRow(column(10, offset = 1,
        p("The map below shows the number of Wireless Emergency Alert (WEA)
            messages transmitted by the PBS WARN system to each county in the.
            United States between May 20, 2014 and ",
            textOutput("last_alert", inline = TRUE),
          "."),
        p("These messages are received by PBS from FEMA's IPAWS-OPEN alert
          aggregator and transmitted from every public television station in the
          country to cellular mobile service providers to protect against a failure of
          the cellular company's internet connection to IPAWS-OPEN."),
        p("This information is provided as a convenience for responders and the public.
          It is not guaranteed to be complete or error-free. Geographic outlines reflect
          the orignators' input target areas, actual alert coverage depends on cellular
          system implementation and may vary."),
        p("For a map of active alerts, please visit ",
          a('warn.pbs.org', href='http://warn.pbs.org'),
          ". For more information about this map or PBS WARN, please contact",
          a('amsilverman@pbs.org', href='mailto://amsilverman@pbs.org'),
          '.'))),

  # choropleth map
  fluidRow(
      column(10, offset = 1,
      h4(textOutput("type", inline = TRUE),
               "Warnings:",
               textOutput("county_name", inline = TRUE))
      )),
  fluidRow(
    column(6, offset = 1,
        leafletOutput("map", height = "600px")),

  #### list of alerts ####

    column(width = 5,
         # plotOutput("pie"), #Nobody likes pie charts!
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
  allCounties %>%
            mutate_(inst = input$alertType)
      })
# Reactive variable containing click_data
  click_data <- reactiveValues(clickedMarker = NULL)

# Default County Name
  output$county_name <- renderText("Full Country")
  output$events <- renderText("Please select a county and alert type.")

# Most recent alert
  output$last_alert <- renderText(paste0(month(max(msg$rec_time),label = TRUE),' ',
                                        day(max(msg$rec_time)),', ',
                                        year(max(msg$rec_time))))

 # Base Map
output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    setView(-98.5, 40,zoom = 4)
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
         loc_id = click_data$clickedShape$id #%>%
               # print()
         filter(allCounties, GEOID == loc_id) %>%
         select(NAME, description, iso_3166_2) %>%
         st_set_geometry(NULL) %>%
         paste(collapse = " ")
       })
    })
 # Create a pie chart of alert types for selected area
 # Commented out because nobody likes pie charts.
     # output$pie <- renderPlot({
     #     smry <- allCounties %>%
     #         as.data.frame() %>%
     #         ungroup() %>%
     #         select(AMBER:Tornado) %>%
     #         summarize_all(sum, na.rm = TRUE) %>%
     #         as_vector()
     #     lbls <- paste(names(smry)
     #                   , round(smry/sum(smry)*100,digits = 2)
     #                   , "%")
     #         pie(smry, labels = lbls, radius = .5)
     # })
     #
     # observeEvent(input$map_shape_click, {
     #     output$pie <- renderPlot({
     #        loc_id = click_data$clickedShape$id
     #        smry <- filter(allCounties, GEOID == loc_id) %>%
     #             as.data.frame() %>%
     #             ungroup() %>%
     #             select(AMBER:Tornado) %>%
     #             as_vector()
     #        lbls <- paste(names(smry)
     #                      , round(smry/sum(smry)*100,digits = 2)
     #                      , "%")
     #        pie(smry, labels = lbls)
     #             pie()
     #
     #     })
     # })


 #Create a table with all the events of type in that geoid
   output$events <- renderDataTable({
     county_events = click_data$clickedShape$id
     alert_type = input$alertType
      # print(county_events)
      # print(input$alertType)
     #### What are we looking to put in our table?
     #### Whole country or single county?
     if (is.null(county_events)) {tmptbl <- fips_msg
     } else {tmptbl <- fips_msg %>% filter(GEOID == county_events)}
     if (alert_type == "Total") {tmptbl <- tmptbl %>% left_join(msg2)
     } else {tmptbl <- tmptbl %>% left_join(msg2) %>%
               filter(type == alert_type)}

     tmptbl <- tmptbl %>%
         distinct(msg_id,.keep_all = TRUE) %>%
         arrange(desc(rec_time)) %>%
         transmute(`Date` = rec_time
                       , `Message Text` = str_replace_all(wea, "\'", "")
                       , `Affected Areas` = areas)

###### Place Output into datatable ######
        datatable(tmptbl,
                  options = list(
                      initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                          "}"),
                      columnDefs = list(list(
                          targets = 2,
                          render = JS(
                              "function(data, type, row, meta) {",
                              "return type === 'display' && data.length > 50 ?",
                              "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                              "}")
                      )),
                      pageLength = 5,
                      lengthMenu = c(5, 10, 25, 50)),
                  class = 'stripe compact',
                  callback = JS('table.draw(false);'),
                  rownames = FALSE,
                  autoHideNavigation = TRUE,
                  selection = "single") %>%
        formatStyle(1:3, verticalAlign = 'top') %>%
        formatDate(1, 'toLocaleDateString')
     }
    )
}

# Run the application
shinyApp(ui = ui, server = server)

