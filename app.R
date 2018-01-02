#### Remaining to-do list ####

# x Dropdown to go to full country
# x Dropdown to select specific state
# X change on state dropdown should select whole state for tabular results
# X Map changes on date selection as well as type selection
# x Dynamic coloring for legend and map
# Histogram of alert types
# Click an alert in the table and have it highlighted on the map -

#### Setup ####

library(shiny)
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

state_sf <- map_states()

counties_sf <- read_sf(c_shp[grep("shp$", c_shp)]) %>%
  as.data.frame() %>% #to fix July 25 problem with the join.sf methods
  inner_join(lsad_lookup()) %>%
    select(STATEFP, COUNTYFP, GEOID, NAME, description, geometry) %>%
    left_join(state_sf %>% select(STATEFP, STUSPS)) %>%
  st_sf(sf_column_name = 'geometry') %>%
  st_transform('+proj=longlat +datum=WGS84')


### Define UI ####
ui <- fluidPage(
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


    fluidRow(column(8, offset = 1, #Title
          h1("WARN Alerts by County"),
          h2(textOutput("type", inline = TRUE),
                "Warnings:",
                textOutput("county_name", inline = TRUE))
          )
          ),
    fluidRow( #Controls
        column(3, offset = 1, #Date Filter
           dateRangeInput(inputId = "dateRange", label = "Date Range"
                       ,start = date(min(msg2$rec_time))
                       ,end = date(max(msg2$rec_time))
                       ,format = 'M d, yyyy')

           ),

        column(3, # HTML Selector menu
        selectInput(inputId = "alertType" , label = "Alert Type"
                              ,choices = c("Total" = "Total"
                                           ,"AMBER Alert" = "AMBER"
                                           ,"Flash Flood" = "FlashFlood"
                                           ,"Hurricane" = "Hurricane"
                                           ,"Tornado" = "Tornado"
                                           ,"Other" = "Other")
            )

        ),
        column(3, # State selector
               selectInput(inputId = 'state', label = 'State or Region',
                           list(`Full Country` = c('Full Country', 'Continental US'),
                                 `State` =  state_sf %>%
                                    select(NAME) %>%
                                    arrange(NAME))))
        ),

    #### Instructions ####
    fluidRow(column(10, offset = 1,
        p("The map below shows the number of Wireless Emergency Alert (WEA)
            messages transmitted by the PBS WARN system to each county in the
            United States between May 20, 2014 and ",
           ## Change the dates here to reflect the input$dateRange ##
           textOutput("last_alert", inline = TRUE),
          "."),
        p("Messages are received by PBS from FEMA's IPAWS-OPEN alert
          aggregator, and then rebroadcast from every public television station in the
          country to cellular mobile service providers. This alternate path for WEA
          protects WEA messages against an internet connection failure."),
        p("This map is provided as a convenience for responders, researchers, and the public.
          It is not guaranteed to be complete or error-free. Geographic outlines reflect
          the orignators' input target areas, actual alert coverage depends on cellular
          system implementation and may vary."),
        p("For a map of currently active alerts, please visit ",
          a('warn.pbs.org', href='http://warn.pbs.org'),
          ". For more information about PBS WARN, please visit ",
          a('pbs.org/about/WARN', href='http://www.pbs.org/about/contact-information/warn/'),
         " or email ",
           a('Aaron Silverman', href='mailto://amsilverman@pbs.org'),
          '.'))),

  # choropleth map
  fluidRow(
      column(10, offset = 1#,
      )),
  fluidRow(
    column(6, offset = 1,
        leafletOutput("map", height = "600px")),

  #### list of alerts ####

    column(width = 5,
        DT::dataTableOutput("events")
    )
  )
)

# Define server logic required to draw choropleth and datatable
server <- function(input, output, session) {
  output$type <- renderText(input$alertType)


# Reactive variable fd containing (f)iltered (d)ata
fd <- reactive({

  allCounties <- left_join(counties_sf, tally_alerts(msg2, fips_msg,
                                                       input$dateRange[1],
                                                       input$dateRange[2]))
  allCounties[is.na(allCounties)] <- 0
  allCounties %>%
            mutate_(inst = input$alertType)
      })


# Reactive variable containing click_data
  click_data <- reactiveValues(clickedMarker = NULL)

# Default County Name
  output$county_name <- renderText("Full Country")
  output$events <- renderText("Please select a state and alert type.
                              Click on a county for a list of events in that county.")

# Most recent alert
  output$last_alert <- renderText(paste0(month(max(msg$rec_time),label = TRUE),' ',
                                        day(max(msg$rec_time)),', ',
                                        year(max(msg$rec_time))))

 # Base Map ####
output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
       setView(-98.5, 40,zoom = 4)
})
## When you pick a full state, clear the clicked county
observeEvent(input$state,{
    quostate <- quo(input$state)
    click_data$clickedShape$id <- NULL


    output$county_name <- renderText({
        input$state
    })


    ## Clear county if we select a state or the country ##
    #click_data <- clickedMarker = NULL

    if (input$state == "Full Country") {
        bounds <- list(-179.1505, 17.91377, -66.885444, 49.384358)
        leafletProxy('map') %>%
            removeShape(layerId = 'stateSelection') %>%
            fitBounds(lng1 = bounds[[1]],
                      lat1 = bounds[[2]],
                      lng2 = bounds[[3]],
                      lat2 = bounds[[4]])
    } else if(input$state == "Continental US") {
        bounds <- list(-124.848974, 24.396308, -66.885444, 49.384358)
        leafletProxy('map') %>%
            removeShape(layerId = 'stateSelection') %>%
            fitBounds(lng1 = bounds[[1]],
                      lat1 = bounds[[2]],
                      lng2 = bounds[[3]],
                      lat2 = bounds[[4]])
    } else if(input$state == "Alaska") { ##automatic bounds don't work here
      bounds <- list(-179.1505, 51.2097, -129.9795, 71.4410)
      brdr  <- state_sf %>%
          filter(NAME == 'Alaska') %>%
          st_sf()
      leafletProxy('map') %>%
          addPolygons(data = brdr,
                      layerId = 'stateSelection',
                      color = '#000',
                      weight = 3,
                      fill = FALSE) %>%
          fitBounds(lng1 = bounds[[1]],
                    lat1 = bounds[[2]],
                    lng2 = bounds[[3]],
                    lat2 = bounds[[4]]
          )
    } else {
        bounds <- state_sf %>% st_sf() %>%
            filter(NAME == !!quostate) %>%
            select(geometry) %>%
            st_bbox()
        brdr  <- state_sf %>%
            filter(NAME == !!quostate) %>%
            st_sf()
        leafletProxy('map') %>%
            addPolygons(data = brdr,
                        layerId = 'stateSelection',
                        color = '#000',
                        weight = 3,
                        fill = FALSE) %>%
            fitBounds(lng1 = bounds[[1]],
                      lat1 = bounds[[2]],
                      lng2 = bounds[[3]],
                      lat2 = bounds[[4]]
            )
    }
    print(bounds)


})
 # Alert Type ####

observeEvent(input$alertType, {
    # bins and pallette
    bins <- c(unique(quantile(fd()$inst, probs = seq(0,1,.12))),max(fd()$inst))
    pal <- colorBin("YlOrRd",
                    domain = NULL,
                    bins = bins,
                    pretty = FALSE,
                    na.color = "#fefefe")
    leafletProxy('map') %>%
#   clearShapes() %>%
    addPolygons(data = fd()
                , group = input$alertType
                , layerId = ~GEOID
                , stroke = FALSE
                , label = ~paste0("<strong>"
                                  ,NAME
                                  ," "
                                  ,description #lookup table for lsad
                                  ,", "
                                  ,STUSPS
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
# Store the Map Boundaries on screen so map doesn't reset when new filters applied
 observeEvent(input$map1_bounds, {
   proxy <- leafletProxy("map") %>%
     setView(input$map1_bounds)
 })

 # Re-title the legend
 observeEvent(input$alertType, {
     # bins and pallette
     bins <- c(unique(quantile(fd()$inst, probs = seq(0,1,.12))),max(fd()$inst))
     pal <- colorBin("YlOrRd",
                     domain = NULL,
                     bins = bins,
                     pretty = TRUE,
                     na.color = "#fefefe")
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
     # Get the county name for the clicked county
     observeEvent(input$map_shape_click, {
       output$county_name <- renderText({
         loc_id = click_data$clickedShape$id
         filter(fd(), GEOID == loc_id) %>%
         select(NAME, description, STUSPS) %>%
         st_set_geometry(NULL) %>%
         paste(collapse = " ")
       })
    })

#Create a table with all the events of type in that geoid
   output$events <- renderDataTable({
     county_events = click_data$clickedShape$id
     quocounty = quo(county_events)
     state = input$state
     quostate = quo(state)
     alert_type = input$alertType

       message(paste0('selected county is ',county_events))
       message(paste0('selected state is ',state))
       message(paste0('alert type is ',alert_type))
       print(input$dateRange)
     #### What are we looking to put in our table?
     #### Whole country or single county?
         if (is.null(county_events)) { ## All cases where we haven't selected a county
             if (state == 'Full Country') {
                 tmptbl <- fips_msg
             } else if (state == 'Continental US') {
                 tmptbl <- fips_msg %>%
                     mutate(stfps = str_extract(GEOID, '^[0-9]{2}')) %>%
                     filter(!(stfps %in% c('02', '15', '72'))) %>%
                     select(-stfps)
             } else{tmptbl <- fips_msg %>% ## Get all messages from given state
                    mutate(stfps = str_extract(GEOID, '^[0-9]{2}')) %>%
                    filter(stfps == map_states() %>%
                                        filter(NAME == !!quostate) %>%
                                            select(STATEFP) %>%
                                            as.character()) %>%
                    select(-stfps)}
        } else {tmptbl <- fips_msg %>%
                filter(GEOID == !!quocounty)
        }
     #### All alerts or specific type?
     if (alert_type == 'Total') {
         tmptbl <- tmptbl %>%
             left_join(msg2)
     } else {
        tmptbl <- tmptbl %>%
            left_join(msg2) %>%
            filter(type == alert_type)}

       tmptbl <- tmptbl %>%
            distinct(msg_id,.keep_all = TRUE) %>%
            arrange(desc(rec_time)) %>%
            transmute(`Date` = rec_time
                           , `Message Text` = str_replace_all(wea, "\'", "")
                           , `Affected Areas` = areas) %>%
            filter(Date >= min(input$dateRange)) %>%
            filter(Date <= max(input$dateRange) + 1)


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
                      pageLength = 10,
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

