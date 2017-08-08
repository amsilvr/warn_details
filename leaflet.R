library(leaflet)
library(tidyverse)
library(lubridate)
library(stringr)
library(rgdal)
library(sf)

if (!exists("alert_tally")) {
    source("CMAS_Clean_shiny.R", echo = TRUE)
}
countyshapes_url <- "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_20m.zip"
if (!dir.exists("data")) {dir.create("data")}
if (!file.exists("data/county_shape_file.zip")) {
    download.file(countyshapes_url
                  , destfile = "data/county_shape_file.zip")}

c_shp <- unzip("data/county_shape_file.zip", exdir = "data")

counties_sf <- read_sf(c_shp[grep("shp$", c_shp)]) %>% #pulls the shp file from the zip
    as.data.frame() %>%
    left_join(state_iso) %>%
    inner_join(lsad_lookup()) %>%
    select(STATEFP, COUNTYFP, GEOID, NAME, description, iso_3166_2, geometry) %>%
    st_sf() %>%
    st_transform('+proj=longlat +datum=WGS84')

long_county <- counties_sf %>%
    transmute(GEOID
            , name = paste0(NAME, " ", description,", ", iso_3166_2)
            , geometry) %>%
        left_join(alert_tally %>%
                    gather("alertType", "value", -GEOID)
                  )
### Remove na's ###
long_county[is.na(long_county)] <- 0

#bins <- c(0, 1, 3, 5, 10, 20, 30, 40, 80, 205)
bins <- c(1,5,10,15,20,25,30,40,50,210)
pal <- colorBin("YlOrRd",
                domain = NULL,
                bins = bins,
                pretty = TRUE,
                na.color = "#fefefe")

p <- leaflet() %>%
        addTiles() %>%
        #addProviderTiles(providers$Stamen.TonerLite, group = "Toner by Stamen") %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
    addPolygons(
                  data = long_county
                , group = ~alertType
                , layerId = ~paste(GEOID, alertType)
                , stroke = FALSE
                , label = ~paste0("<strong>"
                                  ,name
                                  ,":</strong><br />"
                                  , value, " "
                                  , alertType, " Alerts") %>%
                            lapply(htmltools::HTML)
                , labelOptions = labelOptions(style = list(
                    "color" = "#2b3e50",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "text-align" = "left",
                    "font-size" = "17px",
                    "border-color" = "rgba(0,0,0,0.5)"))
                , fillOpacity = .5
                , smoothFactor = .5
                , fillColor = ~pal(value)
                , highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 1,
                    bringToFront = FALSE)) %>%
    # addPopups(data = long_county
    #           , group = ~alertType
    #           #, layerID = ~paste(GEOID, alertType)
    #           , popup = ~paste(GEOID, alertType, value)) %>%
    addLayersControl(baseGroups = c("Total"
                                   , "AMBER"
                                   , "FlashFlood"
                                   , "Hurricane"
                                   , "Tornado"
                                   , "Other")
         , options = layersControlOptions(autoZIndex = FALSE
                                      , collapse = FALSE
                                      , position = "topleft"))  %>%
    addControl("<img src = 'docs/New Logo.png'>", position = "bottomleft") %>%
    addLegend(pal = pal
              , values = bins
              , opacity = .5
              #, group = alertType
              , title = paste0("Number of "," WEAs")
              , position = "topleft")
