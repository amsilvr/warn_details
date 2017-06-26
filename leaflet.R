library(leaflet)
library(tidyverse)

long_county <- alert_tally %>%
        gather("alertType", "value", -GEOID) %>%
        left_join(counties_sf) %>%
        ungroup() %>%
    st_sf()
    
    
p <- leaflet(data = long_county) %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
    addPolygons(group = ~alertType, 
                 layerId = ~GEOID
                , stroke = FALSE
                # , label = ~paste0("<strong>"
                #                   ,NAME
                #                   ," "
                #                   ,description #lookup table for lsad
                #                   ,", "
                #                   ,iso_3166_2
                #                   ,":</strong><br />"
                #                   ,inst
                #                   ," "
                #                   ,input$alertType
                #                   ," WEA Messages") %>% 
                #     lapply(htmltools::HTML)
                # , labelOptions = labelOptions(style = list(
                #     "color" = "#2b3e50",
                #     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                #     "text-align" = "left",
                #     "font-size" = "17px",
                #     "border-color" = "rgba(0,0,0,0.5)"))
                , fillOpacity = .2
                , smoothFactor = .5
                # , fillColor = ~pal(inst)
                # , highlight = highlightOptions(
                #     weight = 5,
                #     color = "#666",
                #     dashArray = "",
                #     fillOpacity = 1,
                #     bringToFront = FALSE)
    )
# addLayersControl(
#     alertTypeGroups = c("Total" = "Total"
#                         ,"AMBER Alert" = "AMBER"
#                         ,"Flash Flood" = "FlashFlood"
#                         ,"Hurricane" = "Hurricane"
#                         ,"Tornado" = "Tornado"
#                         ,"Other" = "Other")
#     , options = layersControlOptions(collapsed = FALSE)
# )