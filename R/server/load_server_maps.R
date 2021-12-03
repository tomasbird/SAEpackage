
## Leaflet map to show boundaries and data distribution

output$map <- renderLeaflet({
  leaflet() %>%
    # base layers
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    
    # adm areas
    addPolygons(data=adm0, group="National", weight = 2, color = "red") %>%
    addPolygons(data=districts, group="Districts", weight = 2, color = "blue") %>%
    addPolygons(data=adm1, group="Admin 1", weight = 2, color = "yellow") %>%
    
    # sample locations
    addCircleMarkers(data=clusters, group="DHS clusters", radius=2,
                     clusterOptions = markerClusterOptions(freezeAtZoom = FALSE)) %>%
    
    # add controls
    addLayersControl(
      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
      overlayGroups = c("National", "Sub-Regions", "Districts"),
      options = layersControlOptions(collapsed = FALSE),
      position="bottomleft"
    ) %>%
    
    # set viewing area
    setView(lng = 84.61, lat = 28.033, zoom = 7)
})
