fun_exploratory_map <- function(){
  
  map <-
    leaflet(
      options = leafletOptions(minZoom = 6, maxZoom = 20, preferCanvas = TRUE, zoomSnap = 0.5, zoomDelta = 0.5,
                               trackResize = TRUE)
    ) %>%
    addScaleBar() %>%
    setView(lng = -107.3434, lat = 38.99793, zoom = 7.5) %>%
    addMapPane("aoi", zIndex = 355) %>%
    addMapPane("county", zIndex = 350) %>%
    addMapPane("division", zIndex = 350) %>%
    addMapPane("district", zIndex = 350) %>%
    addMapPane("econreg", zIndex = 350) %>%    
    addMapPane("huc10", zIndex = 350) %>%
    addMapPane("huc12", zIndex = 350) %>%
    addMapPane("salinity", zIndex = 350) %>%
    addMapPane("selenium", zIndex = 350) %>%
    addMapPane("streams", zIndex = 351) %>%
    addMapPane("shade", zIndex = 350) %>%
    addMapPane("habitat", zIndex = 350) %>%
    addMapPane("field", zIndex = 353) %>%
    addMapPane("diversion", zIndex = 354) %>%
    addMapPane("structures", zIndex = 354) %>%
    addMapPane("highlight", zIndex = 400) %>%
    addMapPane("highlight_layer", zIndex = 401) %>%
    addProviderTiles("CartoDB.Positron", group = "base_map",
                     options = providerTileOptions(
                       updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                       updateWhenIdle = TRUE           # map won't load new tiles when panning
                     ))
  
  map
}

