fun_update_map_symbology <- function(map, geom_data, symbology){
  
  m <- map %>%
    clearGroup("Fields") %>%
    removeControl("custom_legend") %>%
    removeControl("layer_fields")
  
  layer_column <- layers %>% 
    dplyr::filter(layer_name == symbology) %>%
    dplyr::pull(layer_column)
  
  layer_source <- layers %>% 
    dplyr::filter(layer_name == symbology) %>%
    dplyr::pull(layer_source)
  
  
  new_geom_data <- geom_data
  
  tryCatch({
    
    new_geom_data$fields <- new_geom_data$fields %>%
      dplyr::rename(layer_selected = any_of(layer_column))
    
    types <- unique(new_geom_data$fields$layer_selected) %>% sort() %>% na.omit()
    
    if(stringr::str_detect(symbology, "Field Acreage|Crop Type|Diversion Extent|Irrigation Type")){
      
      palette <- c("#C30010","#FF8E00","#fcf5b1","#FF9AA2",
                   "#B450C8","#CD8ADA", "#732982","#FFFF00",
                   "#737373","#000000")
      
    }else if(stringr::str_detect(symbology, "Impact:|Non-Ag")){
      
      palette <- c("#000000","#e6e1e1")
      
    }else{
      
      position <- which(stringr::str_detect(types %>% tolower(), "unknown"))
      
      palette <- c("#3D1778","#715DAA","#BAB4D9","#FFCDA1","#F7921E","#D35000","#802A07") 
      if(length(position) != 0){
        palette[position] <- "#000000"
      }
      
    }
    
    pal <- colorFactor(palette[1:length(types)],
                       levels = types,
                       na.color = "#e6e1e1",
                       alpha = F)
    palNA <- colorFactor(palette[1:length(types)],
                         levels = types,
                         reverse = F,
                         na.color = NA)
    m %>%
      addPolygons(
        data = new_geom_data$fields,
        layerId = ~field_id,
        group = "Fields",
        color = '#363636',
        opacity = 1,
        fillColor = ~pal(layer_selected),
        fillOpacity = 0.8,
        weight = 0.5,
        label = ~lapply(paste0("<b>Field #", field_id, "</b><br>", layer_selected), htmltools::HTML),
        labelOptions = labelOptions(textsize = "14px"),
        highlightOptions = highlightOptions(
          color = "yellow", weight = 5,
          bringToFront = TRUE
        ),
        options = pathOptions(pane = "field")
      ) %>%
      addLegendCustom(
        labels = c("Diversion","Storage","Stream Gage","Other",new_geom_data$aoi$aoi_name,"Potential System Impacts"),
        colors = c("", "","#0996FF","#0996FF","",""),
        border_colors = c("#18a324","","","","black","black"),
        border_dash = c("solid","solid","solid","solid","solid","solid"),
        radius = c(50, 0, 50, 10, 0, 0),
        background = c("","","","","","repeating-linear-gradient(135deg,#000000 0px,#000000 1px,#ffffff 1px,#ffffff 5px)"),
        border_px = c(3,0,0,0,3,3),
        border_left = c("","7.5px solid transparent","","","",""),
        border_right = c("","7.5px solid transparent","","","",""),
        border_bottom = c("","13px solid #0996FF","","","",""),
        height = c(13,0,13,13,14,14),
        width = c(13,0,13,13,14,14),
        margin_top = c(2,2,2,2,2,2),
        margin_left = c(1,0,1,1,0.5,0.5),
        layer_id = "custom_legend"
      ) %>%
      addLegend(
        position = "bottomleft", opacity = 1,
        pal = palNA, values = ~layer_selected, data = new_geom_data$fields,
        group = "layer_fields", na.label = "", 
        title = paste0(
          symbology,
          "<br><span style = 'font-size: 12px; font-weight:normal'> Source: ",
          layer_source,
          "</span>"
        ),
        layerId = "layer_fields"
      )
  },
  error = function(e){
    
    showNotification("Oops! Something went wrong. Wait a few minutes and try again or
                             email us at <basinscoutsupport@thefreshwatertrust.org>.",
                     type = "error", duration = NULL,
                     id = "layer-error")
    
    # showNotification(type = "error", paste0("Oops!\n", e,"."), id = "layer-error", duration = NULL)
    
  })
  
}





