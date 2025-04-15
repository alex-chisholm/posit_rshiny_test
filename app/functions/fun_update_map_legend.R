fun_update_map_legend <- function(map, layers_selected, feature_layer_selected = NULL, feature_layer_selected_stream = NULL, geom,
                                  hydro_condition_selected = NULL){
  
  new_map <- map %>%
    removeControl("legend_boundary") %>%
    removeControl("legend_shade") %>%
    removeControl("legend_habitat") %>%
    removeControl("legend_salinity") %>%
    removeControl("legend_selenium") %>%
    removeControl("legend_streams")  %>%
    removeControl("legend_fields") %>%
    {
      if("West Slope" %in% layers_selected){
        showGroup(., "West Slope")
      }else{
        hideGroup(., "West Slope")
      }
    } %>%
    {
      if("Fields" %in% layers_selected){
        showGroup(., "Fields")
      }else{
        hideGroup(., "Fields") %>%
          removeControl("legend_fields")
      }
    } %>%
    {
      if("County" %in% layers_selected){
        showGroup(., "County")
      }else{
        hideGroup(., "County")
      }
    } %>%
    {
      if("Econ. Region" %in% layers_selected){
        showGroup(., "Econ. Region")
      }else{
        hideGroup(., "Econ. Region")
      }
    } %>%    
    {
      if("Division" %in% layers_selected){
        showGroup(., "Division")
      }else{
        hideGroup(., "Division")
      }
    } %>%
    {
      if("District" %in% layers_selected){
        showGroup(., "District")
      }else{
        hideGroup(., "District")
      }
    } %>%
    {
      if("HUC10" %in% layers_selected){
        showGroup(., "HUC10")
      }else{
        hideGroup(., "HUC10")
      }
    } %>%
    {
      if("HUC12" %in% layers_selected){
        showGroup(., "HUC12")
      }else{
        hideGroup(., "HUC12")
      }
    } %>%
    {
      if("Diversions" %in% layers_selected){
        showGroup(., "Diversions")
      }else{
        hideGroup(., "Diversions")
      }
    }  %>%
    {
      if("Streams" %in% layers_selected){
        showGroup(., "Streams")
      }else{
        hideGroup(., "Streams") %>%
          removeControl("legend_streams")
      }
    } %>%
    {
      if("Shade" %in% layers_selected){
        showGroup(., "Shade")
      }else{
        hideGroup(., "Shade") %>%
          removeControl("legend_shade")
      }
    } %>%
    {
      if("Habitats" %in% layers_selected){
        showGroup(., "Habitats")
      }else{
        hideGroup(., "Habitats")%>%
          removeControl("legend_habitat")
      }
    } %>%
    {
      if("Structures" %in% layers_selected){
        showGroup(., "Structures")
      }else{
        hideGroup(., "Structures")
      }
    }  %>%
    {
      if("Salinity" %in% layers_selected){
        showGroup(., "Salinity")
      }else{
        hideGroup(., "Salinity") %>%
          removeControl("legend_salinity")
      }
    } %>%
    {
      if("Selenium" %in% layers_selected){
        showGroup(., "Selenium")
      }else{
        hideGroup(., "Selenium") %>%
          removeControl("legend_selenium")
      }
    } 
  
  #  stream legend ===============================
  if(!is.null(feature_layer_selected_stream)){
    
    if("Streams" %in% layers_selected){
      
      layer_column <- layers_stream %>% 
        dplyr::filter(layer_name == feature_layer_selected_stream,  stringr::str_detect(string = spatial_context, pattern = "stream")) %>%
        dplyr::pull(layer_column)
      
      
      if(length(layer_column) != 0){
        
        new_geom <- geom
        
        new_geom$streams$layer_selected <- geom$streams[[layer_column]]
        
        values <- new_geom$streams$layer_selected %>% unique() %>% sort(na.last = T)
        
        if(length(na.omit(values)) != 0){
          palette <- c("#000080","#008b8b","#00FF7F","#E75480","#ADD8E6")
          # palette <- c("purple","gold","darkorange","darkred","lightblue")
          n <- length(values)
          palette <- palette[1:n]
          
          pal <- colorFactor(palette,
                             levels = values,
                             na.color = "#CCCCCC",
                             alpha = F)
          palNA <- colorFactor(palette,
                               levels = values,
                               reverse = F,
                               na.color = NA)
          
          new_map <- new_map %>%
            addLegend(
              position = "topright", opacity = 1,
              pal = pal, values = values, 
              group = "legend", na.label = "N/A", 
              title = paste0("<span style = 'font-weight:normal'>Stream Layer</span><br>", feature_layer_selected_stream),
              layerId = "legend_streams"
            )
          
        }
      }
    }
  }
  
  #browser()
  
  # adding legend if feature layer is selected ===============================
  if(!is.null(feature_layer_selected)){ # | !is.null(feature_layer_selected_stream)){
    
    ## legend for fields (new legend) =====================================
    if("Fields" %in% layers_selected){
      
      layer_column <- layers %>% 
        dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "field")) %>%
        dplyr::pull(layer_column)
      
      hydro_text <- ""
      
      if(!is.null(hydro_condition_selected)){
        layer_column_hydro <- layers %>% 
          dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "field")) %>%
          dplyr::pull(layer_column_hydro)
        
        if(hydro_condition_selected != "0"){
          hydro_settings <- app_settings[app_settings$setting_name == "Hydrologic Conditions",]
          choices <- hydro_settings$details[[1]] %>% names()
          values <- sapply(choices, function(x) hydro_settings$details[[1]][[x]]$val) %>% as.numeric()
          hydro_text <- paste0("<br>Hydrologic Condition: ", choices[hydro_condition_selected == values])
        }
        
      }else{
        layer_column_hydro <- NA
      }
      
      
      if(length(layer_column) != 0){
        
        # replace layer column using hydro conditions if available
        if(!is.na(layer_column_hydro)){
          layer_column <- rjson::fromJSON(layer_column_hydro)[[hydro_condition_selected]]
        }
        
        new_geom <- geom
        
        new_geom$fields$layer_selected <- geom$fields[[layer_column]]
        
        values <- new_geom$fields$layer_selected %>% unique() %>% sort(na.last = T)
        
        # choosing colors
        if(layer_column == "pod_extent"){
          palette <- pod_extent_palette
        }else{
          palette <- palette1
        }
        
        # check if legend has []
        contain_brackets <- stringr::str_detect(values, "\\[") %>% sum(na.rm =T)
        # if true find the colors according to the bracket value otherwise keep using color with order warm to cold
        if(contain_brackets > 0){
          values_order <- stringr::str_remove(values, "\\].*") %>% stringr::str_remove("\\[*")
          palette <- palette[values_order]
          values <- stringr::str_remove(values, ".*\\]")
        }else{
          if(length(values) == 1){
            n <- 2
          }else{
            n <- length(values)
          }
          if(layer_column == "pod_extent"){
            palette <- palette[values]
          }else{
            palette <- palette[1:n]
          }
        }
        
        if(length(na.omit(values)) != 0){
          pal <- colorFactor(palette,
                             levels = values,
                             na.color = "#000000",
                             alpha = F)
          palNA <- colorFactor(palette,
                               levels = values,
                               reverse = F,
                               na.color = NA)
          
          new_map <- new_map %>%
            addLegend(
              position = "topright", opacity = 0.8,
              pal = pal, values = values,
              group = "legend", na.label = "N/A",
              title = paste0("<span style = 'font-weight:normal'>Field Layer</span><br>", feature_layer_selected,
                             hydro_text),
              layerId = "legend_fields"
            )
        }
      }
      
    }
    
    
    # boundary legend ========================================================
    boundary_legend <- layers_selected[layers_selected %in% c("District","Division","County","Econ. Region")] %>% tolower()
    boundary_legend <- ifelse(boundary_legend == "econ. region", "econreg", boundary_legend)
    
    #browser()
    
    if(length(boundary_legend) != 0){
      
      #browser()
      
      # update legend
      layer_column <- layers %>% 
        dplyr::filter(layer_name == feature_layer_selected, stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
        dplyr::pull(layer_column)
      
      hydro_text <- ""
      
      if(!is.null(hydro_condition_selected)){
        layer_column_hydro <- layers %>% 
          dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
          dplyr::pull(layer_column_hydro)
        
        if(hydro_condition_selected != "0"){
          hydro_settings <- app_settings[app_settings$setting_name == "Hydrologic Conditions",]
          choices <- hydro_settings$details[[1]] %>% names()
          values <- sapply(choices, function(x) hydro_settings$details[[1]][[x]]$val) %>% as.numeric()
          hydro_text <- paste0("<br>Hydrologic Condition: ", choices[hydro_condition_selected == values])
        }
        
      }else{
        layer_column_hydro <- NA
      }
      
      if(length(layer_column) != 0){
        
        # replace layer column using hydro conditions if available
        if(!is.na(layer_column_hydro)){
          layer_column <- rjson::fromJSON(layer_column_hydro)[[hydro_condition_selected]]
        }
        
        new_geom <- geom
        
        new_geom[[boundary_legend]]$layer_selected <- geom[[boundary_legend]][[layer_column]]
        
        values <- new_geom[[boundary_legend]]$layer_selected %>% unique() %>% sort(na.last = T)
        
        # check if legend has []
        contain_brackets <- stringr::str_detect(values, "\\[") %>% sum(na.rm = T)
        # if true find the colors according to the bracket value
        # otherwise keep using color with order warm to cold
        if(contain_brackets > 0){
          values_order <- stringr::str_remove(values, "\\].*") %>% stringr::str_remove("\\[*")
          palette <- palette1[values_order]
          values <- stringr::str_remove(values, ".*\\]")
        }else{
          if(length(values) == 1){
            n <- 2
          }else{
            n <- length(values)
          }
          palette <- palette1[1:n]
        }
        
        if(length(na.omit(values)) != 0){
          pal <- colorFactor(palette,
                             levels = values,
                             na.color = "#CCCCCC",
                             alpha = F)
          palNA <- colorFactor(palette,
                               levels = values,
                               reverse = F,
                               na.color = NA)
          
          new_map <- new_map %>%
            addLegend(
              position = "topright", opacity = 0.6,
              pal = pal, values = values,
              group = "legend", na.label = "N/A",
              title = paste0("<span style = 'font-weight:normal'>",stringr::str_to_title(boundary_legend)," Boundary Layer</span>",
                             "<br>", feature_layer_selected,
                             hydro_text),
              layerId = "legend_boundary"
            )
        }
      }
      
      
    }
  }
  
  # shade legend ========================================================
  if("Shade" %in% layers_selected){
    
    new_geom <- geom
    
    values <- new_geom$shade %>% dplyr::distinct(qt_descr, color)
    
    if(nrow(na.omit(values)) != 0){
      pal <- colorFactor(values$color,
                         levels = as.character(values$qt_descr),
                         na.color = "#CCCCCC",
                         alpha = F)
      palNA <- colorFactor(values$color,
                           levels = as.character(values$qt_descr),
                           reverse = F,
                           na.color = NA)
      
      new_map <- new_map %>%
        addLegend(
          position = "topright", opacity = 0.8,
          pal = pal, values = values$qt_descr,
          group = "legend", na.label = "",
          title = paste0("<span style = 'font-weight:normal'>"," Shade: August kcal/day</span>"),
          layerId = "legend_shade"
        )
    }
  }
  
  # habitats legend ========================================================
  if("Habitats" %in% layers_selected){
    
    if(!is.null(geom$habitat)){
      
      new_geom <- geom
      
      values <- new_geom$habitat %>% dplyr::distinct(habitat_type, color)
      
      if(nrow(na.omit(values)) != 0){
        pal <- colorFactor(values$color,
                           levels = as.character(values$habitat_type),
                           na.color = "#CCCCCC",
                           alpha = F)
        palNA <- colorFactor(values$color,
                             levels = as.character(values$habitat_type),
                             reverse = F,
                             na.color = NA)
        
        new_map <- new_map %>%
          addLegend(
            position = "topright", opacity = 0.8,
            pal = pal, values = values$habitat_type,
            group = "legend", na.label = "",
            title = paste0("<span style = 'font-weight:normal'>","Habitat Types</span>"),
            layerId = "legend_habitat"
          )
      }
      
    }
  }
  
  
  # salinity legend ========================================================
  if("Salinity" %in% layers_selected){
    
    if(!is.null(geom$salinity)){
      
      new_geom <- geom
      
      values <- new_geom$salinity %>% dplyr::distinct(salinity_type, color)
      
      if(nrow(na.omit(values)) != 0){
        pal <- colorFactor(values$color,
                           levels = as.character(values$salinity_type),
                           na.color = "#CCCCCC",
                           alpha = F)
        palNA <- colorFactor(values$color,
                             levels = as.character(values$salinity_type),
                             reverse = F,
                             na.color = NA)
        
        new_map <- new_map %>%
          addLegend(
            position = "topright", opacity = 0.8,
            pal = pal, values = values$salinity_type,
            group = "legend", na.label = "",
            title = paste0("<span style = 'font-weight:normal'>","Salinity Types</span>"),
            layerId = "legend_salinity"
          )
      }
    }
    
  }

  # selenium legend ========================================================
  if("Selenium" %in% layers_selected){
    
    if(!is.null(geom$selenium)){
      
      new_geom <- geom
      
      values <- new_geom$selenium %>% dplyr::distinct(selenium_type, color)
      
      if(nrow(na.omit(values)) != 0){
        pal <- colorFactor(values$color,
                           levels = as.character(values$selenium_type),
                           na.color = "#CCCCCC",
                           alpha = F)
        palNA <- colorFactor(values$color,
                             levels = as.character(values$selenium_type),
                             reverse = F,
                             na.color = NA)
        
        new_map <- new_map %>%
          addLegend(
            position = "topright", opacity = 0.8,
            pal = pal, values = values$selenium_type,
            group = "legend", na.label = "",
            title = paste0("<span style = 'font-weight:normal'>","Selenium Types</span>"),
            layerId = "legend_selenium"
          )
      }
    }
    
  }  
  
  
  # output
  new_map
  
  
}