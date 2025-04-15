fun_update_exploratory_map <- function(map, geom, theme_selected, feature_layer_selected = NULL, feature_layer_stream_selected = NULL,
                                       hydro_condition_selected = NULL, layers_options = NULL){
  
  theme_options <- themes %>%
    dplyr::filter(theme == theme_selected)
  
  if(!is.null(layers_options)){
    theme_options <- fun_layers_options_update(theme_options, layers_options)
  }  
  
  map <- map %>%
    clearGroup("West Slope") %>%
    clearGroup("Fields") %>%
    clearGroup("County") %>%
    clearGroup("Econ. Region") %>%
    clearGroup("District") %>%
    clearGroup("Division") %>%
    clearGroup("HUC10") %>%
    clearGroup("HUC12") %>%
    clearGroup("Streams") %>%
    clearGroup("Shade") %>%
    clearGroup("Habitats") %>%
    clearGroup("Diversions") %>%
    clearGroup("Structures") %>%
    clearGroup("Salinity") %>%
    clearGroup("Selenium") %>%
    clearMarkerClusters()
  # %>%
  #   removeControl("legend_fields") %>%
  #   removeControl("legend_district") %>%
  #   removeControl("legend_county")
  
  # draw aoi ==================================================================
  if(theme_options$has_aoi){
    
    map <- map %>%
      addPolylines(
        data = geom$aoi,
        layerId = ~id,
        group = "West Slope",
        color = 'black',
        opacity = 1,
        weight = 2,
        options = pathOptions(pane = "aoi")
      )
    
  }
  
  # draw fields ===============================================================
  if(theme_options$has_field){
    
    layer_column <- layers %>% 
      dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "field")) %>%
      dplyr::pull(layer_column)
    
    if(!is.null(hydro_condition_selected)){
      layer_column_hydro <- layers %>% 
        dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "field")) %>%
        dplyr::pull(layer_column_hydro)
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
      
      values <- new_geom$fields$layer_selected %>% unique() %>% sort()
      
      # choosing colors
      if(layer_column == "pod_extent"){
        palette <- pod_extent_palette
      }else{
        palette <- palette1
      }
      # check if legend has []
      contain_brackets <- stringr::str_detect(values, "\\[") %>% sum()
      # if true find the colors according to the bracket value
      # otherwise keep using color with order warm to cold
      if(contain_brackets > 0){
        values_order <- stringr::str_remove(values, "\\].*") %>% stringr::str_remove("\\[*")
        palette <- palette[values_order]
        values <- stringr::str_remove(values, ".*\\]")
        new_geom$fields$layer_selected <- stringr::str_remove(new_geom$fields$layer_selected, ".*\\]")
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
      
      pal <- colorFactor(palette,
                         levels = values,
                         na.color = "#000000",
                         alpha = F)
      palNA <- colorFactor(palette,
                           levels = values,
                           reverse = F,
                           na.color = NA)
      
      map <- map %>%
        addPolygons(
          data = new_geom$fields,
          layerId = ~field_id,
          group = "Fields",
          color = ~pal(layer_selected),
          opacity = 0.5,
          fillColor = ~pal(layer_selected),
          fillOpacity = 0.8,
          dashArray = 1,
          weight = 1,
          highlightOptions = highlightOptions(
            color = "orangered", weight = 3,
            bringToFront = FALSE
          ),
          label = ~lapply(paste0("<span style = 'color:", pal(layer_selected),
                                 ";'><i class = 'fa-solid fa-square'></i></span> <b>",pod_list,"</b>", 
                                 "<br>Acreage: ", acres,
                                 #"<br>Land Use: ", land_use,
                                 #"<br>Irrigation Type: ", irrig_type,
                                 "<br>Fields: ", fields
                                 #"<br>CU Vol. (af): ", cu_avg_acft,
                                 #"<br>Diversion Vol. (af): ", div_acft
          ), htmltools::HTML),
          labelOptions = labelOptions(textsize = "14px"),
          options = pathOptions(pane = "field")
        ) #%>%
      # addLegend(
      #   position = "topright", opacity = 0.8,
      #   pal = palNA, values = values,
      #   group = "Fields", na.label = "", 
      #   title = paste0("<span style = 'font-weight:normal'>Feature Layer</span><br>", feature_layer_selected),
      #   layerId = "legend_fields"
      # )
    }
  }
  
  # draw county ===============================================================
  if(theme_options$has_county){
    
    if(is.null(feature_layer_selected)){
      map <- map %>%
        addPolygons(
          data = geom$county,
          layerId = ~boundary_type_geom_id,
          group = "County",
          color = "#715DAA",
          fillOpacity = 0,
          dashArray = 1,
          opacity = 1,
          weight = 2,
          highlightOptions = highlightOptions(
            color = "#715DAA", weight = 5,
            bringToFront = FALSE
          ),
          label = ~lapply(paste0("<span style = 'color:#715DAA'><b>",boundary_name,"</b></span><br>", boundary_name), htmltools::HTML),
          labelOptions = labelOptions(textsize = "14px"),
          options = pathOptions(pane = "county")
        )
      
    }else{
      
      layer_column <- layers %>% 
        dplyr::filter(layer_name == feature_layer_selected, stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
        dplyr::pull(layer_column)
      
      if(!is.null(hydro_condition_selected)){
        layer_column_hydro <- layers %>% 
          dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
          dplyr::pull(layer_column_hydro)
      }else{
        layer_column_hydro <- NA
      }
      
      if(length(layer_column) != 0){
        
        # replace layer column using hydro conditions if available
        if(!is.na(layer_column_hydro)){
          layer_column <- rjson::fromJSON(layer_column_hydro)[[hydro_condition_selected]]
        }
        
        new_geom <- geom
        
        new_geom$county$layer_selected <- geom$county[[layer_column]]
        
        values <- new_geom$county$layer_selected %>% unique() %>% sort()
        
        # check if legend has []
        contain_brackets <- stringr::str_detect(values, "\\[") %>% sum()
        # if true find the colors according to the bracket value
        # otherwise keep using color with order warm to cold
        if(contain_brackets > 0){
          values_order <- stringr::str_remove(values, "\\].*") %>% stringr::str_remove("\\[*")
          palette <- palette1[values_order]
          values <- stringr::str_remove(values, ".*\\]")
          new_geom$county$layer_selected <- stringr::str_remove(new_geom$county$layer_selected, ".*\\]")
        }else{
          if(length(values) == 1){
            n <- 2
          }else{
            n <- length(values)
          }
          palette <- palette1[1:n]
        }
        
        
        pal <- colorFactor(palette,
                           levels = values,
                           na.color = "#e6e1e1",
                           alpha = F)
        palNA <- colorFactor(palette,
                             levels = values,
                             reverse = F,
                             na.color = NA)
        
        map <- map %>%
          addPolygons(
            data = new_geom$county,
            layerId = ~boundary_type_geom_id,
            group = "County",
            color = ~pal(layer_selected),
            fillColor = ~pal(layer_selected),
            dashArray = 1,
            opacity = 1,
            fillOpacity = 0.6,
            weight = 2,
            highlightOptions = highlightOptions(
              color = "#715DAA", weight = 5,
              bringToFront = FALSE
            ),
            
            label = ~lapply(paste0("<span style = 'color:#715DAA'><b>",boundary_name,"</b></span>"
                                   #"<br>Dom. Land Use: ", land_use,
                                   #"<br>Dom. Irrigation: ", irrig_type
            ), htmltools::HTML),
            
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "county")
          )
        
      }
      
    }
    
  }
  
  # draw econreg ===============================================================
  if(theme_options$has_econreg){
    
    if(is.null(feature_layer_selected)){
      
      map <- map %>%
        addPolygons(
          data = geom$econreg,
          layerId = ~boundary_type_geom_id,
          group = "Econ. Region",
          color = "#666666",
          fillOpacity = 0,
          dashArray = 1,
          opacity = 1,
          weight = 2,
          highlightOptions = highlightOptions(
            color = "#666666", weight = 5,
            bringToFront = FALSE
          ),
          label = ~lapply(paste0("<span style = 'color:#666666'><b>Econ. Region</b></span><br>", boundary_name), htmltools::HTML),
          labelOptions = labelOptions(textsize = "14px"),
          options = pathOptions(pane = "econreg")
        )
      
    }else{
      
      layer_column <- layers %>% 
        dplyr::filter(layer_name == feature_layer_selected, stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
        dplyr::pull(layer_column)
      
      if(!is.null(hydro_condition_selected)){
        layer_column_hydro <- layers %>% 
          dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
          dplyr::pull(layer_column_hydro)
      }else{
        layer_column_hydro <- NA
      }
      
      if(length(layer_column) != 0){
        
        # replace layer column using hydro conditions if available
        if(!is.na(layer_column_hydro)){
          layer_column <- rjson::fromJSON(layer_column_hydro)[[hydro_condition_selected]]
        }
        
        new_geom <- geom
        
        new_geom$econreg$layer_selected <- geom$econreg[[layer_column]]
        
        values <- new_geom$econreg$layer_selected %>% unique() %>% sort()
        
        # check if legend has []
        contain_brackets <- stringr::str_detect(values, "\\[") %>% sum()
        # if true find the colors according to the bracket value
        # otherwise keep using color with order warm to cold
        if(contain_brackets > 0){
          values_order <- stringr::str_remove(values, "\\].*") %>% stringr::str_remove("\\[*")
          palette <- palette1[values_order]
          values <- stringr::str_remove(values, ".*\\]")
          new_geom$econreg$layer_selected <- stringr::str_remove(new_geom$econreg$layer_selected, ".*\\]")
        }else{
          if(length(values) == 1){
            n <- 2
          }else{
            n <- length(values)
          }
          palette <- palette1[1:n]
        }
        
        
        pal <- colorFactor(palette,
                           levels = values,
                           na.color = "#e6e1e1",
                           alpha = F)
        palNA <- colorFactor(palette,
                             levels = values,
                             reverse = F,
                             na.color = NA)
        
        map <- map %>%
          addPolygons(
            data = new_geom$econreg,
            layerId = ~boundary_type_geom_id,
            group = "Econ. Region",
            color = ~pal(layer_selected),
            fillColor = ~pal(layer_selected),
            dashArray = 1,
            opacity = 1,
            fillOpacity = 0.6,
            weight = 2,
            highlightOptions = highlightOptions(
              color = "#666666", weight = 5,
              bringToFront = FALSE
            ),
            label = ~lapply(paste0("<span style = 'color:#666666'><b>",boundary_name,"</b></span>"
                                   #"<br>Dom. Land Use: ", land_use,
                                   #"<br>Dom. Irrigation: ", irrig_type
            ), htmltools::HTML),
            
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "econreg")
          )
        
      }
    }
    
  }
  
  # draw district ============================================================
  if(theme_options$has_district){
    
    if(is.null(feature_layer_selected)){
      
      map <- map %>%
        addPolygons(
          data = geom$district,
          layerId = ~boundary_type_geom_id,
          group = "District",
          color = "#3573B9",
          dashArray = 1,
          opacity = 1,
          fillOpacity = 0,
          weight = 2,
          highlightOptions = highlightOptions(
            color = "#3573B9", weight = 5,
            bringToFront = FALSE
          ),
          label = ~lapply(paste0("<span style = 'color:#3573B9'><b>",boundary_name,"</b></span>"
                                 #"<br>Dom. Land Use: ", land_use,
                                 #"<br>Dom. Irrigation: ", irrig_type
          ), htmltools::HTML),
          labelOptions = labelOptions(textsize = "14px"),
          options = pathOptions(pane = "district")
        )
      
    }else{
      
      layer_column <- layers %>% 
        dplyr::filter(layer_name == feature_layer_selected, stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
        dplyr::pull(layer_column)
      
      if(!is.null(hydro_condition_selected)){
        layer_column_hydro <- layers %>% 
          dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
          dplyr::pull(layer_column_hydro)
      }else{
        layer_column_hydro <- NA
      }
      
      if(length(layer_column) != 0){
        
        # replace layer column using hydro conditions if available
        if(!is.na(layer_column_hydro)){
          layer_column <- rjson::fromJSON(layer_column_hydro)[[hydro_condition_selected]]
        }
        
        new_geom <- geom
        
        new_geom$district$layer_selected <- geom$district[[layer_column]]
        
        values <- new_geom$district$layer_selected %>% unique() %>% sort()
        
        # check if legend has []
        contain_brackets <- stringr::str_detect(values, "\\[") %>% sum()
        # if true find the colors according to the bracket value
        # otherwise keep using color with order warm to cold
        if(contain_brackets > 0){
          values_order <- stringr::str_remove(values, "\\].*") %>% stringr::str_remove("\\[*")
          palette <- palette1[values_order]
          values <- stringr::str_remove(values, ".*\\]")
          new_geom$district$layer_selected <- stringr::str_remove(new_geom$district$layer_selected, ".*\\]")
        }else{
          if(length(values) == 1){
            n <- 2
          }else{
            n <- length(values)
          }
          palette <- palette1[1:n]
        }
        
        
        pal <- colorFactor(palette,
                           levels = values,
                           na.color = "#e6e1e1",
                           alpha = F)
        palNA <- colorFactor(palette,
                             levels = values,
                             reverse = F,
                             na.color = NA)
        
        map <- map %>%
          addPolygons(
            data = new_geom$district,
            layerId = ~boundary_type_geom_id,
            group = "District",
            color = ~pal(layer_selected),
            fillColor = ~pal(layer_selected),
            dashArray = 1,
            opacity = 1,
            fillOpacity = 0.6,
            weight = 2,
            highlightOptions = highlightOptions(
              color = "#3573B9", weight = 5,
              bringToFront = FALSE
            ),
            label = ~lapply(paste0("<span style = 'color:#3573B9'><b>",boundary_name,"</b></span>"
                                   #"<br>Dom. Land Use: ", land_use,
                                   #"<br>Dom. Irrigation: ", irrig_type
            ), htmltools::HTML),
            
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "district")
          )
        
      }
    }
    
  }
  
  # draw division OLD ============================================================
  if(FALSE & theme_options$has_division){
    
    map <- map %>%
      addPolygons(
        data = geom$division,
        layerId = ~boundary_type_geom_id,
        group = "Division",
        color = '#EA4C3B',
        dashArray = 1,
        opacity = 1,
        fillOpacity = 0,
        weight = 2,
        highlightOptions = highlightOptions(
          color = "#EA4C3B", weight = 5,
          bringToFront = FALSE
        ),
        label = ~lapply(paste0("<span style = 'color:#EA4C3B'><b>",boundary_name,"</b></span><br>", boundary_name), htmltools::HTML),
        labelOptions = labelOptions(textsize = "14px"),
        options = pathOptions(pane = "division")
      )
    
  }
  
  # draw division NEW ============================================================
  if(theme_options$has_division){
    
    if(is.null(feature_layer_selected)){    
      map <- map %>%
        addPolygons(
          data = geom$division,
          layerId = ~boundary_type_geom_id,
          group = "Division",
          color = '#EA4C3B',
          dashArray = 1,
          opacity = 1,
          fillOpacity = 0,
          weight = 2,
          highlightOptions = highlightOptions(
            color = "#EA4C3B", weight = 5,
            bringToFront = FALSE
          ),
          label = ~lapply(paste0("<span style = 'color:#EA4C3B'><b>",boundary_name,"</b></span><br>", boundary_name), htmltools::HTML),
          labelOptions = labelOptions(textsize = "14px"),
          options = pathOptions(pane = "division")
        )
      
    }else{
      
      layer_column <- layers %>% 
        dplyr::filter(layer_name == feature_layer_selected, stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
        dplyr::pull(layer_column)
      
      if(!is.null(hydro_condition_selected)){
        layer_column_hydro <- layers %>% 
          dplyr::filter(layer_name == feature_layer_selected,  stringr::str_detect(string = spatial_context, pattern = "boundary")) %>%
          dplyr::pull(layer_column_hydro)
      }else{
        layer_column_hydro <- NA
      }
      
      if(length(layer_column) != 0){
        
        # replace layer column using hydro conditions if available
        if(!is.na(layer_column_hydro)){
          layer_column <- rjson::fromJSON(layer_column_hydro)[[hydro_condition_selected]]
        }
        
        new_geom <- geom
        
        new_geom$division$layer_selected <- geom$division[[layer_column]]
        
        values <- new_geom$division$layer_selected %>% unique() %>% sort()
        
        # check if legend has []
        contain_brackets <- stringr::str_detect(values, "\\[") %>% sum()
        # if true find the colors according to the bracket value
        # otherwise keep using color with order warm to cold
        if(contain_brackets > 0){
          values_order <- stringr::str_remove(values, "\\].*") %>% stringr::str_remove("\\[*")
          palette <- palette1[values_order]
          values <- stringr::str_remove(values, ".*\\]")
          new_geom$division$layer_selected <- stringr::str_remove(new_geom$division$layer_selected, ".*\\]")
        }else{
          if(length(values) == 1){
            n <- 2
          }else{
            n <- length(values)
          }
          palette <- palette1[1:n]
        }
        
        
        pal <- colorFactor(palette,
                           levels = values,
                           na.color = "#e6e1e1",
                           alpha = F)
        palNA <- colorFactor(palette,
                             levels = values,
                             reverse = F,
                             na.color = NA)
        
        map <- map %>%
          addPolygons(
            data = new_geom$division,
            layerId = ~boundary_type_geom_id,
            group = "Division",
            color = ~pal(layer_selected),
            fillColor = ~pal(layer_selected),
            dashArray = 1,
            opacity = 1,
            fillOpacity = 0.6,
            weight = 2,
            highlightOptions = highlightOptions(
              color = "#EA4C3B", weight = 5,
              bringToFront = FALSE
            ),
            label = ~lapply(paste0("<span style = 'color:#EA4C3B'><b>",boundary_name,"</b></span>"
                                   #"<br>Dom. Land Use: ", land_use,
                                   #"<br>Dom. Irrigation: ", irrig_type
            ), htmltools::HTML),
            
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "division")
          )
        
      }
      
    }
    
  }
  
  # draw huc10 ===============================================================
  if(theme_options$has_huc10){
    
    map <- map %>%
      addPolygons(
        data = geom$huc10,
        layerId = ~boundary_type_geom_id,
        group = "HUC10",
        color = '#A71B4B',
        dashArray = 1,
        opacity = 1,
        fillOpacity = 0,
        weight = 2,
        highlightOptions = highlightOptions(
          color = "#A71B4B", weight = 5,
          bringToFront = FALSE
        ),
        label = ~lapply(paste0("<span style = 'color:#A71B4B'><b>",boundary_name,"</b></span><br>"), htmltools::HTML),
        labelOptions = labelOptions(textsize = "14px"),
        options = pathOptions(pane = "huc10")
      )
    
  }
  
  # draw huc12 ===============================================================
  if(theme_options$has_huc12){
    
    map <- map %>%
      addPolygons(
        data = geom$huc12,
        layerId = ~boundary_type_geom_id,
        group = "HUC12",
        color = '#4B0055',
        dashArray = 1,
        opacity = 1,
        fillOpacity = 0,
        weight = 2,
        highlightOptions = highlightOptions(
          color = "#4B0055", weight = 5,
          bringToFront = FALSE
        ),
        label = ~lapply(paste0("<span style = 'color:#4B0055'><b>",boundary_name,"</b></span><br>"), htmltools::HTML),
        labelOptions = labelOptions(textsize = "14px"),
        options = pathOptions(pane = "huc12")
      )
  }
  
  # draw streams ===============================================================
  if(theme_options$has_stream){
    
    if(is.null(feature_layer_stream_selected)){
      
      map <- map %>%
        addPolylines(
          data = geom$streams,
          layerId = ~id,
          group = "Streams",
          color = '#0f52ba',
          dashArray = 1,
          opacity = 1,
          fillOpacity = 0,
          weight = ~weight,
          highlightOptions = highlightOptions(
            color = "orangered", weight = 5,
            bringToFront = FALSE
          ),
          label = ~lapply(paste0("<span style = 'color:#0f52ba'><b>",gnis_name,"</b></span><br>"), htmltools::HTML),
          labelOptions = labelOptions(textsize = "14px"),
          options = pathOptions(pane = "streams")
        )
      
    }else{
      
      layer_column <- layers_stream %>% 
        dplyr::filter(layer_name == feature_layer_stream_selected,  stringr::str_detect(string = spatial_context, pattern = "stream")) %>%
        dplyr::pull(layer_column)
      
      if(length(layer_column) != 0){
        
        new_geom <- geom
        
        new_geom$streams$layer_selected <- geom$streams[[layer_column]]
        
        values <- new_geom$streams$layer_selected %>% unique() %>% sort()
        palette <- c("#000080","#008b8b","#00FF7F","#E75480","#ADD8E6")
        # palette <- c("purple","gold","darkorange","darkred","lightblue")
        n <- length(values)
        # if(length(values) == 1){
        #   n <- 2
        # }else{
        #   n <- length(values)
        # }
        palette <- palette[1:n]
        
        pal <- colorFactor(palette,
                           levels = values,
                           na.color = "#CCCCCC",
                           alpha = F)
        palNA <- colorFactor(palette,
                             levels = values,
                             reverse = F,
                             na.color = NA)
        
        new_geom$streams <- new_geom$streams %>% 
          dplyr::mutate(
            color = pal(layer_selected),
            colorNA = palNA(layer_selected)
          )
        
        purrr::walk(
          unique(new_geom$streams$color), 
          ~{
            
            data_grouped_by_color <- new_geom$streams %>% 
              dplyr::filter(color == .x)
            
            map <<- map %>%
              addPolylines(
                data = data_grouped_by_color,
                layerId = ~id,
                group = "Streams",
                color = ~color,
                dashArray = 1,
                opacity = 1,
                fillOpacity = 0,
                weight = ~weight,
                highlightOptions = highlightOptions(
                  color = .x, weight = 15,
                  bringToFront = FALSE
                ),
                label = ~lapply(paste0("<span style = 'color:", .x,"'><b>",gnis_name,"</b></span><br>"), htmltools::HTML),
                labelOptions = labelOptions(textsize = "14px"),
                options = pathOptions(pane = "streams")
              )
            
          }
        )
        
        
      }
      
      
      
    }
  }
  
  # draw shade ===============================================================
  if(theme_options$has_shade){
    map <- map %>%
      addPolygons(
        data = geom$shade,
        layerId = ~shade_id,
        group = "Shade",
        color = ~color,
        dashArray = 1,
        opacity = 1,
        fillColor = ~color,
        fillOpacity = 0.8,
        weight = 5,
        highlightOptions = highlightOptions(
          color = "black", weight = 5,
          bringToFront = FALSE
        ),
        label = ~lapply(paste0("<span style = 'color:#000'><b>Shade</b></span>",
                               "<br>APN: ", apn,
                               "<br>Length (ft): ", length_ft,
                               "<br>Kcals/day: ", aug_kcal,
                               "<br>Rank: ", shade_id
        ), htmltools::HTML),
        labelOptions = labelOptions(textsize = "14px"),
        options = pathOptions(pane = "shade")
      )
    
  }
  
  # draw habitat ===============================================================
  if(theme_options$has_habitat){
    
    purrr::walk(
      unique(geom$habitat$color), 
      ~{
        
        data_grouped_by_color <- geom$habitat %>% 
          dplyr::filter(color == .x)
        
        map <<- map %>%
          addPolygons(
            data = data_grouped_by_color,
            layerId = ~habitat_id,
            group = "Habitats",
            color = ~color,
            fillColor = ~color,
            dashArray = 1,
            opacity = 1,
            fillOpacity = 0.8,
            weight = 1.5,
            highlightOptions = highlightOptions(
              color = .x, weight = 5,
              bringToFront = FALSE
            ),
            label = ~lapply(paste0("<span style = 'color:",.x,"'><b>",habitat_subtype,"</b></span>",
                                   "<br>", biodivsig), htmltools::HTML),
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "habitat")
          )
        
      }
    )
    
  }
  
  # draw infra ===============================================================
  if(theme_options$has_infra){
    
    map <- map %>%
      addMarkers(
        data = geom$structures,
        layerId = ~id,
        icon = ~icon[[1]],
        group = "Structures",
        label = ~lapply(paste0("<span style = 'color:", color, "'><b>", structure_name,"</b></span>",
                               "<br>Type: ", structure_type),
                        htmltools::HTML),
        labelOptions = labelOptions(textsize = "14px"),
        options = pathOptions(pane = "structures")
      ) 
    
  }
  
  # draw salinity =============================================================
  
  if(theme_options$has_salinity){
    
    purrr::walk(
      unique(geom$salinity$color), 
      ~{
        
        data_grouped_by_color <- geom$salinity %>% 
          dplyr::filter(color == .x)
        
        map <<- map %>%
          addPolygons(
            data = data_grouped_by_color,
            layerId = ~id,
            group = "Salinity",
            color = ~color,
            fillColor = ~color,
            dashArray = 1,
            opacity = 1,
            fillOpacity = 0.7,
            weight = 1.5,
            highlightOptions = highlightOptions(
              color = .x, weight = 5,
              bringToFront = FALSE
            ),
            label = ~lapply(paste0("<span style = 'color:", .x,"'><b>", salinity_type ,"</b></span>",
                                   "<br>", salinity_subtype), htmltools::HTML),
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "salinity")
          )
        
      }
    )
    
  }
  
  
  # draw selenium =============================================================
  
  if(theme_options$has_selenium){
    
    purrr::walk(
      unique(geom$selenium$color), 
      ~{
        
        data_grouped_by_color <- geom$selenium %>% 
          dplyr::filter(color == .x)
        
        map <<- map %>%
          addPolygons(
            data = data_grouped_by_color,
            layerId = ~id,
            group = "Selenium",
            color = ~color,
            fillColor = ~color,
            dashArray = 1,
            opacity = 1,
            fillOpacity = 0.7,
            weight = 1.5,
            highlightOptions = highlightOptions(
              color = .x, weight = 5,
              bringToFront = FALSE
            ),
            label = ~lapply(paste0("<span style = 'color:", .x,"'><b>", selenium_type ,"</b></span>",
                                   "<br>", selenium_subtype), htmltools::HTML),
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "selenium")
          )
        
      }
    )
    
  }
  
  # draw diversions ==========================================================
  if(theme_options$has_pod){
    
    
    map <- map %>%
      addCircleMarkers(
        data = geom$diversions %>% filter(pod_extent == 1),
        layerId = ~wdid,
        radius = 8,
        stroke = TRUE,
        group = "Diversions",
        fill = TRUE,
        color = "orange",
        fillColor = ~color,
        opacity = 1,
        fillOpacity = 0.9,
        weight = 2,
        labelOptions = labelOptions(textsize = "14px"),
        label = ~lapply(paste0("<span style = 'color:", color,"'><b>",structure_name," (#", wdid,")</b></span>",
                               "<br>Irrig. Quantile: ", pod_classify,
                               "<br>Irrig. Volume (af): ", ag_use_iyr_af
        ), htmltools::HTML),
        clusterOptions = markerClusterOptions(
          #   spiderfyOnMaxZoom = FALSE,
          showCoverageOnHover = FALSE,
          #   zoomToBoundsOnClick = FALSE,
          #   animateAddingMarkers = FALSE,
          #   disableClusteringAtZoom = 7  # Adjust as needed
          # ),
          iconCreateFunction = JS(
            "function(cluster) {",
            "  var childCount = cluster.getChildCount();",
            "  return new L.DivIcon({",
            "    html: '<div style=\" font-size: 10px; height: 22px; width: 30px; padding: 5px 5px !important; margin: 0 !important;  background-color: #370439; color: white;' + '\" class=\"marker-cluster\">' + childCount + '</div>',",
            "    className: 'marker-cluster',",
            "    iconSize: new L.Point(0, 0)",
            "  });",
            "}"
          )
        ),
        options = pathOptions(pane = "diversion")
      ) %>%
      addCircleMarkers(
        data = geom$diversions %>% filter(pod_extent == 2),
        layerId = ~wdid,
        radius = 8,
        stroke = TRUE,
        group = "Diversions",
        fill = TRUE,
        color = "orange",
        fillColor = ~color,
        opacity = 1,
        fillOpacity = 0.9,
        weight = 2,
        labelOptions = labelOptions(textsize = "14px"),
        label = ~lapply(paste0("<span style = 'color:", color,"'><b>",structure_name," (#", wdid,")</b></span>",
                               "<br>Irrig. Quantile: ", pod_classify,
                               "<br>Irrig. Volume (af): ", ag_use_iyr_af
        ), htmltools::HTML),
        clusterOptions = markerClusterOptions(
          #   spiderfyOnMaxZoom = FALSE,
          showCoverageOnHover = FALSE,
          #   zoomToBoundsOnClick = FALSE,
          #   animateAddingMarkers = FALSE,
          #   disableClusteringAtZoom = 7  # Adjust as needed
          # ),
          iconCreateFunction = JS(
            "function(cluster) {",
            "  var childCount = cluster.getChildCount();",
            "  return new L.DivIcon({",
            "    html: '<div style=\" font-size: 10px; height: 22px; width: 30px; padding: 5px 5px !important; margin: 0 !important;  background-color: #870061; color: white;' 
            + '\" class=\"marker-cluster\">' + childCount + '</div>',",
            "    className: 'marker-cluster',",
            "    iconSize: new L.Point(0, 0)",
            "  });",
            "}"
          )
        ),
        options = pathOptions(pane = "diversion")
      ) %>%
      addCircleMarkers(
        data = geom$diversions %>% filter(pod_extent == 3),
        layerId = ~wdid,
        radius = 8,
        stroke = TRUE,
        group = "Diversions",
        fill = TRUE,
        color = "orange",
        fillColor = ~color,
        opacity = 1,
        fillOpacity = 0.9,
        weight = 2,
        labelOptions = labelOptions(textsize = "14px"),
        label = ~lapply(paste0("<span style = 'color:", color,"'><b>",structure_name," (#", wdid,")</b></span>",
                               "<br>Irrig. Quantile: ", pod_classify,
                               "<br>Irrig. Volume (af): ", ag_use_iyr_af
        ), htmltools::HTML),
        clusterOptions = markerClusterOptions(
          #   spiderfyOnMaxZoom = FALSE,
          showCoverageOnHover = FALSE,
          #   zoomToBoundsOnClick = FALSE,
          #   animateAddingMarkers = FALSE,
          #   disableClusteringAtZoom = 7  # Adjust as needed
          # ),
          iconCreateFunction = JS(
            "function(cluster) {",
            "  var childCount = cluster.getChildCount();",
            "  return new L.DivIcon({",
            "    html: '<div style=\" font-size: 10px; height: 22px; width: 30px; padding: 5px 5px !important; margin: 0 !important;  background-color: #E85D48; color: white;' 
            + '\" class=\"marker-cluster\">' + childCount + '</div>',",
            "    className: 'marker-cluster',",
            "    iconSize: new L.Point(0, 0)",
            "  });",
            "}"
          )
        ),
        options = pathOptions(pane = "diversion")
      ) %>%
      addCircleMarkers(
        data = geom$diversions %>% filter(pod_extent == 4),
        layerId = ~wdid,
        radius = 8,
        stroke = TRUE,
        group = "Diversions",
        fill = TRUE,
        color = "orange",
        fillColor = ~color,
        opacity = 1,
        fillOpacity = 0.9,
        weight = 2,
        labelOptions = labelOptions(textsize = "14px"),
        label = ~lapply(paste0("<span style = 'color:", color,"'><b>",structure_name," (#", wdid,")</b></span>",
                               "<br>Irrig. Quantile: ", pod_classify,
                               "<br>Irrig. Volume (af): ", ag_use_iyr_af
        ), htmltools::HTML),
        clusterOptions = markerClusterOptions(
          #   spiderfyOnMaxZoom = FALSE,
          showCoverageOnHover = FALSE,
          #   zoomToBoundsOnClick = FALSE,
          #   animateAddingMarkers = FALSE,
          #   disableClusteringAtZoom = 7  # Adjust as needed
          # ),
          iconCreateFunction = JS(
            "function(cluster) {",
            "  var childCount = cluster.getChildCount();",
            "  return new L.DivIcon({",
            "    html: '<div style=\" font-size: 10px; height: 22px; width: 30px; padding: 5px 5px !important; margin: 0 !important; background-color: #F2BE91; color: black;'
            + '\" class=\"marker-cluster\">' + childCount + '</div>',",
            "    className: 'marker-cluster',",
            "    iconSize: new L.Point(0, 0)",
            "  });",
            "}"
          )
        ),
        options = pathOptions(pane = "diversion")
      )
    
  }
  
  
  map
  
  
  
}