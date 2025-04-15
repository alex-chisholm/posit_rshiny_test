fun_update_diversion_aoi_map <- function(map, geom, feature_layer_selected = NULL, feature_layer_stream_selected = NULL,
                                              hydro_condition_selected = NULL){
  
  map <- map %>%
    clearGroup("Fields") %>%
    clearGroup("West Slope") %>%
    clearGroup("Streams") %>%
    clearGroup("Habitats") %>%
    clearGroup("HUC10") %>%
    clearGroup("Diversions") %>%
    clearGroup("Structures") %>%
    clearGroup("Salinity") %>%
    clearGroup("Selenium") %>%
    # clearShapes() %>%
    clearMarkerClusters() 
  
  # draw aoi ==================================================================
  if(!is.null(geom$aoi)){
    
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
  if(!is.null(geom$fields)){
    
    layer_column_options <- layers %>% 
      dplyr::filter(
        layer_name == feature_layer_selected, 
        stringr::str_detect(string = spatial_context, pattern = "field")
      )
    
    if(!is.na(layer_column_options$layer_column_hydro)){
      layer_column <- rjson::fromJSON(layer_column_options$layer_column_hydro)[[hydro_condition_selected]]
    }else{
      layer_column <- layer_column_options$layer_column
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
      )
    
  }
  
  
  # draw streams ===============================================================
  if(!is.null(geom$streams)){
    
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
  
  # draw habitat ===============================================================
  if(!is.null(geom$habitat)){

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
            label = ~lapply(paste0("<span style = 'color:", .x,"'><b>",habitat_subtype,"</b></span>",
                                   "<br>", biodivsig), htmltools::HTML),
            labelOptions = labelOptions(textsize = "14px"),
            options = pathOptions(pane = "habitat")
          )
        
      }
    )

  }
  
  # draw huc10 ===============================================================
  if(!is.null(geom$huc10)){
    
    map <- map %>%
      addPolygons(
        data = geom$huc10,
        layerId = ~boundary_type_geom_id,
        group = "HUC10",
        color = '#000000',
        dashArray = 1,
        opacity = 1,
        fillOpacity = 0,
        weight = 2,
        highlightOptions = highlightOptions(
          color = "#000000", weight = 5,
          bringToFront = FALSE
        ),
        label = ~lapply(paste0("<span style = 'color:#000000'><b>",boundary_name,"</b></span><br>"), htmltools::HTML),
        labelOptions = labelOptions(textsize = "14px"),
        options = pathOptions(pane = "huc10")
      )
    
  }
  
  # draw infra ===============================================================
  if(!is.null(geom$structures)){
    
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
  
  # draw salinity ============================================================
  if(!is.null(geom$salinity)){

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
  
  # draw selenium ============================================================
  if(!is.null(geom$selenium)){
    
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
  if(!is.null(geom$diversions)){
    
    
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