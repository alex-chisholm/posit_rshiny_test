fun_get_geometries_theme <- function(layers, geom){
  
  cli::cli_h1("Downloading geometries for the Exploratory Map")
  
  if(layers$has_aoi & is.null(geom$aoi)){
    
    cli::cli_alert("West Slope geometry")
    query <- "select * from project_wsba.get_boundary_geometry('aoi')"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        id = 1:n(),
        geom = sf::st_as_sfc(geom)
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$aoi <- data_final
    
  }
  
  if(layers$has_county  & is.null(geom$county)){
    
    cli::cli_alert("County geometry")
    query <- "select * from project_wsba.get_boundary_geometry('county')"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        boundary_type_geom_id = as.integer(boundary_type_geom_id),
        geom = sf::st_as_sfc(geom)
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$county <- data_final
    
  }
  
  
  if(layers$has_econreg  & is.null(geom$econreg)){
    
    cli::cli_alert("Econ. Region geometry")
    query <- "select * from project_wsba.get_boundary_geometry('econreg')"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        boundary_type_geom_id = as.integer(boundary_type_geom_id),
        geom = sf::st_as_sfc(geom)
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$econreg <- data_final
    
  }
  
  if(layers$has_district  & is.null(geom$district)){
    
    cli::cli_alert("District geometry")
    query <- "select * from project_wsba.get_boundary_geometry('district')"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        boundary_type_geom_id = as.integer(boundary_type_geom_id),
        geom = sf::st_as_sfc(geom)
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$district <- data_final
    
  }
  
  if(layers$has_division  & is.null(geom$division)){
    
    cli::cli_alert("Division geometry")
    query <- "select * from project_wsba.get_boundary_geometry('division')"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        boundary_type_geom_id = as.integer(boundary_type_geom_id),
        geom = sf::st_as_sfc(geom)
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$division <- data_final
    
  }
  
  if(layers$has_huc10  & is.null(geom$huc10)){
    
    cli::cli_alert("HUC10 geometry")
    query <- "select * from project_wsba.get_boundary_geometry('huc10')"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        boundary_type_geom_id = as.integer(boundary_type_geom_id),
        geom = sf::st_as_sfc(geom)
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$huc10 <- data_final
    
  }
  
  if(layers$has_huc10  & is.null(geom$huc12)){
    
    cli::cli_alert("HUC12 geometry")
    query <- "select * from project_wsba.get_boundary_geometry('huc12')"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        boundary_type_geom_id = as.integer(boundary_type_geom_id),
        geom = sf::st_as_sfc(geom)
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$huc12 <- data_final
    
  }
  
  if(layers$has_pod  & is.null(geom$diversions)){
    
    cli::cli_alert("Diversion geometry")
    geom$diversions <- fun_get_structure_geometry() %>% distinct()
    
  }
  
  if(layers$has_field  & is.null(geom$fields)){
    
    cli::cli_alert("Field geometry")
    geom$fields <-  fun_get_field_geometry()
    
  }
  
  if(layers$has_stream  & is.null(geom$stream)){
    
    cli::cli_alert("Stream geometry")
    query <- "select * from project_wsba.get_stream_geometry()"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        id = as.integer(id),
        geom = sf::st_as_sfc(geom),
        weight = 5 - qt
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTILINESTRING", warn = FALSE)
    geom$streams <- data_final
    
  }
  
  if(layers$has_shade  & is.null(geom$shade)){
    
    cli::cli_alert("Shade geometry")
    query <- "select * from project_wsba.get_shade_geometry()"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        shade_id = as.integer(shade_id),
        geom = sf::st_as_sfc(geom),
        color = case_when(
          qt_descr == "Low" ~ "#E0FFFF", #"gold", #"forestgreen",
          qt_descr == " Med." ~ "lightskyblue", #"orange", #"gold",
          qt_descr == " High" ~ "steelblue", #"darkred",
        )
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$shade <- data_final
    
  }
  
  if(layers$has_habitat  & is.null(geom$habitat)){
    
    cli::cli_alert("Habitat geometry")
    query <- "select * from project_wsba.get_habitat_geometry()"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        habitat_id = as.integer(habitat_id),
        geom = sf::st_as_sfc(geom),
        color = case_when(
          habitat_type == "Sage Grouse Habitat" ~ "#4d8784",
          habitat_type == "Biodiverse Wetlands" ~ "#868955",
          habitat_type == "Protected Lands" ~ "#654000",
          habitat_type == "Flood-Irrigated Wetlands" ~ "#261a12"     
        )
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$habitat <- data_final
    
  }
  
  if(layers$has_infra  & is.null(geom$structures)){
    
    cli::cli_alert("Structures geometry")
    query <- "select * from project_wsba.get_infra_geometry()"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        id = as.integer(id),
        geom = sf::st_as_sfc(geom),
        color = case_when(
          structure_type == "Reservoir" ~ '#4e90c2',
          structure_type == "Power Plant" ~ '#F4B183',
          structure_type == "Treatment" ~ '#50C878'
        ),
        icon_name = case_when(
          structure_type == "Reservoir" ~ 'blue_triangle',
          structure_type == "Power Plant" ~ 'red_triangle',
          structure_type == "Treatment" ~ 'green_triangle',
        ),
        icon = list(makeIcon(
          iconUrl = paste0("www/img/", icon_name,".png"),
          iconWidth = (5 - structure_size)*3 +10, 
          iconHeight = (5 - structure_size)*3 +10,
          iconAnchorX = 0, iconAnchorY = 0
        )),
        icon_highlight = list(makeIcon(
          iconUrl = "www/img/highlight_triangle.png",
          iconWidth = (5 - structure_size)*3 +10, 
          iconHeight = (5 - structure_size)*3 +10,
          iconAnchorX = 0, iconAnchorY = 0
        ))
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "POINT", warn = FALSE)
    geom$structures <- data_final
  }
  
  if(layers$has_salinity  & is.null(geom$salinity)){
    
    cli::cli_alert("Salinity geometry")
    query <- "select * from project_wsba.get_salinity_geometry()"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        id = as.integer(id),
        geom = sf::st_as_sfc(geom),
        color = case_when(
          salinity_type == "Salinity Control" ~ "#94BF5A",
          #salinity_type == "Selenium Load" ~ "#B05947",
          salinity_type == "Salinity Load" ~ "#C385D6"     
        )
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$salinity <- data_final
    
  }

  if(layers$has_selenium  & is.null(geom$selenium)){
    
    cli::cli_alert("Selenium geometry")
    query <- "select * from project_wsba.get_selenium_geometry()"
    
    data <- dbGetQuery(con, query) %>%
      tibble::tibble() %>%
      dplyr::mutate(
        id = as.integer(id),
        geom = sf::st_as_sfc(geom),
        color = case_when(
          selenium_type == "Selenium Control" ~ "#67055A",
          selenium_type == "Selenium Load" ~ "#B05947"
        )
      )
    
    data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
    geom$selenium <- data_final
    
  }
  
  cli::cli_alert_success("Geometries updated!")
  geom
  
}