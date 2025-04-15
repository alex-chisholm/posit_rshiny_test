fun_get_geometries_diversion_aoi_map <- function(id, name){
  
  # object to store geometries
  geom <- list(
    aoi = NULL,
    fields = NULL,
    diversions = NULL,
    county = NULL,
    econreg = NULL,
    district = NULL,
    division = NULL,
    huc10 = NULL,
    huc12 = NULL,
    streams = NULL,
    shade = NULL,
    habitat = NULL,
    structures = NULL,
    salinity = NULL,
    selenium = NULL
  )
  
  
  cli::cli_h1("Downloading geometries for the Exploratory Map/Search")
  cli::cli_h2(paste0("Diversion AOI: ", name))
  cli::cli_h2(paste0("wdid: ", id))
  
  
  # FIELD GEOMETRY ===========================================================
  cli::cli_alert("Field geometry")
  
  query <- paste0("select * from project_wsba.get_field_geometry(_wdid=>", id,")")
  data_field <-  dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::mutate(
      field_id = as.integer(field_id),
      geom = sf::st_as_sfc(geom), 
      color = case_when(
        pod_extent == 1 ~ "#370439" ,
        pod_extent == 2 ~ "#870061",
        pod_extent == 3 ~ "#E85D48",
        pod_extent == 4 ~ "#F2BE91",
        TRUE ~ "#FDE333"
      )
    )
  
  data_field_final <- sf::st_cast(sf::st_as_sf(data_field), "MULTIPOLYGON", warn = FALSE)
  geom$fields <- data_field_final
  
  
  # DIVERSION GEOMETRY =======================================================
  cli::cli_alert("Diversion geometry")
  
  query <- paste0("select * from project_wsba.get_structure_geometry(_wdid=>", id,")")
  data_diversion <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::mutate(
      geom = sf::st_as_sfc(geom),
      color = case_when(
        pod_extent == 1 ~ "#370439" ,
        pod_extent == 2 ~ "#870061",
        pod_extent == 3 ~ "#E85D48",
        pod_extent == 4 ~ "#F2BE91",
        TRUE ~ "#FDE333"
      )
    )
  
  data_diversion_final <- sf::st_cast(sf::st_as_sf(data_diversion), "POINT", warn = FALSE)
  geom$diversions <- data_diversion_final
  
  # STREAM GEOMETRY =======================================================
  cli::cli_alert("Stream geometry")
  
  query <- paste0("select * from project_wsba.get_stream_geometry(_wdid=>", id,")")
  data_stream <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::mutate(
      id = as.integer(id),
      geom = sf::st_as_sfc(geom),
      weight = 5 - qt
    ) %>%
    dplyr::ungroup()
  
  data_stream_final <- sf::st_cast(sf::st_as_sf(data_stream), "MULTILINESTRING", warn = FALSE)
  geom$streams <- data_stream_final
  
  
  # HABITAT GEOMETRY =======================================================
  cli::cli_alert("Habitat geometry")
  query <- paste0("select * from project_wsba.get_habitat_geometry(_wdid=>", id,")")
  
  data_habitat <- dbGetQuery(con, query) %>%
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
  
  data_habitat_final <- sf::st_cast(sf::st_as_sf(data_habitat), "MULTIPOLYGON", warn = FALSE)
  geom$habitat <- data_habitat_final
  
  # BOUNDARY GEOMETRY =======================================================
  cli::cli_alert("HUC10 geometry")
  query <- paste0("select * from project_wsba.get_boundary_geometry(_wdid=>", id,")")
  
  data_huc10 <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::mutate(
      boundary_type_geom_id = as.integer(boundary_type_geom_id),
      geom = sf::st_as_sfc(geom)
    )
  
  data_huc10_final <- sf::st_cast(sf::st_as_sf(data_huc10), "MULTIPOLYGON", warn = FALSE)
  geom$huc10 <- data_huc10_final
  
  
  # STRUCTURES GEOMETRY =====================================================
  cli::cli_alert("Structures geometry")
  query <- paste0("select * from project_wsba.get_infra_geometry(_wdid=>", id,")")
  
  data_structure <- dbGetQuery(con, query) %>%
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
  
  data_structure_final <- sf::st_cast(sf::st_as_sf(data_structure), "POINT", warn = FALSE)
  geom$structures <- data_structure_final
  
  # SALINITY GEOMETRY =====================================================
  cli::cli_alert("Salinity geometry")
  query <- paste0("select * from project_wsba.get_salinity_geometry(_wdid=>", id,")")

  data_salinity <- dbGetQuery(con, query) %>%
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

  data_salinity_final <- sf::st_cast(sf::st_as_sf(data_salinity), "MULTIPOLYGON", warn = FALSE)
  geom$salinity <- data_salinity_final
  
  
  # SELENIUM GEOMETRY =====================================================
  cli::cli_alert("Selenium geometry")
  query <- paste0("select * from project_wsba.get_selenium_geometry(_wdid=>", id,")")
  
  data_selenium <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::mutate(
      id = as.integer(id),
      geom = sf::st_as_sfc(geom),
      color = case_when(
        selenium_type == "Selenium Control" ~ "#67055A",
        selenium_type == "Selenium Load" ~ "#B05947"
      )
    )
  
  data_selenium_final <- sf::st_cast(sf::st_as_sf(data_selenium), "MULTIPOLYGON", warn = FALSE)
  geom$selenium <- data_selenium_final
  
  
  # check if any of the geoms have 0 rows ====================================
  
  purrr::walk(names(geom),
  ~{
    if(!is.null(geom[[.x]])){
      if(nrow(geom[[.x]]) == 0){
        geom[.x] <<- list(NULL)
      }
    }
  })
  
    # OUTPUT ===================================================================
  cli::cli_alert_success("Geometries updated!")
  geom
  
}