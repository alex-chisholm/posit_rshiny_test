fun_layers_options_update <- function(layers_options, layers_list){
  
  # only update the  no checkbox items
  layers_options %>%
    dplyr::mutate(
      # has_aoi = ifelse("AOI" %in% layers_list, TRUE, FALSE),
      # has_division = ifelse("Division" %in% layers_list, TRUE, FALSE),
      # has_district = ifelse("District" %in% layers_list, TRUE, FALSE),
      # has_county = ifelse("County" %in% layers_list, TRUE, FALSE),
      # has_huc10 = ifelse("HUC10" %in% layers_list, TRUE, FALSE),
      # has_huc12 = ifelse("HUC12" %in% layers_list, TRUE, FALSE),
      has_pod = ifelse("Diversions" %in% layers_list, TRUE, FALSE),
      has_field = ifelse("Fields" %in% layers_list, TRUE, FALSE),
      # has_econreg = ifelse("Econ. Region" %in% layers_list, TRUE, FALSE),
      has_stream = ifelse("Streams" %in% layers_list, TRUE, FALSE),
      has_shade = ifelse("Shade" %in% layers_list, TRUE, FALSE),
      has_habitat = ifelse("Habitats" %in% layers_list, TRUE, FALSE),
      has_infra = ifelse("Structures" %in% layers_list, TRUE, FALSE),
      has_salinity = ifelse("Salinity" %in% layers_list, TRUE, FALSE),
      has_selenium = ifelse("Selenium" %in% layers_list, TRUE, FALSE)      
    )
  
}