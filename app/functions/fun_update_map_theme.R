fun_update_map_theme <- function(map, theme_selected){
  
  theme_options <- themes %>%
    dplyr::filter(theme == theme_selected)
   
  # all_layers <-  c("West Slope","Fields","County","District","Division","HUC10","HUC12","Diversions")
  # 
  # layers_options <- tibble::tibble(
  #   key = theme_options %>% colnames(),
  #   val = theme_options %>% t() %>% c() %>% as.logical()
  # ) %>%
  #   dplyr::filter(stringr::str_detect(key, "has"), val == TRUE) %>%
  #   dplyr::mutate(
  #     key_fixed = stringr::str_remove(key, "has_") %>%
  #       stringr::str_replace("pod","diversions") %>%
  #       stringr::str_replace("field","fields")
  #   )
  # 
  # layers_on <- all_layers[tolower(all_layers) %in% layers_options$key_fixed]
  # 
  new_map <- map %>%
    {
      if(theme_options$has_aoi){
        showGroup(., "West Slope")
      }else{
        hideGroup(., "West Slope")
      }
    } %>%
    {
      if(theme_options$has_field){
        showGroup(., "Fields")
      }else{
        hideGroup(., "Fields")
      }
    } %>%
    {
      if(theme_options$has_county){
        showGroup(., "County")
      }else{
        hideGroup(., "County")
      }
    } %>%
    {
      if(theme_options$has_econreg){
        showGroup(., "Econ. Region")
      }else{
        hideGroup(., "Econ. Region")
      }
    } %>%    
    {
      if(theme_options$has_division){
        showGroup(., "Division")
      }else{
        hideGroup(., "Division")
      }
    } %>%
    {
      if(theme_options$has_district){
        showGroup(., "District")
      }else{
        hideGroup(., "District")
      }
    } %>%
    {
      if(theme_options$has_huc10){
        showGroup(., "HUC10")
      }else{
        hideGroup(., "HUC10")
      }
    } %>%
    {
      if(theme_options$has_huc12){
        showGroup(., "HUC12")
      }else{
        hideGroup(., "HUC12")
      }
    } %>%
    {
      if(theme_options$has_pod){
        showGroup(., "Diversions")
      }else{
        hideGroup(., "Diversions")
      }
    } %>%
    {
      if(theme_options$has_shade){
        showGroup(., "Shade")
      }else{
        hideGroup(., "Shade")
      }
    } #%>%
    # removeLayersControl() %>%
    # addLayersControl(
    #   overlayGroups = layers_on,
    #   position = 'topleft',
    #   options = layersControlOptions(
    #     collapsed = FALSE,
    #     autoZIndex = FALSE
    #   )
    # )
  
  # output
  new_map
  
  
}