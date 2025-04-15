fun_get_available_geometries_theme <- function(theme_layer){
  
  layers_list <- theme_layer %>% 
    dplyr::select(theme, starts_with("has_")) %>%
    tidyr::pivot_longer(cols = -theme, names_to = "layers", values_to = "val") %>%
    dplyr::filter(val == TRUE) %>%
    dplyr::mutate(
      layer_ok = layers %>%
        stringr::str_remove("has_") %>%
        stringr::str_replace("pod","diversions") %>%
        stringr::str_replace("reg",". region") %>%
        stringr::str_replace("field","fields") %>%
        stringr::str_replace("stream","streams") %>%
        stringr::str_replace("habitat","habitats") %>%
        stringr::str_to_title() %>%
        stringr::str_replace("Huc","HUC") %>%
        stringr::str_replace("Aoi","west slope") 
    ) %>%
    dplyr::pull(layer_ok)
  
  # keep only checkbox items
  layers_list <- layers_list[which(layers_list %in% checkbox_items)]
  
  # output
  layers_list

}