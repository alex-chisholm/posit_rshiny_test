fun_get_default_inputs <- function(default_theme, app_settings, layers, layers_stream){
  
  # spatial layers to load =====================================================
  default_spatial_layers_input <- app_settings %>%
    dplyr::filter(
      setting_name == "Spatial Layers",
      stringr::str_detect(pattern = as.character(default_theme$theme_id), string = themes)
    )
  if(nrow(default_spatial_layers_input) == 0) {
    default_spatial_layers <- NULL
  }else{
    choices <- default_spatial_layers_input$details[[1]] %>% names()
    checked <- sapply(choices, function(x) default_spatial_layers_input$details[[1]][[x]]$checked)
    default_spatial_layers <- choices[checked]
  }
  
  # hydro conditions initial ===================================================
  hydro_info <- app_settings %>%
    dplyr::filter(
      setting_name == "Hydrologic Conditions",
      stringr::str_detect(pattern = as.character(default_theme$theme_id), string = themes)
    ) 
  
  if(nrow(hydro_info) == 0){
    hydro_default <- NULL
  }else{
    choices <- hydro_info$details[[1]] %>% names()
    values <- sapply(choices, function(x) hydro_info$details[[1]][[x]]$val) %>% as.numeric()
    checked <- sapply(choices, function(x) hydro_info$details[[1]][[x]]$checked)
    hydro_default <- values[checked]
  }
  
  # app feature layers (exploratory map) =======================================
  layer_default <- layers[layers$app_default,] %>%
    dplyr::filter(stringr::str_detect(pattern = as.character(default_theme$theme_id), string = themes)) %>%
    dplyr::pull(layer_name)
  if(length(layer_default) == 0) {
    layer_default <- NULL
  }
  
  stream_layer_default <- layers_stream[layers_stream$app_default,] %>%
    dplyr::filter(stringr::str_detect(pattern = as.character(default_theme$theme_id), string = themes)) %>%
    dplyr::pull(layer_name)
  if(length(stream_layer_default) == 0) {
    stream_layer_default <- NULL
  }
  
  # app feature layers (diversion aoi) =========================================================
  divaoi_layer_default <- layers[layers$app_default,] %>% 
    dplyr::slice(1) %>% 
    dplyr::pull(layer_name)
  divaoi_stream_layer_default <- layers_stream[layers_stream$app_default,] %>% 
    dplyr::slice(1) %>%
    dplyr::pull(layer_name)
  
  # irrigation =================================================================
  
 irrigation_input <- app_settings %>%
    dplyr::filter(
      setting_name == "Diversion Vol.",
      stringr::str_detect(pattern = as.character(default_theme$theme_id), string = themes)
    )
  
  if(nrow(irrigation_input) == 0) {
    default_irrigation <- NULL
  }else{
    choices <- irrigation_input$details[[1]] %>% names()
    values <- sapply(choices, function(x) irrigation_input$details[[1]][[x]]$val) %>% as.numeric()
    checked <- sapply(choices, function(x) irrigation_input$details[[1]][[x]]$checked)
    default_irrigation <- values[checked]
  }
  
  # output
  list(
    spatial_layers = default_spatial_layers,
    hydro = hydro_default,
    layer = layer_default,
    stream_layer = stream_layer_default,
    divaoi_layer = divaoi_layer_default,
    divaoi_stream_layer = divaoi_stream_layer_default,
    irrigation = default_irrigation
  )
  
}