fun_compare_default_click_values <- function(default_values, last_values){

  total <- (FALSE %in% (default_values$theme %in% last_values$theme)) +
    (FALSE %in% (default_values$spatial_layers %in% last_values$spatial_layers)) +
    #(FALSE %in% (default_values$hydro %in% last_values$hydro)) +
    #(FALSE %in% (default_values$layer == last_values$layer)) +
    #(FALSE %in% (default_values$stream_layer == last_values$stream_layer)) +
    (FALSE %in% (default_values$irrigation %in% last_values$irrigation))
  
  # output
  total
  
}