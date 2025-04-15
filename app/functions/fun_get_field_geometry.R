fun_get_field_geometry <- function(){
  
  # query
  query <- readr::read_file("queries/get_field_geometry.sql") %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
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

  data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
  
  # output
  data_final
}