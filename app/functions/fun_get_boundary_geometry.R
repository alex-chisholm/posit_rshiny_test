fun_get_boundary_geometry <- function(){
  # query
  query <- readr::read_file("queries/get_boundary_geometry.sql") %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::group_by(boundary_type) %>%
    dplyr::mutate(
      id = 1:n(),
      geom = sf::st_as_sfc(geom)
    ) %>%
    dplyr::ungroup()
  
  data_final <- sf::st_cast(sf::st_as_sf(data), "MULTIPOLYGON", warn = FALSE)
  
  # output
  data_final
}