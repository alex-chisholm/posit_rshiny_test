fun_get_app_settings <- function(){
  
  # query
  query <- "select * from project_wsba.get_app_settings()"
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble()
  
  data_ok <- data %>%
    dplyr::mutate(
      themes = as.character(themes),
      details = purrr::map(as.character(details), ~jsonlite::fromJSON(.x))
    )
  
  # output
  data_ok
  
}
