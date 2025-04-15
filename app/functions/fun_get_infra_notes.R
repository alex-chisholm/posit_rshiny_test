fun_get_infra_notes <- function(id){
  
  # query
  query <- "select * from project_wsba.get_infra_notes(id)" %>%
    stringr::str_replace("id", as.character(id)) %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::select(note)
  
  # output
  data
}