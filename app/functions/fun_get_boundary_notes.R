fun_get_boundary_notes <- function(id, type){
  
  # query
  query <- "select * from project_wsba.get_boundary_notes(type, id)" %>%
    stringr::str_replace("id", as.character(id)) %>%
    stringr::str_replace("type", paste0("'",type,"'")) %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::select(note)
  
  # output
  data
}