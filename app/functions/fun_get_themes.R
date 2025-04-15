fun_get_themes <- function(){
  
  # query
  query <- readr::read_file("queries/get_app_theme.sql") %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble()

  # output
  data
}