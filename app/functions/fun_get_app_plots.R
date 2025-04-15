fun_get_app_plots <- function(theme_name){
  
  if(is.null(theme_name)){
    theme_id <- 'NULL'
  }else{
    theme_id <- themes %>%
      dplyr::filter(theme == theme_name) %>%
      dplyr::pull(theme_id) %>% as.character()
  }
  
  # query
  query <- "select * from project_wsba.get_app_plots(_theme_id)" %>%
    stringr::str_replace("_theme_id", paste0("_theme_id=>", theme_id)) %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query)
  
  # output
  data
  
}