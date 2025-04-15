fun_get_app_layers <- function(spatial_context = NULL){
  
  # query
  # query <- "select * from project_wsba.get_app_layers()"

  if(is.null(spatial_context)){
    query <- "select * from project_wsba.get_app_layers()"
  }
  else{  
    query <- "select * from project_wsba.get_app_layers(_context)" %>%
      stringr::str_replace("_context", paste0("_context=>'", spatial_context,"'")) %>%
      glue::glue_collapse(sep = "\n")
  }
  
  # collect data
  layers <- dbGetQuery(con, query)
  
  # output
  layers
  
}
