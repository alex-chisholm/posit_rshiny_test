fun_get_boundary_attributes <- function(id, type){
  
  # query get_structure_attributes ===========================================
  query <- "select * from project_wsba.get_boundary_attributes(type, id)" %>%
    stringr::str_replace("id", as.character(id)) %>%
    stringr::str_replace("type", paste0("'",type,"'")) %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble()
  
  # title
  name <- data %>% 
    dplyr::filter(key == "Name") %>%
    dplyr::pull(val)
  
  title <- paste0(
    "<h4><span class = 'badge mb-3 bg-secondary text-white' style = ''><b>", 
    name, "</b></span></h4>"
  )
  
  data <- data %>% dplyr::filter(key != "Name")
  n <- nrow(data)
  n1 <- ceiling(n/2)
  
  table1 <- data %>%
    dplyr::slice(1:n1) 
  
  table2 <- data %>%
    dplyr::slice((n1+1):n)
  
  
  # output
  list(
    title = title,
    table1 = table1,
    table2 = table2
  )
  
}