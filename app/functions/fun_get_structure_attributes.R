fun_get_structure_attributes <- function(wdid){
  
  # query get_structure_attributes ===========================================
  query <- "select * from project_wsba.get_structure_attributes(_wdid)" %>%
    stringr::str_replace("_wdid", paste0("_wdid=>", wdid)) %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble()
  
  # title
  structure_name <- data %>% 
    dplyr::filter(key == "Name") %>%
    dplyr::pull(val)
  
  structure_title <- paste0(
    "<h4><span class = 'badge mb-3 bg-secondary text-white' style = ''><b>", 
    structure_name, " (#", wdid,")</span></h4>"
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
    title = structure_title,
    table1 = table1,
    table2 = table2
  )
  
}