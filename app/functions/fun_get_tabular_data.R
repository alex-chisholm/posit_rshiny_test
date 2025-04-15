fun_get_tabular_data <- function(hydro_in){
  
  tryCatch({
    
  #browser()
  query <- readr::read_file("queries/get_tabular_data.sql") %>%
    stringr::str_replace("_hydro_in", paste0("_hydro=>", "'",hydro_in,"'")) %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble()
  # output
  data
  },
  error = function(e){
    paste0(
      "<div class = 'text-center p-2 border-danger rounded' style = 'background-color: #F8D7DA; color: #842029; font-size: 14px'>",  
      "Oops! Something went wrong. Wait a few minutes and try again or email us at basinscoutsupport@thefreshwatertrust.org.",
      "</div>"
    )
  })
  
}