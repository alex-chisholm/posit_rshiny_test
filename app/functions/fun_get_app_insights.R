fun_get_app_insights <- function(theme_name){
  
  tryCatch({
    
  theme_id <- themes %>%
    dplyr::filter(theme == theme_name) %>%
    dplyr::pull(theme_id) %>% as.character()
  
  
  query <- "select * from project_wsba.get_app_insights(_theme_id)" %>%
    stringr::str_replace("_theme_id", paste0("_theme_id=>", theme_id)) %>%
    glue::glue_collapse(sep = "\n")
  
  # collect data
  data <- dbGetQuery(con, query) %>%
    tibble::tibble() %>%
    dplyr::select(key, val)
  

    n <- nrow(data)
    n1 <- ceiling(n/2)

    insights_title <- paste0("<h4><span class = 'badge mb-3 bg-dark text-white'><b>", theme_name, "</b></span></h4>")


    insights_table1 <- data %>%
      dplyr::slice(1:n1) 

    insights_table2 <- data %>%
      dplyr::slice((n1+1):n)


    # output
    list(
      title = insights_title,
      table1 = insights_table1,
      table2 = insights_table2
    )
    
  },
  error = function(e){
    paste0(
      "<div class = 'text-center p-2 border-danger rounded' style = 'background-color: #F8D7DA; color: #842029; font-size: 14px'>",  
      "Oops! Something went wrong. Wait a few minutes and try again or email us at basinscoutsupport@thefreshwatertrust.org.",
      "</div>"
    )
  })
  
}