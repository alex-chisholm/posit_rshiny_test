fun_get_app_notes <- function(theme_name, ns){
  
  f_name <- ns("setinput") %>% stringr::str_replace("-","_")
  tryCatch({
    
    theme_id <- themes %>%
      dplyr::filter(theme == theme_name) %>%
      dplyr::pull(theme_id) %>% as.character()
    
    # query
    query <- "select * from project_wsba.get_app_notes(_theme_id)" %>%
      stringr::str_replace("_theme_id", paste0("_theme_id=>", theme_id)) %>%
      glue::glue_collapse(sep = "\n")
    
    # collect data
    data <- dbGetQuery(con, query)
    
    app_note <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # note = paste0("<b>", note) %>% stringr::str_replace(":",":</b>"),
        links = note %>%
          stringr::str_extract_all("\\[(.*?)\\]") %>%
          unlist() %>%
          stringr::str_remove_all("\\[|\\]") %>% 
          list(),
        links_replacement = list(as.character(NA))
      ) %>%
      dplyr::ungroup()
    
    app_note$links_replacement <- purrr::map(
      1:nrow(app_note),
      ~{
        
        tryCatch({
          
          lapply(
            app_note$links[.x] %>% unlist() %>% stringr::str_split(":"),
            function(x){
              if(x[3] == "icon"){
                
                if(stringr::str_detect(x[1],"_structure")){
                  paste0(
                    "<a id = '", ns('find_structure'), "' class ='action-button' onclick = '",f_name,"(\"find_structure\",\"",
                    x[2],"\",\"", x[3],"\");' href = '", x[2],"'>", icon("location-dot"),"</a>"
                  )
                }else if(stringr::str_detect(x[1],"_field")){
                  paste0(
                    "<a id = '", ns('find_field'), "' class ='action-button' onclick = '",f_name,"(\"find_field\",\"",
                    x[2],"\",\"", x[3],"\");' href = '", x[2],"'>", icon("location-dot"),"</a>"
                  ) 
                }else if(stringr::str_detect(x[1],"_stream")){
                  paste0(
                    "<a id = '", ns('find_stream'), "' class ='action-button' onclick = '",f_name,"(\"find_stream\",\"",
                    x[2],"\",\"", x[3],"\");' href = '", x[2],"'>", icon("location-dot"),"</a>"
                  ) 
                }else{
                  ""
                }
                
              }else{
                
                if(stringr::str_detect(x[1],"_structure")){
                  paste0(
                    "<a id = '", ns('click_structure'), "' class ='action-button' onclick = '",f_name,"(\"click_structure\",\"",
                    x[2],"\",\"", x[3],"\");' href = '", x[2],"'>", x[3],"</a>"
                  )
                }else if(stringr::str_detect(x[1],"_field")){
                  paste0(
                    "<a id = '", ns('click_field'), "' class ='action-button' onclick = '",f_name,"(\"click_field\",\"",
                    x[2],"\",\"", x[3],"\");' href = '", x[2],"'>", x[3],"</a>"
                  ) 
                }else if(stringr::str_detect(x[1],"_stream")){
                  paste0(
                    "<a id = '", ns('click_stream'), "' class ='action-button' onclick = '",f_name,"(\"click_stream\",\"",
                    x[2],"\",\"", x[3],"\");' href = '", x[2],"'>", x[3],"</a>"
                  ) 
                }else{
                  ""
                }
                
              }
            }
          )
          
        }, error = function(e) list()
        )
        
      })
    
    # replace note link [] with clickable link
    for(i in 1:nrow(app_note)){
      purrr::walk(
        seq_along(app_note$links[i] %>% unlist()),
        ~{
          app_note$note[i] <<- stringr::str_replace(
            string = app_note$note[i], 
            pattern = app_note$links[i] %>% unlist() %>% .[.x],
            replacement = app_note$links_replacement[i] %>% unlist() %>% .[.x]
          )
        }
      )
    }
    
    app_note <- app_note %>%
      dplyr::mutate(
        note = note %>% stringr::str_remove_all("\\[|\\]")
      ) %>%
      dplyr::select(note)    
    
    # output
    app_note
    
  },
  error = function(e){
    paste0(
      "<div class = 'text-center p-2 border-danger rounded' style = 'background-color: #F8D7DA; color: #842029; font-size: 14px'>",  
      "Oops! Something went wrong. Wait a few minutes and try again or email us at basinscoutsupport@thefreshwatertrust.org.",
      "</div>"
    )
  })
  
}
