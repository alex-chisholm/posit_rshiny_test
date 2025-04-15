fun_get_stream_notes <- function(id, ns){
  
  f_name <- ns("setinput") %>% stringr::str_replace("-","_")
  tryCatch({
    # query
    query <- "select * from project_wsba.get_stream_notes(id)" %>%
      stringr::str_replace("id", as.character(id)) %>%
      glue::glue_collapse(sep = "\n")
    
    # collect data
    data <- dbGetQuery(con, query)
    
    stream_note <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        note = paste0("<b>", note) %>% stringr::str_replace(":",":</b>"),
        links = note %>%
          stringr::str_extract_all("\\[(.*?)\\]") %>%
          unlist() %>%
          stringr::str_remove_all("\\[|\\]") %>% 
          list(),
        links_replacement = list(as.character(NA))
      ) %>%
      dplyr::ungroup()
    
    stream_note$links_replacement <- purrr::map(
      1:nrow(stream_note),
      ~{
        
        tryCatch({
          
          lapply(
            stream_note$links[.x] %>% unlist() %>% stringr::str_split(":"),
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
               
             }else if(x[3] %in% c("HUC10","HUC12")){
                  
                  if(stringr::str_detect(x[1],"_stream")){
                    paste0(
                      "<a id = '", ns('new_stream'), "' class ='action-button' onclick = '",f_name,"(\"new_stream\",\"",
                      x[2],"\",\"", x[3],"\");' href = '", x[2],"'>", x[3],"</a>"
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
    for(i in 1:nrow(stream_note)){
      purrr::walk(
        seq_along(stream_note$links[i] %>% unlist()),
        ~{
          stream_note$note[i] <<- stringr::str_replace(
            string = stream_note$note[i], 
            pattern = stream_note$links[i] %>% unlist() %>% .[.x],
            replacement = stream_note$links_replacement[i] %>% unlist() %>% .[.x]
          )
        }
      )
    }
    
    stream_note <- stream_note %>%
      dplyr::mutate(
        note = note %>% stringr::str_remove_all("\\[|\\]")
      ) %>%
      dplyr::select(note)    
    
    # output
    stream_note
    
  },
  error = function(e){
    paste0(
      "<div class = 'text-center p-2 border-danger rounded' style = 'background-color: #F8D7DA; color: #842029; font-size: 14px'>",  
      "Oops! Something went wrong. Wait a few minutes and try again or email us at basinscoutsupport@thefreshwatertrust.org.",
      "</div>"
    )
  })
  
}
