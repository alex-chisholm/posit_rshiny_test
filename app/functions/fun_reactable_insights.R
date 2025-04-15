fun_reactable_insights <- function(data, header = FALSE, col_names = c("","")){
  
  data %>%
    reactable:: reactable(
      highlight = TRUE, 
      bordered = TRUE,
      striped = TRUE, 
      compact = TRUE,
      wrap = TRUE,
      resizable = FALSE,
      fullWidth = TRUE, 
      showPageSizeOptions = FALSE, 
      pagination = FALSE,
      defaultColDef = colDef(
        # headerClass = "insights-table-header",
        headerStyle = ifelse(header, "", "display: none; "),
        align = "center",
        style = "
            font-size: 14px;
             color: #232e3f;",
        #na = "N/A",
        
        sortNALast = TRUE
      ),
      columns = list(
        key = colDef(
          style = "
            font-weight: bold;
            text-align: left;
            font-size: 14px;",
          width = ifelse(ncol(data) == 2, 100, 200),
          header = "",
          html = TRUE,
          cell = function(value){
            if(stringr::str_detect(value, "\\{\\{")){
              
              key <-  stringr::str_remove_all(value, "\\{\\{.*") %>% stringr::str_trim()
              text <- stringr::str_remove_all(value, ".*\\{\\{|\\}\\}")
              fun_with_tooltip(
                value = key,
                placement = "bottom-start",
                tooltip = text
              )
            }else{
              value
            }
          }
        ),
        val = colDef(
          minWidth  = 100
        ),
        val1 = colDef(
          width = 140,
          header = col_names[1]
        ),
        val2 = colDef(
          width = 140,
          header = col_names[2]
        )
      )
    )
}