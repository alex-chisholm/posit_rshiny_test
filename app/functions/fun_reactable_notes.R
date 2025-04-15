fun_reactable_notes <- function(data){
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
        html = T,
        headerStyle = "display: none; ",
        sortNALast = TRUE
      ),
      columns = list(
        note = colDef(
          style = "
            font-size: 14px;
            color: #232e3f;"
        )
      )
    )
}