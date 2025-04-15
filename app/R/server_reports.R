server_reports <- function(id, parent_session){
  moduleServer(
    id,
    function(input, output, session){
      
      output$user_guide <- renderUI({
        
        tags$iframe(
          seamless="seamless",
          src = paste0("quarto-reports/user_guide.html"),
          width = "100%",
          onload="self.scrollTo(0,0)"
        )
        
      })
      
      output$methods <- renderUI({
        
        tags$iframe(
          seamless="seamless",
          src = paste0("quarto-reports/methods.html"),
          width = "100%",
          onload="self.scrollTo(0,0)"
        )
        
      })
      
      output$data_validation <- renderUI({
        
        tags$iframe(
          seamless="seamless",
          src = paste0("quarto-reports/data_validation.html"),
          width = "100%",
          onload="self.scrollTo(0,0)"
        )
        
      })
      
      
      
      
    })
}
