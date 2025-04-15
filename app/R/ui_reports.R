# UI module for Reports

ui_reports <- function(id){
  
  ns <- NS(id)
  
  div(
    class = "container-fluid",
    
    # loading indicator ======================================================
    div(
      id = "loading-indicator-report", class = "shinyjs-hide text-center",
      style = "
      position: absolute;
      top: 200px;
      left: 50%;
      z-index: 1050;
      transform: translate(-50%, -50%);
      ",
      span(class="spinner-border spinner-border-sm", role="status",
             `aria-hidden`="true", style="width: 2rem; height: 2rem;"),
        br(),
        "Loading..."
    ),
    
    # reports menu 
    navlistPanel(
      widths = c(3,9),
      well = FALSE,
      id = "reports_list",
      
      tabPanel(
        title = "User Guide", 
        value = "user_guide", 
        wellPanel(class = "bg-white p-0 m-0", uiOutput(ns("user_guide")))
      ),
      
      tabPanel(
        title = "Methods", 
        value = "methods", 
        wellPanel(class = "bg-white p-0 m-0", uiOutput(ns("methods")))
      ),
      
      tabPanel(
        title = "Data Validation", 
        value = "data_validation", 
        wellPanel(class = "bg-white p-0 m-0", uiOutput(ns("data_validation")))
      )
      
    )
    
  )
  
}
