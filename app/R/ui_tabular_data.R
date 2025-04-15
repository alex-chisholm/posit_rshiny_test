# UI module for Tabular Data

ui_tabular_data <- function(id){
  
  ns <- NS(id)
  
  div(
    
    div(id = "prevent_click_screen_tabular_data",
        class = "shinyjs-hide",
        style = "
            position: fixed;
            margin-top: -10px;
            z-index: 5000;
            width: 100%;
            height: 100vh;
            background-color: rgba(0, 0, 0, 0);
            padding: 0;"
    ),

    div(
      style = "max-height: 83vh;",
      class = "fluid-container",

      div(id = "loading-indicator-table", class = "shinyjs-hide",
          span(class="spinner-border spinner-border-sm", role="status",
               `aria-hidden`="true", style="width: 2rem; height: 2rem;"),
          br(),
          "Loading..."
      ),
      div(
        class = "px-3 pt-3 pb-0",
        selectInput(
          inputId = ns("select_an_aoi"),
          label = tags$b("Select an AOI"),
          choices = letters[1:3],
          selected = letters[1]
        )
      ),
      br(),
      reactableOutput(ns("table_results"), width = "100%", height = "70vh")

    )
  )
  
}