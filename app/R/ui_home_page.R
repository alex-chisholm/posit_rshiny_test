# UI module: home page

ui_home_page <- function(id){
  
  ns <- NS(id)
  
  div(
    class = "container-fluid",
    
    # initial loading page ===================================================
    div(id = "prevent_click_screen_home",
        style = "
            position: fixed;
            margin: -80px 0 0 -25px;
            z-index: 5000;
            width: 100%;
            height: 101vh;
            background-color: rgba(255, 255, 255, 0.97);",
        
        div(
          class = "d-flex flex-column align-items-center justify-content-center",
          style = "height: 100%",
          img(src = "img/TFT_logo.png", height = "200px", style = ""),
          div(class="spinner-border text-success", role="status",
              `aria-hidden`="true", style="width: 3rem; height: 3rem;"),
          div("Please Wait", class = "fs-2 pt-2")
        )
    ),
    
    
    div(
      style = "",
      class = "d-flex flex-column justify-content-center align-items-center align-content-around",
      
      # app title ============================================================
      div(class = "pt-1 pb-2 w-100 mb-2",
          
          p(
            class = "text-uppercase fw-bold text-center",
            style = "font-size: 4.5em;",
            "PROJECT NAME"
          )
          
      ),
      
      div(class = "d-flex flex-row", textOutput("count"), div(class="", style="margin-left:5px", "session(s) connected to this app.")),
      
      div(class = "d-flex flex-row",
          
          # card: project overview ============================================
          # brief description of the project (replace lorem ipsum)
          div(
            class = "card m-2",
            style = "width: 20rem;  height: 15.5rem; background-color: #232e3f",
            div(class="card-img-overlay",
                h5(
                  class = "card-title text-light text-start text-capitalize fw-bold",
                  "About This Project"
                ),
                p(
                  class = "card-text text-light",
                  "Lorem ipsum odor amet, consectetuer adipiscing elit. 
                  Vivamus fermentum facilisi aptent ultrices feugiat vel. 
                  Nascetur montes quam tempor suspendisse per. In varius et 
                  molestie sem nostra vivamus facilisis mauris. 
                  Curae natoque elit montes malesuada potenti turpis."
                )
            )
          ),
          
          # card: exploratory map ============================================
          # add inputs that interact with exploratory map
          div(
            class = "card m-2",
            style = "width: 20rem; height: 15.5rem; background-color: #e2a583",
            div(class="card-img-overlay",
                h5(
                  class = "card-title text-start text-capitalize fw-bold",
                  "Exploratory Map"
                ),
                p(
                  class = "card-text",
                  "View all available insights for a targeted geographic area:"
                ),
                selectInput(
                  inputId =  ns("select_an_aoi"),
                  label = tags$b("Select an AOI"),
                  selected = letters[1],
                  choices = letters[1:3],
                ),
                actionButton(inputId = ns("go2expmap"), "GO", css.class = "btn btn-sm btn-primary")
            )
          ),
          
          # card: methods and analysis =======================================
          div(
            class = "card m-2 align-text-top", 
            style = "width: 20rem; height: 15.5rem; background-color: #afc4a1",
            div(class="card-img-overlay",
                
                h5(
                  class = "card-title text-start text-capitalize fw-bold",
                  "Documentation"
                ),
                p(
                  class = "card-text",
                  "Review documentation on data sources and methods, as well as 
                  some of the supporting analysis behind the geospatial and
                  statistical insights."
                ),
                
                actionLink(inputId = ns("go2reports_user_guide"), "User Guide"),
                "  *  ",
                actionLink(inputId = ns("go2reports_methods"), "Methods"),
                "  *  ",
                actionLink(inputId = ns("go2reports_data_validation"), "Data Validation")
                
                
            )
          )
          
      ),
      
      # developed by ==========================================================
      
      div(class = "m-3 text-center",
          div("Developed by", class = "text-uppercase fw-bold"),
          div(class = "d-flex flex-wrap justify-content-center",
              div(class = "m-3 text-center text-uppercase fw-bold",
                  div("", class = "m-0 p-1"),
                  div(class = "",
                      tags$a(
                        class = "text-decoration-none",
                        href = "https://www.thefreshwatertrust.org/", target="_blank",
                        tags$img(src = "img/TFT_logo.png", height = "70px",
                                 title = "The Freshwater Trust")
                      )
                  )
              )
          )
      )
      
    )
    
  )
  
}
