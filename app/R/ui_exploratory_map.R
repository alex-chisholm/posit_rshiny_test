# UI module for Exploratory map

ui_exploratory_map <- function(id){
  
  ns <- NS(id)
  
  div(class = "row p-0 m-0",
      
      div(id = "prevent_click_screen_exploratory_map",
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
      
      # SETTINGS PANEL =======================================================
      div(class = "col-lg-2 col-md-3 col-sm-3 pe-0",
          id = "settings_panel",
          div(
            style = "height:88vh;",
            div(
              class = "col-12",
              div(id = "settings_card", class = "card border-secondary", style = "height:88vh;",
                  div(class = "card-header text-uppercase fw-bold text-center bg-secondary", 
                      style = "letter-spacing: .1em;",
                      div(
                        class = "d-flex justify-content-center",
                        div(class = "pe-1", "Settings")
                      )
                  ),
                  div(
                    class = "card-body bg-light p-0",
                    id = "settings_panel_content",
                    style = "overflow-y: scroll; overflow-x: hidden; width: 100%",
                    
                    div(
                      class = "px-3 pt-3 pb-0",
                      selectInput(
                        inputId = ns("select_an_aoi"), 
                        label = tags$b("Select an AOI"),
                        choices = letters[1:3],
                        selected = letters[1]
                      )
                    ),
                    
                    div(class = "text-center mt-2",
                        actionButton(
                          inputId = ns("update_settings"), 
                          label = "Update Settings",
                          css.class = "btn btn-dark btn-sm text-light",
                          styleclass = "letter-spacing: 5px;"
                        )
                    )
                    
                  )
              )
            )
          )
      ),
      # MAP PANEL ============================================================
      # div(class = "col-lg-7 col-md-6 col-sm-5 pe-0",
      div(class = "col-lg-6 col-md-4 col-sm-4",
          id = "map_panel",
          
          # loading indicator
          div(id = "loading-indicator-map", class = "shinyjs-hide",
              span(class="spinner-border spinner-border-sm", role="status",
                   `aria-hidden`="true", style="width: 2rem; height: 2rem;"),
              br(),
              "Loading..."
          ),
          
          wellPanel(
            class = "bg-white p-2",
            style = "height:88vh;",
            
            leafletOutput(ns("exploratory_map"), height = "85vh"),
            
            div(id = "top_dropdowns_and_btns",
                class = "d-flex justify-content-center",
                
                div(
                  id = "base_map_dropdown",
                  class = "pe-1",
                  selectInput(
                    inputId = ns("base_map"), 
                    label = NULL, 
                    choices = c("Base Map","Base Map+","Topo Map"),
                    selected = "Base Map",
                    width = 150
                  )
                )
            ),
            
            div(
              id= "map_btns",
              class = "d-flex justify-content-center",
              div(
                id = "zoom_in_btn",
                class = "mx-2 shinyjs-hide",
                actionButton(
                  inputId = ns("zoom_in"), 
                  label = "Zoom In",
                  css.class = "btn btn-dark btn-sm text-light text-nowrap",
                  styleclass = "letter-spacing: 5px;"
                )
              ),
              div(
                id = "zoom_out_btn",
                class = "mx-2 shinyjs-hide",
                actionButton(
                  inputId = ns("zoom_out"), 
                  label = "Reset Zoom",
                  css.class = "btn btn-dark btn-sm text-light text-nowrap",
                  styleclass = "letter-spacing: 5px;"
                )
              ),
              div(
                id = "clear_map_selection_btn",
                class = "shinyjs-hide mx-2",
                actionButton(
                  inputId = ns("clear_map_selection"), 
                  label = "Clear Selection",
                  css.class = "btn btn-dark btn-sm text-light text-nowrap",
                  styleclass = "letter-spacing: 5px;"
                )
              ),
              div(
                class = "mx-2",
                actionLink(
                  inputId = ns("settings_panel_btn"),
                  label = "Settings",
                  icon = icon("eye"),
                  class = "btn btn-dark btn-sm text-light text-nowrap"
                )
              ),
              div(
                class = "mx-2",
                actionLink(
                  inputId = ns("insights_panel_btn"),
                  label = "Insights",
                  icon = icon("eye"),
                  class = "btn btn-dark btn-sm text-light text-nowrap"
                )
              )
            )
          )
      ),
      # INSIGHTS PANEL =======================================================
      div(class = "col-lg-4 col-md-5 col-sm-5 ps-0",
          id = "insights_panel",
          class = "shinyjs-hide",
          div(
            style = "height:88vh;",
            
            div(
              class = "col-12",
              div(class = "card border-secondary", style = "height:88vh;",
                  div(class = "card-header text-uppercase fw-bold text-center bg-secondary", 
                      style = "letter-spacing: .1em;", 
                      div(
                        class = "d-flex justify-content-center",
                        div(class = "pe-1", "Insights")
                      )
                  ),
                  
                  div(
                    class = "card-body border-secondary rounded",
                    style = "overflow-y: scroll; overflow-x: scroll;", 
                    
                    ## title section =========================================
                    div(
                      class = "row d-flex justify-content-center pb-0 mb-0",
                      htmlOutput(ns("panel_insights_title"))
                    ),
                    
                    ## tables section ========================================
                    div(
                      class = "row d-flex justify-content-center px-0",
                      style = "max-height: 300px;  min-width:400px; max-width: 600px;",
                      div(
                        class = "col-lg-6 col-md-12 col-sm-12 text-center m-0 px-1",
                        reactableOutput(ns("panel_insights_table1"))
                      ),
                      div(
                        class = "col-lg-6 col-md-12 col-sm-12 text-center m-0 px-1",
                        reactableOutput(ns("panel_insights_table2"))
                      )
                    ),
                    
                    ## note section ==========================================
                    div(
                      class = "row d-flex justify-content-center text-center mt-3",
                      style = "margin-left:-20px; margin-right:-20px; min-width:400px; max-width: 600px;",
                      reactableOutput(ns("panel_insights_note"), width = "100%")
                    ),
                    
                    ## plot section ==========================================
                    div(
                      id = "plot_section",
                      div(
                        class = "d-flex row justify-content-center mt-3 p-2 bg-light",
                        # plot
                        div(
                          class = "d-flex row justify-content-center",
                          #highchartOutput(ns("insights_plot_selected"), height = "300px", width = "100%")
                        ),
                        # more info
                        div(
                          style = "font-size: 14px;",
                          class = "pb-3",
                          uiOutput(ns('plot_text'))
                          
                        ),
                        # dropdown selector plot
                        div(
                          class = "d-flex row justify-content-center",
                          style = "font-size: 13px;",
                          uiOutput(ns("insights_plots_list"))
                        )
                      )
                    )
                  )
              )
            )
          )
      )
      
  )
  
  
  
  
}
