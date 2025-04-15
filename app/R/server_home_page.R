server_home_page <- function(id, parent_session){
  moduleServer(
    id,
    function(input, output, session){
      
      # loading ==============================================================
      prevent_click <- reactiveValues(start = TRUE)

      observeEvent(prevent_click$start, {
        
        shinyjs::delay(
          10,
          shinyjs::hide(id = "prevent_click_screen_home", asis = TRUE, anim = TRUE, animType = "fade")
        )
        
      }, ignoreInit = FALSE, ignoreNULL = FALSE)
      
      
      # exploratory map ======================================================

      observeEvent(input$go2expmap, {
        
        updateTabsetPanel(parent_session, "pages", selected = "Exploratory Map")
        
        updateSelectInput(parent_session, "exploratory_map-select_an_aoi", selected = input$select_an_aoi)
        
        shinyjs::delay(
          1000, shinyjs::click("exploratory_map-update_settings", asis = TRUE)
        )
        
      })
      
      # methods & analysis ========================================================
      observeEvent(input$go2reports_user_guide, {
        updateTabsetPanel(parent_session, "pages", selected = "Reports")
        updateNavlistPanel(parent_session, "reports_list", selected = "user_guide")
      })
      
      observeEvent(input$go2reports_methods, {
        updateTabsetPanel(parent_session, "pages", selected = "Reports")
        updateNavlistPanel(parent_session, "reports_list", selected = "methods")
      })
      
      observeEvent(input$go2reports_data_validation, {
        updateTabsetPanel(parent_session, "pages", selected = "Reports")
        updateNavlistPanel(parent_session, "reports_list", selected = "data_validation")
      })
      
      
      
    })
}