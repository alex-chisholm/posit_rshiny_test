server_exploratory_map <- function(id, parent_session, tabular_aoi){
  moduleServer(
    id,
    function(input, output, session){
      
      
      # SHOW/HIDE SETTINGS PANEL =============================================
      panels_view <- reactiveValues(settings = "show", insights = "show")
      
      observeEvent(input$settings_panel_btn, {
        if(panels_view$settings == "show"){
          panels_view$settings <- "hide"
          updateActionLink(session, "settings_panel_btn", icon = icon("eye-slash"))
        }else{
          panels_view$settings <- "show"  
          updateActionLink(session, "settings_panel_btn", icon = icon("eye"))
        }
      })
      
      
      observeEvent(input$insights_panel_btn, {
        
        if(panels_view$insights == "show"){
          panels_view$insights <- "hide"
          updateActionLink(session, "insights_panel_btn", icon = icon("eye-slash"))
        }else{
          panels_view$insights <- "show"  
          updateActionLink(session, "insights_panel_btn", icon = icon("eye"))
        }
        
      })
      
      observe({
        
        if(panels_view$settings == "show" & panels_view$insights == "show"){
          
          shinyjs::show(id = "settings_panel", asis = TRUE, anim = T, animType = "fade")
          shinyjs::show(id = "insights_panel", asis = TRUE, anim = T, animType = "fade")
          
          shinyjs::addClass(id = "map_panel", class = "col-lg-6 col-md-4 col-sm-4", asis = TRUE) # map dimensions when settings is on & insights is on
          shinyjs::removeClass(id = "map_panel", class = "col-lg-10 col-md-9 col-sm-9", asis = TRUE) # map dimensions when settings is on & insights is off
          shinyjs::removeClass(id = "map_panel", class = "col-lg-8 col-md-7 col-sm-7", asis = TRUE) # map dimensions when settings is off & insights is on
          shinyjs::removeClass(id = "map_panel", class = "col-lg-12 col-md-12 col-sm-12", asis = TRUE) # map dimensions when settings is off & insights is off
          
          
        }else if(panels_view$settings == "show" & panels_view$insights == "hide"){
          
          shinyjs::show(id = "settings_panel", asis = TRUE, anim = T, animType = "fade")
          shinyjs::hide(id = "insights_panel", asis = TRUE, anim = T, animType = "fade")
          
          shinyjs::removeClass(id = "map_panel", class = "col-lg-6 col-md-4 col-sm-4", asis = TRUE) # map dimensions when settings is on & insights is on
          shinyjs::addClass(id = "map_panel", class = "col-lg-10 col-md-9 col-sm-9", asis = TRUE) # map dimensions when settings is on & insights is off
          shinyjs::removeClass(id = "map_panel", class = "col-lg-8 col-md-7 col-sm-7", asis = TRUE) # map dimensions when settings is off & insights is on
          shinyjs::removeClass(id = "map_panel", class = "col-lg-12 col-md-12 col-sm-12", asis = TRUE) # map dimensions when settings is off & insights is off
          
        }else if(panels_view$settings == "hide" & panels_view$insights == "show"){
          
          shinyjs::hide(id = "settings_panel", asis = TRUE, anim = T, animType = "fade")
          shinyjs::show(id = "insights_panel", asis = TRUE, anim = T, animType = "fade")
          
          shinyjs::removeClass(id = "map_panel", class = "col-lg-6 col-md-4 col-sm-4", asis = TRUE) # map dimensions when settings is on & insights is on
          shinyjs::removeClass(id = "map_panel", class = "col-lg-10 col-md-9 col-sm-9", asis = TRUE) # map dimensions when settings is on & insights is off
          shinyjs::addClass(id = "map_panel", class = "col-lg-8 col-md-7 col-sm-7", asis = TRUE) # map dimensions when settings is off & insights is on
          shinyjs::removeClass(id = "map_panel", class = "col-lg-12 col-md-12 col-sm-12", asis = TRUE) # map dimensions when settings is off & insights is off
          
          
        }else if(panels_view$settings == "hide" & panels_view$insights == "hide"){
          
          shinyjs::hide(id = "settings_panel", asis = TRUE, anim = T, animType = "fade")
          shinyjs::hide(id = "insights_panel", asis = TRUE, anim = T, animType = "fade")
          
          shinyjs::removeClass(id = "map_panel", class = "col-lg-6 col-md-4 col-sm-4", asis = TRUE) # map dimensions when settings is on & insights is on
          shinyjs::removeClass(id = "map_panel", class = "col-lg-10 col-md-9 col-sm-9", asis = TRUE) # map dimensions when settings is on & insights is off
          shinyjs::removeClass(id = "map_panel", class = "col-lg-8 col-md-7 col-sm-7", asis = TRUE) # map dimensions when settings is off & insights is on
          shinyjs::addClass(id = "map_panel", class = "col-lg-12 col-md-12 col-sm-12", asis = TRUE) # map dimensions when settings is off & insights is off
          
        }
        
      })
      
      observeEvent(clicked$change, {
        
        if(panels_view$insights == "hide"){
          
          panels_view$insights <- "show"  
          updateActionLink(session, "insights_panel_btn", icon = icon("eye"))
          
          if(panels_view$settings == "hide"){
            
            shinyjs::removeClass(id = "map_panel", class = "col-lg-6 col-md-4 col-sm-4", asis = TRUE) # map dimensions when settings is on & insights is on
            shinyjs::addClass(id = "map_panel", class = "col-lg-10 col-md-9 col-sm-9", asis = TRUE) # map dimensions when settings is on & insights is off
            shinyjs::removeClass(id = "map_panel", class = "col-lg-8 col-md-7 col-sm-7", asis = TRUE) # map dimensions when settings is off & insights is on
            shinyjs::removeClass(id = "map_panel", class = "col-lg-12 col-md-12 col-sm-12", asis = TRUE) # map dimensions when settings is off & insights is off
            
          }else{
            
            shinyjs::addClass(id = "map_panel", class = "col-lg-6 col-md-4 col-sm-4", asis = TRUE) # map dimensions when settings is on & insights is on
            shinyjs::removeClass(id = "map_panel", class = "col-lg-10 col-md-9 col-sm-9", asis = TRUE) # map dimensions when settings is on & insights is off
            shinyjs::removeClass(id = "map_panel", class = "col-lg-8 col-md-7 col-sm-7", asis = TRUE) # map dimensions when settings is off & insights is on
            shinyjs::removeClass(id = "map_panel", class = "col-lg-12 col-md-12 col-sm-12", asis = TRUE) # map dimensions when settings is off & insights is off
            
          }
          
        }
        
      })
      
      # MAP ==================================================================
      output$exploratory_map <- renderLeaflet({
        
        # show loading indicator
        shinyjs::show(id = "loading-indicator-map", asis = TRUE)
        
        map <- fun_exploratory_map()
        
        # map <- fun_update_exploratory_map(
        #   map = map,
        #   feature = initial_feature   
        # )
        
        # hide loading indicator
        shinyjs::delay(
          500, shinyjs::hide(id = "loading-indicator-map", asis = TRUE)
        )
        
        map
      })
      
      outputOptions(output, "exploratory_map", suspendWhenHidden = FALSE)
      
      # BASE MAP ============================================================
      observeEvent(input$base_map, {
        
        base_map <- ifelse(
          input$base_map == "Base Map", "CartoDB.Positron", ifelse(
            input$base_map == "Base Map+","OpenStreetMap.Mapnik", "USGS.USTopo"
          )
        )
        
        leafletProxy("exploratory_map") %>%
          clearGroup("base_map") %>%
          addProviderTiles(base_map, group = "base_map")
      })
      
      # CLICK ON POLYGONS OR CIRCLES =========================================
      clicked <- reactiveValues(
        id = as.integer(),
        group = as.character(""),
        change = 0
      )
      
      # click on polygon event
      observeEvent(input$exploratory_map_shape_click, {
        click <- input$exploratory_map_shape_click
        clicked$id <- click$id
        clicked$group <- click$group
        clicked$change <- clicked$change + 1
      })
      
      # click on circle marker event
      observeEvent(input$exploratory_map_marker_click, {
        click <- input$exploratory_map_marker_click
        clicked$id <- click$id
        clicked$group <- ifelse(is.null(click$group), "Diversions", click$group)
        clicked$change <- clicked$change + 1
      })
      
      
      # SETTINGS PANEL ========================================================
      settings_status <- reactiveValues(
        aoi = letters[1:3]
      )
      
      ## click btn update_settings ============================================
      observeEvent(input$update_settings, {
  
        ### show loading indicator
        shinyjs::show(id = "loading-indicator-map", asis = TRUE)
        shinyjs::show(id = "prevent_click_screen_exploratory_map", asis = TRUE)
        
        settings_status$aoi <- input$select_an_aoi
        
        m <- leafletProxy("exploratory_map")
        
        # fun_update_exploratory_map(
        #   m,
        #   feature = settings_status$feature
        # )
        
        # update home page selection
        updateSelectInput(parent_session, "home-select_an_aoi", 
                          selected = settings_status$aoi)
        
        ### hide loading indicator
        shinyjs::delay(
          200, shinyjs::hide(id = "loading-indicator-map", asis = TRUE)
        )
        shinyjs::delay(
          200, shinyjs::hide(id = "prevent_click_screen_exploratory_map", asis = TRUE)
        )
        
        ### update selection 
        clicked$id <- as.integer()
        clicked$group <- ""
        clicked$change <- clicked$change + 1
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # INSIGHTS PANEL ========================================================
      
      panel_insights <- reactiveValues(
        title = NULL,
        table1 = NULL,
        table2 = NULL,
        note = NULL
      )
      
      ## update insights title, tables and notes =============================
      # observeEvent(clicked$change, {
      #   
      #   if(clicked$group %in% c("Diversions")){
      #     
      #     results <- fun_get_structure_attributes(clicked$id)
      #     panel_insights$title <- results$title
      #     panel_insights$table1 <- results$table1
      #     panel_insights$table2 <- results$table2
      #     panel_insights$note <- fun_get_structure_notes(clicked$id, ns = session$ns)
      #     
      #   }else  if(clicked$group %in% c("Fields")){
      #     
      #     results <- fun_get_field_attributes(clicked$id)
      #     panel_insights$title <- results$title
      #     panel_insights$table1 <- results$table1
      #     panel_insights$table2 <- results$table2
      #     panel_insights$note <- fun_get_field_notes(clicked$id, ns = session$ns)
      #     
      #   }else  if(clicked$group %in% c("Structures")){
      #     
      #     results <- fun_get_infra_attributes(clicked$id)
      #     panel_insights$title <- results$title
      #     panel_insights$table1 <- results$table1
      #     panel_insights$table2 <- results$table2
      #     panel_insights$note <- fun_get_infra_notes(clicked$id)
      #     
      #   }else if(clicked$group %in% c("County","Division","District","Econ. Region","HUC10","HUC12")){
      #     
      #     type <- ifelse(clicked$group == "Econ. Region", "econreg", tolower(clicked$group))
      #     results <- fun_get_boundary_attributes(clicked$id, type)
      #     panel_insights$title <- results$title
      #     panel_insights$table1 <- results$table1
      #     panel_insights$table2 <- results$table2
      #     panel_insights$note <- fun_get_boundary_notes(clicked$id, type)
      #     
      #   }else if(clicked$group %in% c("Streams")){
      #     
      #     results <- fun_get_stream_attributes(clicked$id)
      #     panel_insights$title <- results$title
      #     panel_insights$table1 <- results$table1
      #     panel_insights$table2 <- results$table2
      #     panel_insights$note <- fun_get_stream_notes(clicked$id, ns = session$ns)
      #     
      #   }else{
      #     
      #     results <- fun_get_app_insights(input$theme)
      #     panel_insights$title <- results$title
      #     panel_insights$table1 <- results$table1
      #     panel_insights$table2 <- results$table2
      #     panel_insights$note <- fun_get_app_notes(settings_status$theme, ns = session$ns)
      #     
      #   }
      # })
      # 
      output$panel_insights_title <- renderText({
        panel_insights$title
      })
      
      output$panel_insights_table1 <- renderReactable({
        if(!is.null(nrow(panel_insights$table1))){
          panel_insights$table1 %>%
            fun_reactable_insights()
        }else{
          NULL
        }
      })
      
      output$panel_insights_table2 <- renderReactable({
        if(!is.null(nrow(panel_insights$table2))){
          panel_insights$table2 %>%
            fun_reactable_insights()
        }else{
          NULL
        }
      })
      
      output$panel_insights_note <- renderReactable({
        if(!is.null(nrow(panel_insights$note))){
          panel_insights$note %>%
            fun_reactable_notes()
        }else{
          NULL
        }
      })
      
      # FROM TABULAR ==========================================================
      observeEvent(tabular_aoi$aoi, {

        updateSelectInput(session, "select_an_aoi", selected = tabular_aoi$aoi)
        
        shinyjs::delay(
          100, shinyjs::click("exploratory_map-update_settings", asis = TRUE)
        )
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      ## update aoi in tabular if exploratory map changed aoi ================
      observeEvent(settings_status$aoi, {
        
        updateSelectInput(parent_session, "tabular_data-select_an_aoi",
                          selected = settings_status$aoi)
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # RETURN  ===============================================================
      # return objects to be used in another module
      return(
        list(settings_status = settings_status)
      )
      
    })
}
