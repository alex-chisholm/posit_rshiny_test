shinyServer(function(session, input, output) {

  # user session ==============================================================
  
  session$onSessionEnded(function() {

    # decrement the session count
    isolate(vals$count <- vals$count - 1)
    isolate(cat("sessions: ",vals$count,fill=T))

    # debug db pool
    # print(dbGetInfo(con))
    
    # bad idea, causes errors since db pool is auto managed 
    #poolClose(con)
    #print("db conn pool closed")

    # bad idea, stops entire app if single user ends session:
    #stopApp()
    #print("app stopped")
    
  })
  
  session_val <- reactiveValues(start = TRUE, user_values = list())
  output$count <- renderText({
    vals$count
  })
  
  observeEvent(session_val$start, {
  if(session_val$start) {

    # debug db pool
    # print(dbGetInfo(con))    
    
    # increment the session count
    isolate(vals$count <- vals$count + 1)
    cat("sessions: ",vals$count, fill=T)

    # get db settings
    # session_val$user_values <- fun_get_last_session_parameters(user = session$user)
    
    session_val$start <- FALSE
  
  }
  }, ignoreInit = FALSE)
  
  
  # Home tab ==================================================================
  server_home_page("home", parent_session = session)
  
  # Exploratory Map tab =======================================================
  items_map = server_exploratory_map("exploratory_map", parent_session = session,
                                     tabular_aoi = items_tabular$tabular_aoi)
  
  # tabular data ==============================================================
  items_tabular = server_tabular_data("tabular_data", parent_session = session,
                      settings_status = items_map$settings_status)
  
  # Quarto Reports tab ========================================================
  server_reports("reports", parent_session = session)
  
  # Contact tab ===============================================================
  #server_contact("contact") 
  
})

