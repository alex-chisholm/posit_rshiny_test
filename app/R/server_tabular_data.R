server_tabular_data <- function(id, parent_session, settings_status){
  moduleServer(
    id,
    function(input, output, session){
      
      
      # get initial table ===================================================
      table <- reactiveValues(
        init = tibble::tibble()
      )
      
      observeEvent(settings_status$aoi,  {
        # update table
        # table$init <- fun_get_tabular_data(aoi = map_settings$aoi)
        
        shinyjs::delay(200, shinyjs::hide(id = "loading-indicator-table", asis = TRUE))
        
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # render table ========================================================
      output$table_results <- renderReactable({
        
        ns <- session$ns
        
        data <- table$init %>%
          dplyr::mutate(`Zoom` = "") %>%
          dplyr::select(`Zoom`, everything())
        
        columns <- colnames(data)
        reactable(
          data,
          fullWidth = TRUE,
          highlight = TRUE,
          filterable = TRUE,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 20, 50, 100, nrow(data)),
          defaultPageSize = 100,
          theme = reactableTheme(cellPadding = "10px"),
          style = "
            border: 1px solid rgba(0, 0, 0, 0.15) ;
            border-radius: 4px;
            box-shadow: 0 2px 7px 0 rgba(0, 0, 0, 0.05);
            background-color: #F4F7FB;
            color: black;
            font-size: 14px;
            font-family: 'Source Sans Pro', sans-serif;",
          rowStyle = "  background-color: white;",
          defaultColDef = colDef(
            minWidth = 120,
            align = "center",html = T,
            vAlign = "center",
            headerStyle = "
              border-bottom-color: rgba(0, 0, 0, 0.15);
              border-bottom-width: 2px;
              color: #4A5F75;
              font-family: 'Source Sans Pro', sans-serif;"
          ),
          columns = list(
            `Zoom` = colDef(
              name = "Zoom",
              header = fun_with_tooltip("Zoom", "Zoom to feature on Exploratory Map."),
              align = "center",
              width = 120,
              sortable = FALSE,
              filterable = FALSE,
              cell =  function(value, rowIndex, colName){
                as.character(tags$div(
                  htmltools::tags$button(
                    "View Field",
                    class = "btn btn-outline-secondary btn-sm",
                    onclick = paste0("Shiny.setInputValue('tabular_data-clicked_table_exp_map', { index: ",
                                     rowIndex,
                                     "}, { priority: 'event' })"
                    )
                  )
                )
                )
              }
            ),
            acres = colDef(
              filterMethod = fun_reac_filterNumber,
              format = colFormat(separators = TRUE, digits = 0, locales = "en-US")
            ),
            n_uplift = colDef(
              filterMethod = fun_reac_filterNumber,
              format = colFormat(separators = TRUE, digits = 2, percent = FALSE, locales = "en-US")
            )
          )
        )
        
      })
      
      outputOptions(output, "table_results", suspendWhenHidden = FALSE)
      
      
      # click on "View Field" button =========================================
      
      # clicked <- reactiveValues(
      #   id = as.integer(),
      #   group = "Fields",
      #   change = 0
      # )
      # 
      # observeEvent(input$clicked_table_exp_map,{
      #   
      #   field_id <- table$init %>%
      #     dplyr::slice(input$clicked_table_exp_map$index) %>%
      #     dplyr::pull(field_id) %>% as.integer()
      # 
      #   updateTabsetPanel(parent_session, "pages", selected = "Exploratory Map")
      # 
      #   clicked$id <- field_id
      #   clicked$change <- clicked$change + 1
      #   
      # }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # store aoi from tabular to be used in exp map =========================
      tabular_aoi <- reactiveValues(aoi = letters[1])
      observeEvent(input$select_an_aoi, {
        
        tabular_aoi$aoi <- input$select_an_aoi
        
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # RETURN  ===============================================================
      # return objects to be used in another module
      return(
        list(tabular_aoi = tabular_aoi)
      )
      
      
      
    })
}

