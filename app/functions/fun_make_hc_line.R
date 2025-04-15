fun_make_hc_line <- function(data, line_color = "#737373", title = "Plot Title", yaxis_title = "", xaxis_title = ""){
  
  
  hc <- highchart()
  series_list <- data$series %>% unique()
  purrr::walk(
    seq_along(series_list),
    ~{
      hc <<- hc %>%
        hc_add_series(
          data = data %>% dplyr::filter(series == series_list[.x]),
          type = 'line',
          hcaes(x = key, y = val),
          name = series_list[.x],
          showInLegend = ifelse(length(series_list) == 1, FALSE, TRUE)
        ) 
    }
  )
  
  hc <- hc %>%
    hc_tooltip(
      useHTML = T,
      headerFormat = paste0("{series.name}<br>"),
      pointFormat = paste0("<span style='color:{point.color}'>\u25CF</span> {point.key}: <b>{point.val:,.2f}"),
      style = list(fontSize = "14px")
    ) %>%
    hc_xAxis(categories = data$key %>% unique(),
             title = list(text = xaxis_title)) %>%
    hc_yAxis(
      plotLines = list(
        list(
          value = 0,   
          color = "black", 
          width = 1,      
          zIndex = 4
        )
      ),
      title = list(text = yaxis_title)
    ) %>%
    hc_title(
      text = paste0("<div style='text-align:center;font-weight:normal;
   font-size:16px; font-family: Source Sans Pro;'>",title,"</div>")
    ) %>%
    hc_add_dependency("modules/exporting.js") %>%
    hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen","downloadPNG")
        )
      )
    ) %>%
    hc_chart(
      backgroundColor = "#f8f9fa"
    ) 
  
  hc
  
}
