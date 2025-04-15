fun_make_hc_boxplot <- function(data, title = "Plot Title") {
  
  data2 <- data_to_boxplot(data, variable = val, group_var = key, add_outliers = TRUE)
  data2 <- data2 %>%
    dplyr::mutate(
      fillColor = "#FF9AA2",
      color = "#C30010",
      showInLegend = FALSE,
      name = ""
    )
  
  data2$tooltip[[1]] <-  list(
    useHTML = TRUE,
    pointFormat = paste0('<b>{point.category}</b><br>',
                         '- Min: {point.low:,.f}<br>',
                         '- Q1: {point.q1:,.f}<br>',
                         '- Median: {point.median:,.f}<br>',
                         '- Q3: {point.q3:,.f}<br>' ,
                         '- Max: {point.high:,.f}'
    ),
    headerFormat = ""
  )
  
  if(nrow(data2) == 2){
    data2$marker[[2]] <- list(
      radius = 4,
      lineWidth = 1
    )
    # data2$tooltip <- NA
    
    data2$tooltip[[2]] <-  list(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = "<span style='color:{point.color}'>\u25CF</span> <b>Outlier:</b> {point.y:,.f}"
    )
  }
  
  
  hc <- highchart() %>%
    hc_add_series_list(data2) %>%
    hc_xAxis(categories = purrr::map_chr(seq_along(data2$data[[1]]), ~data2$data[[1]][[.x]]$name)) %>%
    hc_yAxis(
      labels = list(
        format = ("{text}")
      )
    ) %>%
    hc_plotOptions(
      boxplot = list(
        groupPadding = 0.1
        
      )
    ) %>%
    hc_tooltip(
      style = list(fontSize = "14px") 
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
