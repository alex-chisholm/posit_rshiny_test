fun_make_hc_pie_chart <- function(data, donut = TRUE, title = "", val_type = "", show_val = TRUE) {
  
  data <- data %>%
    tibble::tibble() %>%
    dplyr::mutate(
      key = stringr::str_trim(key),
      val = as.numeric(val)
    )
  
  if(!("color" %in% colnames(data))){
    data <- data %>%
      dplyr::mutate(color = hcl.colors(nrow(data),"dynamic"))
  }
  
  innerSize <- ifelse(donut, "50%", "0%")
  hover_info <- ifelse(
    show_val,
    paste0("<span style='color:{point.color}'>\u25CF</span><b>{point.y:,.0f} ", val_type, "</b>({point.percentage:.1f}%)"),
    paste0("<span style='color:{point.color}'>\u25CF</span>{point.percentage:.1f}%")
  )
  hc <- highchart() %>%
    hc_add_series(
      type = "pie", data = data, hcaes(x = key, y = val),
      innerSize = innerSize
    ) %>%
    hc_colors(data$color) %>%
    hc_title(
      text = paste0("<div style='text-align:center;font-weight:normal;
   font-size:16px; font-family: Source Sans Pro;'>", title, "</div>")
    ) %>%
    hc_tooltip(
      useHTML = T, 
      headerFormat = "<b>{point.key}</b><br>",
      pointFormat = hover_info,
      style = list(fontSize = "14px") 
    ) %>%
    hc_plotOptions(
      pie = list(
        dataLabels = list(
          useHTML = T,
          enabled = TRUE,
          format = "<div style = 'text-align:center; font-weight:normal'><b>{point.name}</b><br>{point.percentage:.1f}%</div>",
          style = list(fontSize = "12px")
          # distance = 10
        )
      )
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
