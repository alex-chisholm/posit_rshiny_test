fun_make_hc_econ_output <- function(data) {
  
  
  if(nrow(data) == 1){
    categories <- list(data$key)
  }else{
    categories <- data$key
  }
  
  
  hc <- highchart() %>%
    hc_add_series(
      data = data,
      type = 'column',
      hcaes(x = key, y = direct),
      name = "Direct",
      color = '#232e3f',
      tooltip = list(
        useHTML = T, 
        headerFormat = "<span style='color:{point.color}'>\u25CF</span> {point.key}<br>",
        pointFormat = "<b>{series.name}:</b> ${point.y:,.0f}<br/>Total: ${point.stackTotal:,.0f}"
      )
    ) %>%
    hc_add_series(
      data = data,
      type = 'column',
      hcaes(x = key, y = indirect),
      name = "Indirect",
      color = '#5791a7',
      tooltip = list(
        useHTML = T, 
        headerFormat = "<span style='color:{point.color}'>\u25CF</span> {point.key}<br>",
        pointFormat = "<b>{series.name}:</b> ${point.y:,.0f}<br/>Total: ${point.stackTotal:,.0f}"
      )
    ) %>%
    hc_add_series(
      data = data,
      type = 'column',
      hcaes(x = key, y = induced),
      name = "Induced",
      color = '#bce3df',
      tooltip = list(
        useHTML = T, 
        headerFormat = "<span style='color:{point.color}'>\u25CF</span> {point.key}<br>",
        pointFormat = "<b>{series.name}:</b> ${point.y:,.0f}<br/>Total: ${point.stackTotal:,.0f}"
      )
    ) %>%
    hc_add_series(
      data = data, 
      hcaes(x = key, y = jobs),
      type = 'scatter',
      name = "Jobs",
      yAxis = 1,
      color = "#e2a583",
      # dataLabels = list(
      #   enabled = TRUE,
      #   inside = TRUE,
      #   padding = 0,
      #   allowOverlap = TRUE,
      #   format = '<h1 style="font-size: 0.9rem; color: #585857;">{y:,.0f} FTE</h1>',
      #   useHTML = TRUE
      # ),
      marker = list(
        symbol = "circle",
        fillColor = "#e2a583",
        radius = 7
      ),
      tooltip = list(
        useHTML = T, 
        headerFormat = "<span style='color:{point.color}'>\u25CF</span> {point.key}<br>",
        pointFormat = "<b>{series.name}:</b> {point.y:,.0f}"
      )
    ) %>%
    hc_tooltip(
      style = list(fontSize = "14px") 
    ) %>%
    hc_plotOptions(
      column = list(
        stacking = "normal",
        groupPadding = 0
      )
    ) %>%
    hc_xAxis(categories = categories) %>%
    hc_yAxis_multiples(
      list(
        lineWidth = 3,
        gridLineWidth = 0,
        reversedStacks=FALSE,
        # stackLabels = list(
        #   enabled = TRUE
        # ),
        title = list(
          text = 'Regional economic output (2021$)'
        ),
        labels = list(
          format = ("${text}")
        )
      ),
      list(
        opposite = TRUE,
        gridLineWidth = 0,
        # max = 1100,
        min = 0,
        endOnTick = FALSE,
        # breaks = list(
        #   from = 0,
        #   to = 705,
        #   breakSize = 100
        # ),
        title = list(
          text = 'Jobs'
        )
      )
    ) %>%
    hc_title(
      text = paste0("<div style='text-align:center;font-weight:normal;
   font-size:16px; font-family: Source Sans Pro;'>Economic Output</div>")
    )
  
  
  
  hc
  
}
