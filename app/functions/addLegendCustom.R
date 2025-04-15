addLegendCustom <- function(map, colors, border_colors, border_px,
                            border_left, border_right, border_bottom, labels, 
                            height, width, margin_top, margin_left, radius, 
                            border_dash, opacity = 1, background, group = NULL, layer_id = "custom_legend"){
  colorAdditions <- paste0(colors,
                           "; border-radius: ", radius, "%; border: ",border_px, "px ", border_dash," ", border_colors,
                           "; border-left:", border_left,
                           "; border-right:", border_right,
                           "; border-bottom:", border_bottom,
                           "; width:", width, "px; height:", height, "px;", "background: ", background,
                           "; margin-top:", margin_top, "px; margin-left:", margin_left, "px;
                           margin-right:", margin_left, "px;")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           15, "px; line-height: ", 15, "px; padding-top:0px;margin-top:0px'>", 
                           labels, "</div>")
  
  return(addLegend(map,
                   group = group,
                   layerId = layer_id,
                   colors = colorAdditions, 
                   labels = labelAdditions, 
                   opacity = opacity, 
                   position = "bottomleft"))
}