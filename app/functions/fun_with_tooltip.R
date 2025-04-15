fun_with_tooltip <- function(value, tooltip, placement = "top") {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help;",
      tippy::tippy(value, tooltip, placement = placement, duration = 0))
}