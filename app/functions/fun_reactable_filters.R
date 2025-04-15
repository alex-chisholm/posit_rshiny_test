# reactable filters
fun_reac_selectFilter <- function(tableId, style = "width: 100%; height: 100%;") {
  function(values, name) {
    tags$select(
      # Set to undefined to clear the filter
      onchange = sprintf("
        const value = event.target.value
        Reactable.setFilter('%s', '%s', value === '__ALL__' ? undefined : value)
      ", tableId, name),
      # "All" has a special value to clear the filter, and is the default option
      tags$option(value = "__ALL__", "All"),
      lapply(sort(unique(values)), tags$option),
      "aria-label" = sprintf("Filter %s", name),
      style = style
    )
  }
}

fun_reac_maxRangeFilter <- function(tableId, style = "width: 100%;") {
  function(values, name) {
    values <- na.omit(values)
    oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, name)
    tags$input(
      type = "range",
      min = floor(min(values)),
      max = ceiling(max(values)),
      value = floor(max(values)),
      step = 1,
      oninput = oninput,
      style = style,
      "aria-label" = sprintf("Filter by maximum %s", name)
    )
  }
}

# Min value filter method that handles NaNs
fun_reac_filterMaxValue <- JS("(rows, columnId, filterValue) => {
  return rows.filter(row => {
    const value = row.values[columnId]
    console.log(filterValue)
    return !isNaN(value) && value <= filterValue
  })
}")

fun_reac_filterRangeValue <- JS("(rows, columnId, filterValue) => {
  return rows.filter(row => {
    const value = row.values[columnId]
    const value_num = filterValue.split('-').map(Number)

    if(value_num.length == 1){
      filter_values = [-Infinity, value_num]
    }else{
      filter_values = [Math.min(value_num[0],value_num[1]),Math.max(value_num[0],value_num[1])]
    }

    return !isNaN(value) && value <= filter_values[1] && value >= filter_values[0]
  })
}")

# new filter
# 0, = min 0
# 0,5 = range (inclusive)
# ,5 = max 5
# 0 = equals
fun_reac_filterNumber <- JS("(rows, columnId, filterValue) => {
  return rows.filter(row => {
    const value = row.values[columnId]
    const value_num = filterValue.split(',')

    if(value_num.length == 2){
      if(value_num[0] == ''){
        value_num[0] = -Infinity
      }
      if(value_num[1] == ''){
        value_num[1] = Infinity
      }
    }else if(value_num.length == 1){
       value_num[1] = value_num[0]
    }

    values_ok = value_num.map(Number)

    filter_values = [Math.min(values_ok[0],values_ok[1]),Math.max(values_ok[0],values_ok[1])]

    return !isNaN(value) && value <= filter_values[1] && value >= filter_values[0]
  })
}")