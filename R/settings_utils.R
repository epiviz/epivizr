.settings_as_df <- function(chart_settings) {
  ids <- sapply(chart_settings, function(x) x$id)
  labels <- sapply(chart_settings, function(x) x$label)
  values <- sapply(chart_settings, function(x) x$defaultValue)
  poss_values <- sapply(chart_settings, function(x) {
    paste0(x$possibleValues, collapse=",")
  })
  types <- sapply(chart_settings, function(x) x$type)
  out <- data.frame(id=ids,
                    label=labels,
                    default_value=values,
                    possible_values=poss_values,
                    type=types,
                    stringsAsFactors=FALSE)
  out
}