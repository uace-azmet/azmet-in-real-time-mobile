#' `fxn_valueBoxChart.R` - Build chart for an individual value box
#' 
#' @param inData - AZMet 15-minute data for user-selected station from `fxn_az15min.R`
#' @param inVariable - AZMet variable to display
#' @return `valueBoxChart` - Chart for an individual value box


fxn_valueBoxChart <- function(inData, inVariable) {
  
  dataPoint <- inData %>% 
    dplyr::filter(
      lubridate::as_datetime(datetime) == max(lubridate::as_datetime(datetime))
    )
  
  valueBoxChart <- 
    ggplot2::ggplot(
      data = inData, 
      mapping = ggplot2::aes(
        x = lubridate::as_datetime(datetime), 
        y = {{ inVariable }}
      )
    ) + 
    geom_line(color = "#606060") + 
    geom_point(data = dataPoint, color = "#191919") +
    theme_void()
  
  return(valueBoxChart)
}
