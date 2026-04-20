#' `fxn_deg_to_dir.R`: convert wind vector degrees to cardinal direction
#' @param: `valueIn` - value of wind vector direction in degrees
#' @return: `valueOut` - value of wind vector direction as cardinal direction


fxn_deg_to_dir <- function(valueIn) {
  
  valueOut <- 
    dplyr::case_when(
      valueIn <= 11.25 ~ "N",
      valueIn > 11.25 & valueIn <= 33.75 ~ "NNE",
      valueIn > 33.75 & valueIn <= 56.25 ~ "NE",
      valueIn > 56.25 & valueIn <= 78.75 ~ "ENE",
      valueIn > 78.75 & valueIn <= 101.25 ~ "E",
      valueIn > 101.25 & valueIn <= 123.75 ~ "ESE",
      valueIn > 123.75 & valueIn <= 146.25 ~ "SE",
      valueIn > 146.25 & valueIn <= 168.75 ~ "SSE",
      valueIn > 168.75 & valueIn <= 191.25 ~ "S",
      valueIn > 191.25 & valueIn <= 213.75 ~ "SSW",
      valueIn > 213.75 & valueIn <= 236.25 ~ "SW",
      valueIn > 236.25 & valueIn <= 258.75 ~ "WSW",
      valueIn > 258.75 & valueIn <= 281.25 ~ "W",
      valueIn > 281.25 & valueIn <= 303.75 ~ "WNW",
      valueIn > 303.75 & valueIn <= 326.25 ~ "NW",
      valueIn > 326.25 & valueIn <= 348.75 ~ "NNW",
      valueIn > 348.75 ~ "N"
    )
  
  return(valueOut)
}
