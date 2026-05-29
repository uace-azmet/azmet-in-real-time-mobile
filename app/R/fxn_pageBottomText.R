#' `fxn_pageBottomText.R` - Build supporting text for page
#' 
#' @return `pageBottomText` - Supporting text for page


fxn_pageBottomText <- function() {
  
  
  # Define inputs -----
  
  apiURL <- 
    a(
      "api.azmet.arizona.edu", 
      href="https://api.azmet.arizona.edu/v1/observations/15min",
      target="_blank"
    )
  
  azmetrURL <- 
    a(
      "azmetr", 
      href="https://uace-azmet.github.io/azmetr/",
      target="_blank"
    )
  
  heatIndexEquationURL <- 
    a(
      "equation", 
      href="https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml",
      target="_blank"
    )
  
  todayDate <- gsub(" 0", " ", format(lubridate::today(), "%B %d, %Y"))
  
  todayYear <- lubridate::year(lubridate::today())
  
  webpageAZMet <- 
    a(
      "AZMet website", 
      href="https://azmet.arizona.edu/", 
      target="_blank"
    )
  
  webpageCode <- 
    a(
      "GitHub page", 
      href="https://github.com/uace-azmet/azmet-in-real-time-mobile", 
      target="_blank"
    )
  
  webpageDataVariables <- 
    a(
      "data variables", 
      href="https://azmet.arizona.edu/about/data-variables", 
      target="_blank"
    )
  
  webpageNetworkMap <- 
    a(
      "station locations", 
      href="https://azmet.arizona.edu/about/network-map", 
      target="_blank"
    )
  
  webpageStationMetadata <- 
    a(
      "station metadata", 
      href="https://azmet.arizona.edu/about/station-metadata", 
      target="_blank"
    )
  
  # Build text -----
  
  pageBottomText <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "Sparkline charts show data over the past 12 hours. Heat index is the apparent, or 'feels like', temperature based on air temperature, relative humidity, and the National Weather Service ", heatIndexEquationURL, ". Maximum values, minimum values, and daily totals are since midnight local time. Values of 'NA' denote no data.",
          htmltools::br(), htmltools::br(),
          "AZMet 15-minute data are from ", apiURL, " and accessed using the ", azmetrURL, " R package. Values are based on provisional data. More information about ", webpageDataVariables, ", ", webpageNetworkMap, ", and ", webpageStationMetadata, " is available on the ", webpageAZMet, ". Users of AZMet data and related information assume all risks of its use.",
          htmltools::br(), htmltools::br(),
          "To cite the above AZMet data, please use: 'Arizona Meteorological Network (", todayYear, ") Arizona Meteorological Network (AZMet) Data. https:://azmet.arizona.edu. Accessed ", todayDate, "', along with 'Arizona Meteorological Network (", todayYear, ") AZMet in Real-time: Mobile. https://viz.datascience.arizona.edu/azmet/azmet-in-real-time-mobile. Accessed ", todayDate, "'.",
          htmltools::br(), htmltools::br(),
          "For information on how this webpage is put together, please visit the ", webpageCode, " for this tool."
        )
      ),
      
      class = "page-bottom-text"
    )
  
  return(pageBottomText)
}
