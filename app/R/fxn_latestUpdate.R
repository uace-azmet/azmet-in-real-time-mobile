#' `fxn_latestUpdate.R` - Build text for Latest Update section
#' 
#' @param inData - AZMet 15-minute data for user-selected station from `fxn_az15min.R`
#' @return `latestUpdate` - Text for Latest Update section


fxn_latestUpdate <- function(inData) {
  
  latestUpdate <- 
    htmltools::p(
      class = "latest-update",
      
      htmltools::HTML(
        paste0(
          "<strong>Latest Update</strong> ",
          inData |> dplyr::filter(datetime == max(datetime)) |> dplyr::pull(datetime)
        )
      )
    )
  
  return(latestUpdate)
}
