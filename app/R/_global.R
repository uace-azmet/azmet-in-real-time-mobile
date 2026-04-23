# Libraries --------------------


library(azmetr)
library(bsicons)
library(bslib)
library(dplyr)
library(geoloc) # https://github.com/ColinFay/geoloc
library(ggplot2)
library(htmltools)
library(lubridate)
library(shiny)
library(shinyjs)


# Files --------------------


# Functions. Loaded automatically at app start if in `R` folder
#source("./R/fxn_functionName.R", local = TRUE)

# Scripts. Loaded automatically at app start if in `R` folder
#source("./R/scr_scriptName.R", local = TRUE)


# Variables --------------------


azmetStationMetadata <- azmetr::station_info |>
  dplyr::mutate(end_date = NA) |>
  dplyr::filter(meta_station_name != "Test") |>
  dplyr::arrange(meta_station_name)

azmetStationChoices <- 
  dplyr::bind_rows(
    dplyr::tibble(choice = "Select a station...", value = "", lat = NA, lon = NA),
    azmetStationMetadata |>
      dplyr::select(
        choice = meta_station_name,
        value = meta_station_name,
        lat = latitude,
        lon = longitude
      )
  )  

showLatestUpdate <- shiny::reactiveVal(FALSE)
showPageBottomText <- shiny::reactiveVal(FALSE)
showValueBoxLayout <- shiny::reactiveVal(FALSE)


# Other --------------------


shiny::addResourcePath("shinyjs", system.file("srcjs", package = "shinyjs"))
