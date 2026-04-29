#' `selectInput()` with geolocation button (UI module)
#' 
#' A select drop-down input with a location button next to it to choose the 
#' input closest to the user (if they have location services allowed).
#'
#' @param id
#' @param label passed to [shiny::selectInput()].
#' @param locations_df a data frame with the columns `choice`, `value`, `lat`, and `lon`.
#' @param ... additional, arguments passed to [shiny::selectInput()] (although,
#'   for `selected`, use the argument in `location_select_server`).
location_select_ui <- function(
  id,
  label = "Select Location:",
  locations_df,
  selected = NULL,
  ...
) {
  
  ns <- shiny::NS(id)

  # Create named vector for choices
  # The value will be "lat,lon" and the name will be the choice
  choices <- setNames(
    locations_df$value,
    locations_df$choice
  )

  htmltools::div(
    # class = "d-flex flex-row justify-content-center align-items-center align-middle",
    class = "azmet-station-selection-div",
    style = "display: flex; align-items: center; column-gap: 1.0rem;", # Flexbox styling
    
    htmltools::div(
      # class = "p-1",
      shiny::selectInput(
        ns("select"),
        label,
        choices = choices,
        selected = selected,
        ...
      )
    ),
    htmltools::div(
      geoloc::button_geoloc(
        ns("loc"),
        bsicons::bs_icon("geo-alt", class = "locator-pin"),
        class = "btn btn-default btn-blue geoloc"
      )
    )
  )
}


#' `inputSelect()` with geolocation button (server module)
#'
#' @param id same ID as provided to `location_select_ui()`.
#' @param locations_df same data frame as provided to `locations_select_ui()`.
#' @param selected passed to [shiny::selectInput()]; one of the `value` options
#'   in `locations_df`.
location_select_server <- function(id, locations_df, selected = NULL) {
  moduleServer(id, function(input, output, session) {
    updateSelectInput(
      session,
      "select",
      selected = selected
    )
    # Update choice when location is received
    shiny::observe({
      shiny::req(input$loc_lat, input$loc_lon)

      user_lat <- as.numeric(input$loc_lat)
      user_lon <- as.numeric(input$loc_lon)

      # Find nearest location using Haversine distance
      distances <- sapply(1:nrow(locations_df), function(i) {
        lat <- locations_df$lat[i]
        lon <- locations_df$lon[i]

        # Haversine formula
        dlat <- (lat - user_lat) * pi / 180
        dlon <- (lon - user_lon) * pi / 180
        a <- sin(dlat / 2)^2 +
          cos(user_lat * pi / 180) * cos(lat * pi / 180) * sin(dlon / 2)^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))
        6371 * c # Distance in km
      })

      nearest_idx <- which.min(distances)
      nearest_location <- locations_df[nearest_idx, ]

      # Update the input
      shiny::updateSelectInput(
        session,
        "select",
        selected = nearest_location$value
      )
    }) |>
      # Run when the location is updated (not just when the button is pressed)
      shiny::bindEvent(
        input$loc_lat,
        input$loc_lon,
        input$loc
      )

    # Return the selected value as a reactive
    reactive(input$select)
  })
}
