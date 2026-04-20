# Mobile-first Shiny app displaying tabular and graphical summaries of the latest 15-minute data from stations across the network

# Add code for the following
# 
# 'azmet-shiny-template.html': <!-- Google tag (gtag.js) -->


# # For now just get data for all sites on app load
# data <- fxn_az15min()

# station_choices <- azmetr::station_info |>
#   select(
#     choice = meta_station_name,
#     value = meta_station_id,
#     lat = latitude,
#     lon = longitude
#   ) |>
#   filter(choice != "Test") |>
#   dplyr::arrange(choice)


# UI --------------------


ui <- 
  htmltools::htmlTemplate(
    
    filename = "azmet-shiny-template.html",
    
    azmetInRealTimeMobile = 
      
      bslib::page_fillable(
        title = NULL,
        fillable = FALSE,
        fillable_mobile = FALSE,
        theme = theme, # `scr##_theme.R`
        
        # htmltools::tags$head(htmltools::includeHTML("www/pwa/pwa.html")),
        
        htmltools::p(
          class = "azmet-station-title",
          
          htmltools::HTML("&nbsp;AZMet Station&nbsp;"),
          bslib::tooltip(
            bsicons::bs_icon("info-circle"),
            "Select an AZMet station or tap on the location pin to choose the nearest one.",
            id = "azmetStationInfo",
            placement = "right"
          )
        ),
        
        htmltools::div(
          class = "azmet-station-selection-div",
          style = "display: flex; align-items: center; column-gap: 1.5rem;", # Flexbox styling
          
          shiny::selectInput(
            inputId = "azmetStation", 
            label = NULL,
            choices = c("Select a station..." = "", azmetStationChoices),
            selected = ""
          ),
          
          shiny::actionButton(
            inputId = "locatorPin",
            label = bsicons::bs_icon("geo-alt", class = "locator-pin"),
            class = "btn btn-default btn-blue geoloc"
          )
        ),
        
        shiny::uiOutput(outputId = "latestUpdate"),
        shiny::uiOutput(outputId = "valueBoxLayout"),
        shiny::htmlOutput(outputId = "pageBottomText")
      )
  ) # htmltools::htmlTemplate()


# Server --------------------


server <- function(input, output, session) {
  
  shinyjs::useShinyjs(html = TRUE)
  shinyjs::hideElement(id = "latestUpdate")
  shinyjs::hideElement(id = "pageBottomText")
  shinyjs::hideElement(id = "valueBoxLayout")
  
  # # Set a default station
  # # TODO: use cookies for this
  # station <- reactiveVal("az01")
  # 
  # # Create modal to contain picker with location button
  # observeEvent(input$locatorPin, {
  #   showModal(
  #     # TODO: it would be nice if making a selection closed the modal
  #     modalDialog(
  #       location_select_ui(
  #         "loc_module",
  #         "Select a station:",
  #         station_choices,
  #         selected = station()
  #       ),
  #       footer = NULL,
  #       easyClose = TRUE
  #     )
  #   )
  # })
  # 
  # # Get results of location selection
  # station_id_choice <- location_select_server(
  #   "loc_module",
  #   station_choices
  # )
  # 
  # # When a choice is made, update the default station
  # observeEvent(station_id_choice(), {
  #   station(isolate(station_id_choice()))
  # })
  # 
  # # Print the station name for the modal button.
  # output$selected_station <- renderText({
  #   station_choices |> filter(value == station()) |> pull(choice)
  # })
  # 
  # station_data <- reactive({
  #   data |> filter(meta_station_id == station())
  # })
  
  
  # Observables -----
  
  shiny::observeEvent(az15min(), {
    shinyjs::showElement(id = "latestUpdate")
    shinyjs::showElement(id = "pageBottomText")
    shinyjs::showElement(id = "valueBoxLayout")
    
    showLatestUpdate(TRUE)
    showPageBottomText(TRUE)
    showValueBoxLayout(TRUE)
  })
  
  
  # Reactives -----
  
  az15min <-
    shiny::reactive({
      fxn_az15min(input$azmetStation)
    }) %>%
    shiny::bindEvent(
      input$azmetStation,
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
  
  valueBoxLayout <- 
    shiny::eventReactive(input$azmetStation, {
      fxn_valueBoxLayout(inData = az15min())
    })
  
  
  # Outputs -----
  
  output$latestUpdate <- 
    shiny::renderUI({
      fxn_latestUpdate(inData = az15min())
    })
  
  output$pageBottomText <- 
    shiny::renderUI({
      shiny::req(showPageBottomText())
      fxn_pageBottomText()
    })
  
  output$vbChart_P <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = precip_total_in)
    })
  
  output$vbChart_RH <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = relative_humidity)
    })
  
  output$vbChart_SR <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = sol_rad_Wm2)
    })
  
  output$vbChart_T <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = temp_airF)
    })
  
  output$vbChart_Tsoil10cm <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = temp_soil_10cmF)
    })
  
  output$vbChart_Tsoil50cm <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = temp_soil_50cmF)
    })
  
  output$vbChart_WS <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = wind_spd_mph)
    })
  
  output$vbChart_WS2min <- 
    renderPlot({
      fxn_valueBoxChart(inData = az15min(), inVariable = wind_2min_spd_mean_mph)
    })
  
  output$valueBoxLayout <- 
    shiny::renderUI(
      bslib::layout_column_wrap(
        !!!valueBoxLayout(),
        #class = ,
        fill = TRUE,
        fillable = TRUE,
        fixed_width = FALSE,
        #gap = "200px",
        #height = "200px",
        heights_equal = c("all", "row"),
        height_mobile = NULL,
        max_height = NULL,
        min_height = NULL,
        width = 1
      )
    )
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
