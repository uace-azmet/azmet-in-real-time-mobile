# Mobile-first Shiny app displaying tabular and graphical summaries of the latest 15-minute data from stations across the network


# PROCESS FOR PWA -----

# Copy icons to `app/www/images/`
# Copy `pwa-service-worker.js` to `app/www/`, edit
# Copy `pwa.html` to `app/www/`, edit
# Copy `manifest.webmanifest` to `app/www/`
# Add `tags$head(includeHTML("www/pwa.html"))` to `app.R`


# UI --------------------


ui <-
  htmltools::htmlTemplate(
    filename = "azmet-shiny-template.html",

    azmetInRealTimeMobile = bslib::page_fillable(
      title = NULL,
      fillable = FALSE,
      fillable_mobile = FALSE,
      theme = theme, # `scr##_theme.R`

      htmltools::tags$head(htmltools::includeHTML("www/pwa.html")),

      htmltools::p(
        class = "azmet-station-title",

        htmltools::HTML("&nbsp;AZMet Station&nbsp;"),
        bslib::tooltip(
          bsicons::bs_icon("info-circle"),
          "Select an AZMet station or tap on the location pin to find the nearest one.",
          id = "azmetStationInfo",
          placement = "right"
        )
      ),

      htmltools::div(
        location_select_ui(
          id = "azmetStation",
          label = NULL,
          locations_df = azmetStationChoices
        )
      ),

      shiny::uiOutput(outputId = "latestUpdate"),
      shiny::uiOutput(outputId = "valueBoxLayout"),
      shiny::htmlOutput(outputId = "pageBottomText")
    )
  ) |> 
  cookies::add_cookie_handlers()


# Server --------------------


server <- function(input, output, session) {
  
  shinyjs::useShinyjs(html = TRUE)
  
  shinyjs::hideElement(id = "latestUpdate")
  shinyjs::hideElement(id = "pageBottomText")
  shinyjs::hideElement(id = "valueBoxLayout")


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

  shiny::observeEvent(azmetStation(), {
    try(
      cookies::set_cookie(
        cookie_name = "station_choice",
        cookie_value = azmetStation()
      )
    )
  })

  azmetStation <- 
    location_select_server(
      id = "azmetStation",
      locations_df = azmetStationChoices,
      selected = isolate(cookies::get_cookie("station_choice")) # infinite loop of updates w/o isolate()
    )
  
  az15min <-
    shiny::reactive({
      fxn_az15min(azmetStation())
    }) %>%
    shiny::bindEvent(
      azmetStation(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

  valueBoxLayout <-
    shiny::eventReactive(azmetStation(), {
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
        width = "320px"
      )
    )
}


# Run --------------------


shiny::shinyApp(ui = ui, server = server)
