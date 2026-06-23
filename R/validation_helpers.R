# -------------------------------------------------
# REPORT VALIDATION HELPERS
# -------------------------------------------------

get_missing_report_fields <- function(input) {
  missing <- character()
  
  if (!nzchar(input$REGION %||% "")) {
    missing <- c(missing, "USFS Region")
  }
  
  if (!nzchar(input$FOREST %||% "")) {
    missing <- c(missing, "Forest")
  }
  
  if (!nzchar(input$BURN_NAME %||% "")) {
    missing <- c(missing, "Burn unit name")
  }
  
  if (!nzchar(input$RUN_ID %||% "")) {
    missing <- c(missing, "BlueSky Playground Run ID")
  }
  
  if (identical(input$REGION, "08")) {
    if (!nzchar(input$FORECAST_AQI_SELECT %||% "")) {
      missing <- c(missing, "Forecasted AQI")
    }
    
    if (!nzchar(input$SUPERFOG_SCREEN_SELECT %||% "")) {
      missing <- c(missing, "Superfog selection")
    }
  }
  
  missing
}


is_report_ready <- function(input) {
  length(get_missing_report_fields(input)) == 0
}


report_required_message_ui <- function(input) {
  missing <- get_missing_report_fields(input)
  
  if (length(missing) == 0) {
    return(NULL)
  }
  
  tags$div(
    class = "required-fields-message",
    tags$strong("Required before downloading: "),
    paste(missing, collapse = ", ")
  )
}