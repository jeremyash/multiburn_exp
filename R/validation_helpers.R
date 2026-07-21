get_missing_multi_burn_fields <- function(input, runs_df = NULL) {
  missing <- character()
  
  if (!nzchar(input$PROJECT_NAME %||% "")) {
    missing <- c(missing, "Project Name")
  }
  
  if (is.null(input$BURN_DATE) || is.na(input$BURN_DATE)) {
    missing <- c(missing, "Burn Date")
  }
  
  if (is.null(runs_df) || nrow(runs_df) == 0) {
    missing <- c(missing, "At least one BlueSky Run ID")
  } else {
    valid_runs <- runs_df |>
      dplyr::filter(nzchar(run_id %||% ""))
    
    if (nrow(valid_runs) == 0) {
      missing <- c(missing, "At least one BlueSky Run ID")
    }
  }
  
  missing
}


is_multi_burn_ready <- function(input, runs_df = NULL) {
  length(get_missing_multi_burn_fields(input, runs_df)) == 0
}


multi_burn_required_message_ui <- function(input, runs_df = NULL) {
  missing <- get_missing_multi_burn_fields(input, runs_df)
  
  if (length(missing) == 0) {
    return(NULL)
  }
  
  tags$div(
    class = "required-fields-message",
    tags$strong("Required before generating report: "),
    paste(missing, collapse = ", ")
  )
}