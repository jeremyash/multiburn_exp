build_report_link_ui <- function(url) {
  
  if (is.null(url)) {
    return(NULL)
  }
  
  tags$div(
    class = "success-link-card",
    
    tags$div(
      class = "success-link-title",
      "Report available online"
    ),
    
    tags$div(
      class = "success-link-note",
      "Note: the link may take 20-60 seconds to become available."
    ),
    
    tags$div(
      class = "success-link-row",
      
      tags$a(
        href = url,
        target = "_blank",
        url
      ),
      
      tags$button(
        type = "button",
        class = "btn btn-success",
        onclick = sprintf(
          "navigator.clipboard.writeText('%s'); this.innerText='Copied!'; setTimeout(() => this.innerText='Copy URL', 1500);",
          url
        ),
        "Copy URL"
      )
    )
  )
}

build_pb_link_ui <- function(url) {
  
  if (is.null(url)) {
    return(NULL)
  }
  
  tags$div(
    class = "success-link-card",
    
    tags$div(
      class = "success-link-title",
      "PB Piedmont map available online"
    ),
    
    tags$div(
      class = "success-link-note",
      "Note: the link may take 20-60 seconds to become available."
    ),
    
    tags$div(
      class = "success-link-row",
      
      tags$a(
        href = url,
        target = "_blank",
        url
      ),
      
      tags$button(
        type = "button",
        class = "btn btn-success",
        onclick = sprintf(
          "navigator.clipboard.writeText('%s'); this.innerText='Copied!'; setTimeout(() => this.innerText='Copy URL', 1500);",
          url
        ),
        "Copy URL"
      )
    )
  )
}

build_region_08_options_ui <- function(input) {
  
  if (is.null(input$REGION) || input$REGION != "08") {
    return(NULL)
  }
  
  tagList(
    tags$div(class = "app-section-title", "Region 8 options"),
    
    selectInput(
      "FORECAST_AQI_SELECT",
      "Forecasted AQI downwind of ignition",
      choices = AQI_CHOICES,
      selected = isolate(input$FORECAST_AQI_SELECT %||% "Good")
    ),
    
    selectInput(
      "SUPERFOG_SCREEN_SELECT",
      "Potential for superfog formation?",
      choices = YES_NO_CHOICES,
      selected = isolate(input$SUPERFOG_SCREEN_SELECT %||% "No")
    )
  )
}


build_pb_hourly_upload_ui <- function(input) {
  
  req(input$REGION)
  
  if (
    input$REGION == "08" &&
    !is.null(input$SUPERFOG_SCREEN_SELECT) &&
    input$SUPERFOG_SCREEN_SELECT == "Yes"
  ) {
    fileInput(
      "PB_HOURLY_ZIP",
      "Upload PB Piedmont hourly_output.zip here (optional)",
      accept = c(".zip", "application/zip", "application/x-zip-compressed")
    )
  } else {
    NULL
  }
}

build_forest_selector_ui <- function(region, nfs) {
  
  req(nzchar(region))
  
  forest_choices <- nfs %>%
    dplyr::filter(region == !!region) %>%
    dplyr::pull(forests)
  
  selectizeInput(
    "FOREST",
    "Forest:",
    choices = c("Select a forest" = "", forest_choices),
    selected = "",
    options = list(
      placeholder = "Select a forest"
    )
  )
}