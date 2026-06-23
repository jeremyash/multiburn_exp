# app_multi_burn.R
# Standalone Shiny app for Multi-burn Exposure reports

require(shiny)
require(tidyverse)
require(rmarkdown)
require(lubridate)
require(shinyjs)
require(here)
require(RCurl)
require(rjson)
require(curl)
require(jsonlite)
require(googlesheets4)
require(gh)
require(fs)

source("R/constants.R")
source("R/helpers.R")
source("R/ui_helpers.R")
source("R/validation_helpers.R")
source("R/filename_helpers.R")
source("R/github_helpers.R")
source("R/log_helpers.R")
source("R/multi_burn_helpers.R")

LOG_SHEET_URL <- get_log_sheet_url()

multi_burn_run_ui <- function(i) {
  tags$div(
    class = "multi-run-row",
    fluidRow(
      column(5, textInput(paste0("burn_name_", i), paste0("Burn/unit name ", i, " (optional)"))),
      column(7, textInput(paste0("run_id_", i), paste0("BlueSky run ID ", i)))
    )
  )
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$title("Multi-burn Exposure"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css?v=2"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon_512x512_rounded.png?v=120"),
    tags$meta(name = "theme-color", content = "#032B5B"),
    tags$style(HTML(".multi-run-row{border-top:1px solid #e5e5e5;padding-top:8px;margin-top:8px;}"))
  ),

  tags$div(
    class = "app-title-banner",
    tags$img(src = "favicon_512x512_rounded.png", alt = "Smoke Report Icon"),
    tags$div(
      class = "app-title-text",
      tags$div(class = "app-title-main", "Multi-burn Exposure Report Generator"),
      tags$div(class = "app-title-sub", "Combine BlueSky PM2.5 PNG outputs from multiple burns")
    )
  ),

  fluidRow(
    class = "app-layout-row",
    column(
      width = 5,
      tags$div(
        class = "sidebar-card app-scroll-sidebar",
        tags$div(class = "app-section-title", "Project information"),
        textInput("PROJECT_NAME", "Project name"),
        dateInput("BURN_DATE", "Burn date", value = Sys.Date()),

        tags$div(class = "app-section-title", "BlueSky runs"),
        helpText("Enter up to 10 BlueSky run IDs. Burn/unit names are optional and are used only for map labels and the included-run table."),
        lapply(1:10, multi_burn_run_ui),

        tags$div(class = "app-section-title", "Download"),
        uiOutput("required_msg"),
        downloadButton("multi_report", "Download Multi-burn Exposure Report")
      )
    ),

    column(
      width = 7,
      tags$div(
        class = "main-card app-fixed-main",
        h2(textOutput("project_title")),
        uiOutput("report_link_ui"),
        hr(),
        h4("Included runs"),
        tableOutput("runs_preview")
      )
    )
  )
)

server <- function(input, output, session) {
  report_link <- reactiveVal(NULL)

  runs_df <- reactive({
    tibble(
      burn_name = purrr::map_chr(1:10, ~ input[[paste0("burn_name_", .x)]] %||% ""),
      run_id = purrr::map_chr(1:10, ~ input[[paste0("run_id_", .x)]] %||% "")
    ) |>
      mutate(
        burn_name = trimws(burn_name),
        run_id = trimws(run_id)
      ) |>
      filter(nzchar(run_id))
  })

  output$project_title <- renderText({
    x <- trimws(input$PROJECT_NAME %||% "")
    if (nzchar(x)) x else "Multi-burn Exposure"
  })

  output$runs_preview <- renderTable({
    runs_df() |>
      mutate(`Burn/unit name` = if_else(nzchar(burn_name), burn_name, "—"), `Run ID` = run_id) |>
      select(`Burn/unit name`, `Run ID`)
  })

  output$required_msg <- renderUI({
    msgs <- character(0)
    if (!nzchar(trimws(input$PROJECT_NAME %||% ""))) msgs <- c(msgs, "Project name is required.")
    if (nrow(runs_df()) < 1) msgs <- c(msgs, "At least one BlueSky run ID is required.")

    if (length(msgs) == 0) return(NULL)
    tags$div(class = "alert alert-warning", HTML(paste(msgs, collapse = "<br>")))
  })

  output$multi_report <- downloadHandler(
    filename = function() {
      make_multi_burn_filename(input$PROJECT_NAME, input$BURN_DATE, Sys.time())
    },
    content = function(file) {
      validate(
        need(nzchar(trimws(input$PROJECT_NAME %||% "")), "Project name is required."),
        need(nrow(runs_df()) >= 1, "At least one BlueSky run ID is required.")
      )

      withProgress(message = "Generating multi-burn exposure report...", value = 0, {
        issued_at <- Sys.time()
        report_filename <- make_multi_burn_filename(input$PROJECT_NAME, input$BURN_DATE, issued_at)
        rendered_file <- file.path(tempdir(), report_filename)
        log_row_file <- file.path(tempdir(), paste0(tools::file_path_sans_ext(report_filename), "_log_row.rds"))

        incProgress(0.10, detail = "Preparing report")

        report_url <- make_github_pages_url(
          owner = APP_OWNER,
          repo = APP_REPO,
          pages_dir = "mb",
          report_filename = report_filename
        )

        params_ls <- list(
          PROJECT_NAME = trimws(input$PROJECT_NAME),
          BURN_DATE = as.Date(input$BURN_DATE),
          RUNS = runs_df(),
          REPORT_URL = report_url,
          LOG_ROW_FILE = log_row_file
        )

        incProgress(0.20, detail = "Reading BlueSky outputs")
        incProgress(0.20, detail = "Combining daily and hourly PM2.5 images")
        incProgress(0.20, detail = "Building maps")

        rmarkdown::render(
          input = here::here("templates", "multi_burn_exposure_template.Rmd"),
          output_file = rendered_file,
          params = params_ls,
          envir = new.env(parent = globalenv())
        )

        incProgress(0.15, detail = "Uploading report")

        github_success <- tryCatch({
          uploaded_url <- upload_report_to_github_pages(
            local_file = rendered_file,
            owner = APP_OWNER,
            repo = APP_REPO,
            branch = APP_BRANCH,
            pages_dir = "mb",
            report_filename = report_filename,
            commit_message = paste(
              trimws(input$PROJECT_NAME),
              "|",
              format(as.Date(input$BURN_DATE), "%Y-%m-%d"),
              "| Multi-burn Exposure"
            )
          )

          report_link(uploaded_url)

          update_index_page(
            owner = APP_OWNER,
            repo = APP_REPO,
            report_filename = report_filename,
            report_type = "multi-burn-exposure",
            region = NA,
            forest = NA,
            burn_name = trimws(input$PROJECT_NAME),
            burn_date = as.Date(input$BURN_DATE),
            issued_at = issued_at,
            branch = APP_BRANCH
          )

          if (file.exists(log_row_file)) {
            log_row <- readRDS(log_row_file)
            safe_append_smoke_app_log(
              sheet_url = LOG_SHEET_URL,
              report_type = log_row$report_type,
              region = log_row$region,
              forest = log_row$forest,
              burn_name = log_row$burn_name,
              burn_date = log_row$burn_date,
              date_issued = log_row$date_issued,
              lat = log_row$lat,
              lon = log_row$lon,
              acreage = log_row$acreage,
              run_id = log_row$run_id,
              superfog_potential = log_row$superfog_potential,
              report_url = uploaded_url,
              pb_map_url = log_row$pb_map_url,
              context = "Multi-burn Exposure log"
            )
          }

          TRUE
        }, error = function(e) {
          message("GitHub upload, index update, or logging failed: ", conditionMessage(e))
          report_link(NULL)
          FALSE
        })

        incProgress(0.15, detail = "Finalizing download")
        file.copy(rendered_file, file, overwrite = TRUE)
      })
    }
  )

  output$report_link_ui <- renderUI({
    if (is.null(report_link())) return(tags$p("Generated report links will appear here after upload."))
    tags$p(tags$a(href = report_link(), target = "_blank", "Open uploaded Multi-burn Exposure report"))
  })
}

shinyApp(ui = ui, server = server)
