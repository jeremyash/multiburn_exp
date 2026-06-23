append_smoke_app_log <- function(
    sheet_url,
    report_type = NA,
    region = NA,
    forest = NA,
    burn_name = NA,
    burn_date = NA,
    date_issued = NA,
    lat = NA,
    lon = NA,
    acreage = NA,
    run_id = NA,
    superfog_potential = NA,
    day_before_or_of = NA,
    report_url = NA,
    pb_map_url = NA
) {
  pg_link <- if (!is.na(run_id) && nzchar(run_id)) {
    paste0(
      "https://tools.airfire.org/playground/v3.5/dispersionresults.php?scenario_id=",
      run_id
    )
  } else {
    NA
  }
  
  log_row <- tibble::tibble(
    Region = region,
    Forest = forest,
    `Burn Unit` = burn_name,
    `Burn Date` = burn_date,
    `Date Issued` = date_issued,
    Latitude = lat,
    Longitude = lon,
    Acreage = acreage,
    `PG Link` = pg_link,
    `Superfog Potential` = superfog_potential,
    `Day before or of` = day_before_or_of,
    `Smoke Report Link` = report_url,
    `PB Piedmont Map Link` = pb_map_url,
    `Report Type` = report_type
  )
  
  google_key_file <- ".secrets/smoke-report-logs-7ae50f5a86d1.json"
  
  if (file.exists(google_key_file)) {
    googlesheets4::gs4_auth(path = google_key_file)
  } else {
    stop("Google service account key file not found: ", google_key_file)
  }
  
  googlesheets4::sheet_append(
    ss = sheet_url,
    data = log_row
  )
}

safe_append_smoke_app_log <- function(..., context = "Smoke app log") {
  
  tryCatch(
    {
      append_smoke_app_log(...)
      message(context, " succeeded")
      TRUE
    },
    error = function(e) {
      message(context, " failed: ", conditionMessage(e))
      FALSE
    }
  )
  
}