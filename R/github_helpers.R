upload_report_to_github_pages <- function(
    local_file,
    owner,
    repo,
    branch = "main",
    pages_dir = "docs/sr",
    report_filename,
    commit_message = paste("Add smoke report", report_filename)
) {
  token <- get_github_pat()
  
  if (identical(token, "")) {
    stop("GITHUB_PAT is not set.")
  }
  
  github_path <- file.path(pages_dir, report_filename)
  
  existing <- tryCatch(
    gh::gh(
      "GET /repos/{owner}/{repo}/contents/{path}",
      owner = owner,
      repo = repo,
      path = github_path,
      ref = branch,
      .token = token
    ),
    error = function(e) NULL
  )
  
  sha <- if (!is.null(existing)) existing$sha else NULL
  
  gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = github_path,
    message = commit_message,
    content = base64enc::base64encode(local_file),
    sha = sha,
    branch = branch,
    .token = token
  )
  
  pages_url_dir <- pages_dir %>%
    stringr::str_replace("^docs/?", "") %>%
    stringr::str_replace_all("^/|/$", "")
  
  if (nzchar(pages_url_dir)) {
    paste0("https://", owner, ".github.io/", repo, "/", pages_url_dir, "/", report_filename)
  } else {
    paste0("https://", owner, ".github.io/", repo, "/", report_filename)
  }
}


make_github_pages_url <- function(owner, repo, pages_dir, report_filename) {
  pages_url_dir <- pages_dir %>%
    stringr::str_replace("^docs/?", "") %>%
    stringr::str_replace_all("^/|/$", "")
  
  if (nzchar(pages_url_dir)) {
    paste0("https://", owner, ".github.io/", repo, "/", pages_url_dir, "/", report_filename)
  } else {
    paste0("https://", owner, ".github.io/", repo, "/", report_filename)
  }
}

log_standalone_pb_piedmont_map <- function(
    region,
    forest,
    burn_unit,
    latitude,
    longitude,
    pb_map_url
) {
  tryCatch(
    expr = {
      googledrive::drive_auth(path = ".secrets/smoke-report-logs-7ae50f5a86d1.json")
      googlesheets4::gs4_auth(path = ".secrets/smoke-report-logs-7ae50f5a86d1.json")
      
      log_url <- "https://docs.google.com/spreadsheets/d/1MR94IFlQSbBQ5mbh1nfnyPwT9Eu0GACjquya7rlg1KE/edit?gid=0#gid=0"
      
      model_run_df <- data.frame(
        "Region" = region,
        "Forest" = forest,
        "Burn Unit" = burn_unit,
        "Burn Date" = as.Date(NA),
        "Date Issued" = Sys.Date(),
        "Latitude" = latitude,
        "Longitude" = longitude,
        "Acreage" = NA_real_,
        "PG Link" = NA_character_,
        "Superfog Potential" = NA_character_,
        "Smoke Report Link" = NA_character_,
        "PB Piedmont Map Link" = pb_map_url,
        "Report Type" = "Standalone PB Piedmont Map",
        check.names = FALSE
      )
      
      googlesheets4::sheet_append(log_url, model_run_df, sheet = 1)
    },
    error = function(e) {
      message("FAILED TO WRITE STANDALONE PB PIEDMONT MAP LOG TO GOOGLE SHEETS: ", conditionMessage(e))
      return(NULL)
    }
  )
}

update_index_page <- function(
    owner,
    repo,
    report_filename,
    report_type = c("report", "pb"),
    region,
    forest,
    burn_name,
    burn_date,
    issued_at,
    branch = "main"
) {
  report_type <- match.arg(report_type)
  
  token <- get_github_pat()
  
  if (identical(token, "")) {
    stop("GITHUB_PAT is not set.")
  }
  
  index_path <- "docs/index.html"
  
  existing <- gh::gh(
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = index_path,
    ref = branch,
    .token = token
  )
  
  index_html <- rawToChar(base64enc::base64decode(existing$content))
  sha <- existing$sha
  
  if (report_type == "report") {
    href <- paste0("sr/", report_filename)
    product_label <- "Smoke Report"
    start_marker <- "<!-- ===== BEGIN AUTO-GENERATED SMOKE REPORT LINKS ===== -->"
    empty_pattern <- "<p class=\"empty\">\\s*No reports available yet\\.\\s*</p>"
  } else {
    href <- paste0("pb/", report_filename)
    product_label <- "PB Piedmont"
    start_marker <- "<!-- ===== BEGIN AUTO-GENERATED PB PIEDMONT LINKS ===== -->"
    empty_pattern <- "<p class=\"empty\">\\s*No PB Piedmont maps available yet\\.\\s*</p>"
  }
  
  if (!grepl(start_marker, index_html, fixed = TRUE)) {
    stop("Index start marker not found: ", start_marker)
  }
  
  issued_label <- paste0(
    format(issued_at, "%Y-%m-%d %H:%M"),
    " ",
    format(issued_at, "%Z")
  )
  
  new_entry <- paste0(
    "    <div class='report-entry' ",
    "data-product='", htmltools::htmlEscape(product_label), "' ",
    "data-region='", htmltools::htmlEscape(region), "' ",
    "data-forest='", htmltools::htmlEscape(forest), "' ",
    "data-burn-name='", htmltools::htmlEscape(burn_name), "' ",
    "data-burn-date='", htmltools::htmlEscape(as.character(burn_date)), "' ",
    "data-issued='", htmltools::htmlEscape(format(issued_at, "%Y-%m-%dT%H:%M:%S")), "' ",
    "data-issued-label='", htmltools::htmlEscape(issued_label), "' ",
    "data-url='", htmltools::htmlEscape(href), "'>",
    "</div>\n"
  )
  
  index_html <- gsub(empty_pattern, "", index_html, perl = TRUE)
  
  index_html <- sub(
    start_marker,
    paste0(start_marker, "\n", new_entry),
    index_html,
    fixed = TRUE
  )
  
  gh::gh(
    "PUT /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = repo,
    path = index_path,
    message = paste("Update index with", report_filename),
    content = base64enc::base64encode(charToRaw(index_html)),
    sha = sha,
    branch = branch,
    .token = token
  )
}
