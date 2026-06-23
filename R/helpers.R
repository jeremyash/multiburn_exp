`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

get_burn_issued_time <- function(lat, lon) {
  burn_tz <- lutz::tz_lookup_coords(
    lat = lat,
    lon = lon,
    method = "fast"
  )
  
  lubridate::with_tz(
    Sys.time(),
    tzone = burn_tz
  )
}

get_burn_meta_from_run <- function(run_id) {
  
  serv1_link <- paste0(
    "https://playground-1.airfire.org/bluesky-web-output/",
    run_id,
    "-dispersion"
  )
  
  serv2_link <- paste0(
    "https://playground-2.airfire.org/bluesky-web-output/",
    run_id,
    "-dispersion"
  )
  
  serv_links_ls <- list(
    serv1_link = serv1_link,
    serv2_link = serv2_link
  )
  
  date_info <- purrr::imap_dfr(
    serv_links_ls,
    function(link, server_name) {
      if (!RCurl::url.exists(link)) {
        return(tibble::tibble(
          server = NA_character_,
          end_time = as.POSIXct(NA)
        ))
      }
      
      end_time_val <- lubridate::as_datetime(
        rjson::fromJSON(file = paste0(link, "/output.json"))$runtime[["end"]]
      )
      
      tibble::tibble(
        server = server_name,
        end_time = end_time_val
      )
    }
  )
  
  recent_server <- date_info |>
    dplyr::filter(!is.na(server)) |>
    dplyr::arrange(dplyr::desc(end_time)) |>
    dplyr::slice(1) |>
    dplyr::pull(server)
  
  if (length(recent_server) == 0 || is.na(recent_server)) {
    stop("Could not find BlueSky output for run ID: ", run_id)
  }
  
  results_output_link <- serv_links_ls[[recent_server]]
  
  burn_info <- read.csv(
    paste0(results_output_link, "/output/data/fire_locations.csv")
  )
  
  list(
    lat = burn_info$latitude[1],
    lon = burn_info$longitude[1],
    burn_date = as.Date(lubridate::ymd(burn_info$date_time[1])),
    results_output_link = results_output_link
  )
}

safe_filename <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "-") %>%
    stringr::str_replace_all("(^-|-$)", "")
}

short_forest_name <- function(x) {
  forest_safe <- safe_filename(x) |>
    stringr::str_replace("national-forests", "nf") |>
    stringr::str_replace("national-forest", "nf") |>
    stringr::str_replace("national-grasslands", "ng") |>
    stringr::str_replace("national-grassland", "ng")
  
  tokens <- stringr::str_split(forest_safe, "-", simplify = FALSE)[[1]]
  tokens <- tokens[tokens != ""]
  
  base <- paste0(
    ifelse(tokens %in% c("nf", "ng"), tokens, stringr::str_sub(tokens, 1, 1)),
    collapse = ""
  )
  
  suffix <- digest::digest(forest_safe, algo = "xxhash32") |>
    stringr::str_sub(1, 4)
  
  paste0(base, "-", suffix)
}

get_github_pat <- function() {
  token <- Sys.getenv("GITHUB_PAT")
  
  if (!identical(token, "")) {
    return(token)
  }
  
  token_file <- ".secrets/github_pat.txt"
  
  if (file.exists(token_file)) {
    return(trimws(readLines(token_file, warn = FALSE)[1]))
  }
  
  stop("GitHub token not found.")
}

get_log_sheet_url <- function() {
  
  url <- Sys.getenv("LOG_SHEET_URL")
  
  if (!identical(url, "")) {
    return(url)
  }
  
  url_file <- ".secrets/log_sheet_url.txt"
  
  if (file.exists(url_file)) {
    return(trimws(readLines(url_file, warn = FALSE)[1]))
  }
  
  stop("Log sheet URL not found.")
}