# R/multi_burn_helpers.R
# Multi-burn Exposure-specific BlueSky image helpers


# -------------------------------------------------
# BLUESKY RUN INFORMATION
# -------------------------------------------------

read_bluesky_run_info <- function(
    run_id,
    burn_name = NA_character_
) {
  
  meta <- get_burn_meta_from_run(run_id)
  
  results_link <- meta$results_output_link
  
  fire_file <- paste0(
    results_link,
    "/output/data/fire_locations.csv"
  )
  
  grid_file <- paste0(
    results_link,
    "/output/grid_info.json"
  )
  
  fire <- read.csv(
    fire_file,
    stringsAsFactors = FALSE
  )
  
  grid <- jsonlite::fromJSON(grid_file)
  
  if (nrow(fire) == 0) {
    stop(
      "No fire location information found for BlueSky run ID: ",
      run_id
    )
  }
  
  fire_one <- fire[1, , drop = FALSE]
  
  burn_tz <- tryCatch(
    lutz::tz_lookup_coords(
      lat = fire_one$latitude,
      lon = fire_one$longitude,
      method = "fast"
    ),
    error = function(e) "UTC"
  )
  
  if (
    is.na(burn_tz) ||
    !nzchar(burn_tz)
  ) {
    burn_tz <- "UTC"
  }
  
  burn_name_clean <- trimws(
    as.character(burn_name %||% "")
  )
  
  burn_label <- if (nzchar(burn_name_clean)) {
    paste0(
      burn_name_clean,
      " — ",
      run_id
    )
  } else {
    as.character(run_id)
  }
  
  tibble::tibble(
    run_id = as.character(run_id),
    burn_name = if (nzchar(burn_name_clean)) {
      burn_name_clean
    } else {
      NA_character_
    },
    label = burn_label,
    
    results_link = results_link,
    
    lat = as.numeric(fire_one$latitude),
    lon = as.numeric(fire_one$longitude),
    
    utc_offset = as.character(
      fire_one$utc_offset
    ),
    
    date_time = as.character(
      fire_one$date_time
    ),
    
    burn_date = as.Date(
      lubridate::ymd(
        fire_one$date_time
      )
    ),
    
    acres = as.numeric(
      fire_one$area
    ),
    
    tz = burn_tz,
    
    xmin = as.numeric(grid$bbox[1]),
    ymin = as.numeric(grid$bbox[2]),
    xmax = as.numeric(grid$bbox[3]),
    ymax = as.numeric(grid$bbox[4])
  )
}


# -------------------------------------------------
# ORIGINAL BLUESKY PM2.5 COLORS
# -------------------------------------------------

# These colors are used ONLY to decode the
# existing rendered BlueSky PNGs.

pm25_cols <- c(
  "#CCE5FFB2",  # 1–9
  "#99CCFFB2",  # 9–35
  "#0D98BAB2",  # 35–55
  "#9ACD32B2",  # 55–150
  "#FFFF00B2",  # 150–250
  "#FF6600B2",  # 250–350
  "#C71585B2",  # 350–500
  "#2B0A78B2"   # >500
)


# Representative value assigned to each
# rendered BlueSky concentration bin.

pm25_bin_values <- c(
  5,
  22,
  45,
  100,
  200,
  300,
  425,
  600
)


# -------------------------------------------------
# MULTI-BURN EXPOSURE SCALE
# -------------------------------------------------

# This is intentionally a different scale
# from the BlueSky PM2.5 palette.
daily_exposure_breaks <- c(
  0, 5, 10, 20, 40, 80, 160, Inf
)

hourly_exposure_breaks <- c(
  0, 5, 10, 20, 40, 80, 160, Inf
)


exposure_labels <- c(
  "Minimal",
  "Low",
  "Moderate",
  "Elevated",
  "High",
  "Very High",
  "Extreme"
)

# Darker colors represent greater
# combined smoke exposure.

exposure_cols <- viridisLite::viridis(
  n = length(exposure_labels),
  direction = -1,
  alpha = 0.72
)


# -------------------------------------------------
# COLOR DECODING
# -------------------------------------------------

hex_to_rgb <- function(hex) {
  
  hex <- gsub(
    "#",
    "",
    hex
  )
  
  rgb <- grDevices::col2rgb(
    paste0(
      "#",
      substr(hex, 1, 6)
    )
  )
  
  t(rgb) / 255
}


pm25_rgb <- hex_to_rgb(
  pm25_cols
)


# Decode a rendered BlueSky PNG into a raster
# containing representative PM2.5-bin values.

decode_pm25_png_to_raster <- function(
    png_file,
    bounds,
    tolerance = 0.22
) {
  
  arr <- png::readPNG(
    png_file
  )
  
  if (
    length(dim(arr)) != 3 ||
    dim(arr)[3] < 3
  ) {
    stop(
      "PNG must contain RGB or RGBA channels: ",
      png_file
    )
  }
  
  nr <- dim(arr)[1]
  nc <- dim(arr)[2]
  
  flatten_channel <- function(x) {
    as.vector(t(x))
  }
  
  rgb <- cbind(
    flatten_channel(arr[, , 1]),
    flatten_channel(arr[, , 2]),
    flatten_channel(arr[, , 3])
  )
  
  alpha <- if (dim(arr)[3] >= 4) {
    flatten_channel(
      arr[, , 4]
    )
  } else {
    rep(
      1,
      nrow(rgb)
    )
  }
  
  
  # Ignore transparent and nearly-white pixels.
  
  transparent <- alpha < 0.05
  
  near_white <- (
    rgb[, 1] > 0.96 &
      rgb[, 2] > 0.96 &
      rgb[, 3] > 0.96
  )
  
  
  # Calculate RGB distance from each known
  # BlueSky PM2.5 color.
  
  distance_matrix <- sapply(
    seq_len(nrow(pm25_rgb)),
    function(i) {
      
      reference <- matrix(
        pm25_rgb[i, ],
        nrow = nrow(rgb),
        ncol = 3,
        byrow = TRUE
      )
      
      rowSums(
        (rgb - reference)^2
      )
    }
  )
  
  
  nearest_bin <- max.col(
    -distance_matrix,
    ties.method = "first"
  )
  
  nearest_distance <- sqrt(
    apply(
      distance_matrix,
      1,
      min
    )
  )
  
  
  values <- pm25_bin_values[
    nearest_bin
  ]
  
  
  # Unknown colors are excluded rather than
  # forcing them into the closest PM2.5 bin.
  
  values[
    transparent |
      near_white |
      nearest_distance > tolerance
  ] <- NA_real_
  
  
  r <- terra::rast(
    nrows = nr,
    ncols = nc,
    xmin = bounds$xmin,
    xmax = bounds$xmax,
    ymin = bounds$ymin,
    ymax = bounds$ymax,
    crs = "EPSG:4326"
  )
  
  
  terra::values(r) <- values
  
  r
}


# -------------------------------------------------
# COMMON COMBINATION GRID
# -------------------------------------------------

make_template_raster <- function(
    run_info,
    max_cells_side = 1200
) {
  
  xmin <- min(
    run_info$xmin,
    na.rm = TRUE
  )
  
  ymin <- min(
    run_info$ymin,
    na.rm = TRUE
  )
  
  xmax <- max(
    run_info$xmax,
    na.rm = TRUE
  )
  
  ymax <- max(
    run_info$ymax,
    na.rm = TRUE
  )
  
  
  x_span <- xmax - xmin
  y_span <- ymax - ymin
  
  resolution <- max(
    x_span,
    y_span
  ) / max_cells_side
  
  
  terra::rast(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    resolution = resolution,
    crs = "EPSG:4326"
  )
}


# -------------------------------------------------
# COMBINE BLUE SKY PNGS
# -------------------------------------------------

combine_pm25_pngs <- function(
    png_files,
    bounds_list,
    template_raster
) {
  
  if (length(png_files) == 0) {
    stop(
      "No BlueSky PNG files supplied for combination."
    )
  }
  
  if (
    length(png_files) !=
    length(bounds_list)
  ) {
    stop(
      "png_files and bounds_list must have equal lengths."
    )
  }
  
  
  decoded <- purrr::map2(
    png_files,
    bounds_list,
    function(
    png_file,
    bounds
    ) {
      
      r <- decode_pm25_png_to_raster(
        png_file = png_file,
        bounds = bounds
      )
      
      terra::resample(
        r,
        template_raster,
        method = "near"
      )
    }
  )
  
  
  # Replace NA with zero only while adding.
  # This allows runs with different spatial
  # domains to contribute independently.
  
  decoded_zero <- purrr::map(
    decoded,
    function(r) {
      terra::ifel(
        is.na(r),
        0,
        r
      )
    }
  )
  
  
  combined <- Reduce(
    `+`,
    decoded_zero
  )
  
  
  # Areas where no run contributed smoke
  # return to NA.
  
  combined <- terra::ifel(
    combined > 0,
    combined,
    NA
  )
  
  
  combined
}


# -------------------------------------------------
# WRITE EXPOSURE INDEX PNG
# -------------------------------------------------

write_exposure_index_raster_png <- function(
    r,
    filename,
    breaks
) {
  
  values <- as.matrix(
    r,
    wide = TRUE
  )
  
  
  nr <- nrow(values)
  nc <- ncol(values)
  
  
  exposure_bin <- cut(
    as.vector(t(values)),
    breaks = breaks,
    labels = FALSE,
    include.lowest = TRUE,
    right = FALSE
  )
  
  
  rgba <- grDevices::col2rgb(
    exposure_cols,
    alpha = TRUE
  ) / 255
  
  
  flat_rgba <- matrix(
    0,
    nrow = length(exposure_bin),
    ncol = 4
  )
  
  
  valid <- !is.na(
    exposure_bin
  )
  
  
  if (any(valid)) {
    
    flat_rgba[
      valid,
    ] <- t(
      rgba[
        ,
        exposure_bin[valid],
        drop = FALSE
      ]
    )
  }
  
  
  output <- array(
    0,
    dim = c(
      nr,
      nc,
      4
    )
  )
  
  
  output[, , 1] <- matrix(
    flat_rgba[, 1],
    nrow = nr,
    ncol = nc,
    byrow = TRUE
  )
  
  output[, , 2] <- matrix(
    flat_rgba[, 2],
    nrow = nr,
    ncol = nc,
    byrow = TRUE
  )
  
  output[, , 3] <- matrix(
    flat_rgba[, 3],
    nrow = nr,
    ncol = nc,
    byrow = TRUE
  )
  
  output[, , 4] <- matrix(
    flat_rgba[, 4],
    nrow = nr,
    ncol = nc,
    byrow = TRUE
  )
  
  
  png::writePNG(
    output,
    target = filename
  )
  
  
  filename
}


# -------------------------------------------------
# DAILY PNG LOCATION
# -------------------------------------------------

get_daily_pm25_png_index <- function(
    run_row
) {
  
  daily_url <- paste0(
    run_row$results_link,
    "/output/images-pm25/100m/daily_average/"
  )
  
  
  hrefs <- rvest::read_html(
    daily_url
  ) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stats::na.omit() |>
    as.character()
  
  
  # Search recursively through the daily-average
  # directory structure rather than constructing
  # one filename from assumptions about UTC offset.
  
  subdirs <- hrefs[
    grepl(
      "/$",
      hrefs
    )
  ]
  
  
  subdirs <- subdirs[
    !subdirs %in% c(
      "../",
      "./"
    )
  ]
  
  
  results <- purrr::map_dfr(
    subdirs,
    function(subdir) {
      
      subdir_url <- paste0(
        daily_url,
        subdir
      )
      
      sub_hrefs <- tryCatch(
        rvest::read_html(
          subdir_url
        ) |>
          rvest::html_elements("a") |>
          rvest::html_attr("href") |>
          stats::na.omit() |>
          as.character(),
        error = function(e) {
          character()
        }
      )
      
      
      # Look for GreyColorBar directory.
      
      grey_dirs <- sub_hrefs[
        grepl(
          "GreyColorBar/?$",
          sub_hrefs
        )
      ]
      
      
      if (length(grey_dirs) == 0) {
        return(
          tibble::tibble()
        )
      }
      
      
      purrr::map_dfr(
        grey_dirs,
        function(grey_dir) {
          
          grey_url <- paste0(
            subdir_url,
            grey_dir
          )
          
          
          files <- tryCatch(
            rvest::read_html(
              grey_url
            ) |>
              rvest::html_elements("a") |>
              rvest::html_attr("href") |>
              stats::na.omit() |>
              as.character() |>
              basename(),
            error = function(e) {
              character()
            }
          )
          
          
          files <- files[
            grepl(
              "\\.png$",
              files,
              ignore.case = TRUE
            )
          ]
          
          
          tibble::tibble(
            run_id = run_row$run_id,
            file = files,
            url = paste0(
              grey_url,
              files
            )
          )
        }
      )
    }
  )
  
  
  offset_string <- gsub(
    ":",
    "",
    run_row$utc_offset
  )
  
  date_string <- format(
    as.Date(run_row$burn_date),
    "%Y%m%d"
  )
  
  results |>
    dplyr::distinct(
      file,
      .keep_all = TRUE
    ) |>
    dplyr::filter(
      !grepl(
        "colorbar\\.png$",
        file,
        ignore.case = TRUE
      ),
      grepl(
        paste0(
          "UTC",
          offset_string,
          "_GreyColorBar_",
          date_string,
          "\\.png$"
        ),
        file
      )
    )
}


# -------------------------------------------------
# HOURLY PNG LOCATION + UTC TIME
# -------------------------------------------------

get_hourly_pm25_png_index <- function(
    run_row
) {
  
  hourly_url <- paste0(
    run_row$results_link,
    "/output/images-pm25/100m/hourly/GreyColorBar/"
  )
  
  
  files <- rvest::read_html(
    hourly_url
  ) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stats::na.omit() |>
    as.character() |>
    basename()
  
  
  files <- files[
    grepl(
      "^pm25_100m_hourly_GreyColorBar_[0-9]{12}\\.png$",
      files
    )
  ]
  
  
  files <- sort(
    unique(files)
  )
  
  
  tibble::tibble(
    run_id = run_row$run_id,
    file = files,
    url = paste0(
      hourly_url,
      files
    ),
    
    datetime_str = stringr::str_extract(
      files,
      "[0-9]{12}"
    ),
    
    datetime_utc = lubridate::ymd_hm(
      datetime_str,
      tz = "UTC"
    )
  ) |>
    dplyr::filter(
      !is.na(datetime_utc)
    ) |>
    dplyr::arrange(
      datetime_utc
    )
}


# -------------------------------------------------
# DOWNLOAD
# -------------------------------------------------

download_file_safe <- function(
    url,
    destfile
) {
  
  dir.create(
    dirname(destfile),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  curl::curl_download(
    url,
    destfile = destfile,
    quiet = TRUE
  )
  
  
  destfile
}