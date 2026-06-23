# R/multi_burn_helpers.R
# Helper functions for Multi-burn Exposure report

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

sanitize_slug <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  if (!nzchar(x)) "multi-burn-exposure" else x
}

make_multi_burn_filename <- function(project_name, burn_date, issued_at = Sys.time()) {
  date_part <- format(as.Date(burn_date), "%Y%m%d")
  issued_id <- format(issued_at, "i%H%M")
  paste0(date_part, "-", sanitize_slug(project_name), "-multi-burn-exposure-", issued_id, ".html")
}

resolve_bluesky_results_link <- function(run_id) {
  run_id <- trimws(as.character(run_id))
  if (!nzchar(run_id)) stop("Missing BlueSky run ID.")

  links <- c(
    paste0("https://playground-1.airfire.org/bluesky-web-output/", run_id, "-dispersion"),
    paste0("https://playground-2.airfire.org/bluesky-web-output/", run_id, "-dispersion")
  )

  end_times <- lapply(links, function(link) {
    out_url <- paste0(link, "/output.json")
    if (!RCurl::url.exists(out_url)) return(as.POSIXct(NA))
    tryCatch(
      lubridate::as_datetime(rjson::fromJSON(file = out_url)$runtime[["end"]]),
      error = function(e) as.POSIXct(NA)
    )
  })

  end_times <- do.call(c, end_times)
  if (all(is.na(end_times))) {
    stop("Could not find BlueSky output for run ID: ", run_id)
  }

  links[which.max(end_times)]
}

read_bluesky_run_info <- function(run_id, burn_name = NA_character_) {
  results_link <- resolve_bluesky_results_link(run_id)

  fire_file <- paste0(results_link, "/output/data/fire_locations.csv")
  grid_file <- paste0(results_link, "/output/grid_info.json")

  fire <- read.csv(fire_file, stringsAsFactors = FALSE)
  grid <- jsonlite::fromJSON(grid_file)

  fire_one <- fire[1, , drop = FALSE]

  tz <- lutz::tz_lookup_coords(
    lat = fire_one$latitude,
    lon = fire_one$longitude,
    method = "fast"
  )

  tibble::tibble(
    run_id = as.character(run_id),
    burn_name = as.character(burn_name %||% NA_character_),
    label = dplyr::if_else(
      !is.na(burn_name) & nzchar(trimws(burn_name)),
      paste0(trimws(burn_name), " — ", run_id),
      as.character(run_id)
    ),
    results_link = results_link,
    lat = fire_one$latitude,
    lon = fire_one$longitude,
    utc_offset = fire_one$utc_offset,
    date_time = fire_one$date_time,
    burn_date = as.Date(lubridate::ymd(fire_one$date_time)),
    acres = fire_one$area,
    tz = tz,
    xmin = grid$bbox[1],
    ymin = grid$bbox[2],
    xmax = grid$bbox[3],
    ymax = grid$bbox[4]
  )
}

# Original BlueSky PM2.5 PNG palette used only for decoding the rendered images.
pm25_cols <- c(
  "#CCE5FFB2",  # 1-9
  "#99CCFFB2",  # 9-35
  "#0D98BAB2",  # 35-55
  "#9ACD32B2",  # 55-150
  "#FFFF00B2",  # 150-250
  "#FF6600B2",  # 250-350
  "#C71585B2",  # 350-500
  "#2B0A78B2"   # >500
)

pm25_bins <- c(1, 9, 35, 55, 150, 250, 350, 500, Inf)
pm25_bin_values <- c(5, 22, 45, 100, 200, 300, 425, 600)

# New palette/product scale for combined multi-burn output.
# This intentionally does not reuse the BlueSky PM2.5 colors because the output is
# an exposure index estimated from summed rendered bins, not a direct PM2.5 field.
exposure_breaks <- c(0, 50, 100, 200, 400, 800, 1200, Inf)
exposure_labels <- c("Minimal", "Low", "Moderate", "Elevated", "High", "Very High", "Extreme")
exposure_cols <- viridisLite::viridis(length(exposure_labels), direction = -1, alpha = 0.72)

hex_to_rgb <- function(hex) {
  hex <- gsub("#", "", hex)
  rgb <- grDevices::col2rgb(paste0("#", substr(hex, 1, 6)))
  t(rgb) / 255
}

pm25_rgb <- hex_to_rgb(pm25_cols)

# Decode a BlueSky PM2.5 PNG to a terra raster of representative PM2.5 values.
# This relies on the known BlueSky PM2.5 palette. Transparent/white/unknown pixels are treated as NA.
decode_pm25_png_to_raster <- function(png_file, bounds, tolerance = 0.22) {
  arr <- png::readPNG(png_file)
  if (length(dim(arr)) != 3 || dim(arr)[3] < 3) {
    stop("PNG must have RGB or RGBA channels: ", png_file)
  }

  nr <- dim(arr)[1]
  nc <- dim(arr)[2]
  flatten_channel <- function(x) as.vector(t(x))
  rgb <- cbind(
    flatten_channel(arr[, , 1]),
    flatten_channel(arr[, , 2]),
    flatten_channel(arr[, , 3])
  )

  alpha <- if (dim(arr)[3] >= 4) flatten_channel(arr[, , 4]) else rep(1, nrow(rgb))
  near_white <- rowMeans(rgb) > 0.96
  transparent <- alpha < 0.05

  d <- sapply(seq_len(nrow(pm25_rgb)), function(i) {
    rowSums((rgb - matrix(pm25_rgb[i, ], nrow = nrow(rgb), ncol = 3, byrow = TRUE))^2)
  })

  nearest <- max.col(-d, ties.method = "first")
  nearest_dist <- sqrt(apply(d, 1, min))

  vals <- pm25_bin_values[nearest]
  vals[transparent | near_white | nearest_dist > tolerance] <- NA_real_

  r <- terra::rast(
    nrows = nr,
    ncols = nc,
    xmin = bounds$xmin,
    xmax = bounds$xmax,
    ymin = bounds$ymin,
    ymax = bounds$ymax,
    crs = "EPSG:4326"
  )

  terra::values(r) <- vals
  r
}

write_exposure_index_raster_png <- function(r, filename) {
  m <- as.matrix(r, wide = TRUE)
  nr <- nrow(m)
  nc <- ncol(m)

  bin <- cut(
    as.vector(t(m)),
    breaks = exposure_breaks,
    labels = FALSE,
    include.lowest = TRUE,
    right = FALSE
  )

  out <- array(0, dim = c(nr, nc, 4))
  rgba <- grDevices::col2rgb(exposure_cols, alpha = TRUE) / 255

  ok <- !is.na(bin)
  if (any(ok)) {
    flat <- matrix(0, nrow = length(bin), ncol = 4)
    flat[ok, ] <- t(rgba[, bin[ok], drop = FALSE])
    out[, , 1] <- matrix(flat[, 1], nrow = nr, ncol = nc, byrow = TRUE)
    out[, , 2] <- matrix(flat[, 2], nrow = nr, ncol = nc, byrow = TRUE)
    out[, , 3] <- matrix(flat[, 3], nrow = nr, ncol = nc, byrow = TRUE)
    out[, , 4] <- matrix(flat[, 4], nrow = nr, ncol = nc, byrow = TRUE)
  }

  png::writePNG(out, target = filename)
  filename
}

# Backward-compatible alias in case other app code still calls the old name.
write_pm25_raster_png <- write_exposure_index_raster_png


make_template_raster <- function(run_info, resolution = NULL) {
  xmin <- min(run_info$xmin, na.rm = TRUE)
  ymin <- min(run_info$ymin, na.rm = TRUE)
  xmax <- max(run_info$xmax, na.rm = TRUE)
  ymax <- max(run_info$ymax, na.rm = TRUE)

  # Keep output reasonably sized for Shiny/GitHub Pages. This is the common grid used
  # only for combining decoded PNG bins, not for scientific model reruns.
  if (is.null(resolution)) {
    max_cells_side <- 1200
    resolution <- max((xmax - xmin), (ymax - ymin)) / max_cells_side
  }

  terra::rast(
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    resolution = resolution,
    crs = "EPSG:4326"
  )
}

combine_pm25_pngs <- function(png_files, bounds_list, template_raster, out_png) {
  rasters <- purrr::map2(png_files, bounds_list, function(png_file, bounds) {
    r <- decode_pm25_png_to_raster(png_file, bounds)
    terra::resample(r, template_raster, method = "near")
  })

  combined <- Reduce(function(a, b) {
    terra::ifel(is.na(a), 0, a) + terra::ifel(is.na(b), 0, b)
  }, rasters)

  combined <- terra::ifel(combined <= 0, NA, combined)
  write_exposure_index_raster_png(combined, out_png)

  list(raster = combined, png = out_png)
}

get_daily_pm25_png_url <- function(run_row) {
  paste0(
    run_row$results_link,
    "/output/images-pm25/100m/daily_average/UTC",
    gsub(":", "", run_row$utc_offset),
    "/GreyColorBar/pm25_100m_daily_average_UTC",
    gsub(":", "", run_row$utc_offset),
    "_GreyColorBar_",
    run_row$date_time,
    ".png"
  )
}

get_hourly_pm25_png_index <- function(run_row) {
  hourly_url <- paste0(run_row$results_link, "/output/images-pm25/100m/hourly/GreyColorBar/")

  files <- rvest::read_html(hourly_url) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stats::na.omit() |>
    as.character() |>
    basename()

  files <- files[grepl("^pm25_100m_hourly_GreyColorBar_[0-9]{12}\\.png$", files)]
  files <- sort(unique(files))

  tibble::tibble(
    run_id = run_row$run_id,
    file = files,
    url = paste0(hourly_url, files),
    datetime_str = stringr::str_extract(files, "[0-9]{12}"),
    datetime_utc = lubridate::ymd_hm(datetime_str, tz = "UTC")
  ) |>
    dplyr::filter(!is.na(datetime_utc)) |>
    dplyr::arrange(datetime_utc)
}

download_file_safe <- function(url, destfile) {
  curl::curl_download(url, destfile = destfile, quiet = TRUE)
  destfile
}
