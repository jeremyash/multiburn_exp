make_report_filename <- function(
    burn_name,
    date,
    issued_time
) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-i",
    format(issued_time, "%H%M"),
    ".html"
  )
}

make_pb_filename <- function(
    burn_name,
    date,
    issued_time
) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-i",
    format(issued_time, "%H%M"),
    ".html"
  )
}

make_kmz_filename <- function(
    burn_name,
    date,
    issued_time
) {
  paste0(
    format(date, "%Y%m%d"),
    "-",
    safe_filename(burn_name),
    "-i",
    format(issued_time, "%H%M"),
    "-bsky-dispersion.kmz"
  )
}