transform_metadata_to_df <- function(stations_metadata) {
  stations_metadata[[1]] %>%
    purrr::map(as_tibble) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(
      latestData = lubridate::ymd_hms(latestData),
      latestData = lubridate::with_tz(latestData, tzone = "UTC"),
      location = purrr::map(location, unlist),
      lat = purrr::map_dbl(location, "latLon.lat"),
      lon = purrr::map_dbl(location, "latLon.lon")
    ) %>%
    dplyr::select(-location)
}

to_iso8601 <- function(datetime, offset) {
  # Convert datetime to Date class, add the offset, then convert back to datetime
  datetime <- lubridate::as_datetime(datetime + lubridate::days(offset))
  # Convert datetime to ISO8601 format
  iso_datetime <- anytime::iso8601(datetime)
  # Append "Z" for UTC timezone
  iso_datetime <- paste0(iso_datetime, "Z")
  return(iso_datetime)
}

transform_volumes <- function(data) {
  volumes <- data$trafficData$volume$byHour$edges
  df <- purrr::map_dfr(volumes, ~{
    tibble::tibble(
      from = .x$node$from,
      to = .x$node$to,
      volume = .x$node$total$volumeNumbers$volume
    )
  })
  return(df)
}






