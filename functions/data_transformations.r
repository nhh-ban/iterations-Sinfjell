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
