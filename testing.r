### Testing purposes
GQL(
  vol_qry(
    id=stations_metadata_df$id[1],
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

your_input <- GQL(
  vol_qry(
    id=stations_metadata_df$id[1],
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)
df <- transform_volumes(your_input)
head(df)

test <- stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes()
summary(test)


stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) -> sampled_station



station_name <- sampled_station$name

sampled_station %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume, group = 1)) + 
  geom_line(aes(color = station_name)) + 
  theme_classic() +
  labs(color = 'Traffic Station')
