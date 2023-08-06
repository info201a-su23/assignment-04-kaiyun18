year_trend_race_rate <- ggplot() +
  geom_line(data = year_data_jail, mapping = aes(
    x = year, 
    y = year_aapi_jail_rate,
    color = "AAPI"
  )) +
  geom_line(data = year_data_jail, mapping = aes(
    x = year, 
    y = year_black_jail_rate,
    color = "Black"
  )) +
  geom_line(data = year_data_jail, mapping = aes(
    x = year, 
    y = year_latinx_jail_rate,
    color = "Latinx"
  )) +
  geom_line(data = year_data_jail, mapping = aes(
    x = year, 
    y = year_native_jail_rate,
    color = "Native"
  )) +
  geom_line(data = year_data_jail, mapping = aes(
    x = year, 
    y = year_white_jail_rate,
    color = "White"
  )) +
  labs(
    title = "Trends of Different Races Jail Population Rate from 1990, United States",
    x = "Year",
    y = "Population Rate",
    color = "Races"
  )
