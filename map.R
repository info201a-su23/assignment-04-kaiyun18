# Get the state borders data for the US
us_states <- map_data("state")

# Filters all years to get only the year 20156
data_jail_2016 <- data_jail %>%
  filter(year == 2016) 

# Gets the jail population rate per state
filtered_jail_pop_2016 <- data_jail_2016 %>%
  group_by(state) %>%
  summarize(state_jail_pop_2016 = sum(total_jail_pop, na.rm = TRUE),
            state_pop_2016 = sum(total_pop, na.rm = TRUE)) %>%
  mutate(jail_pop_rate_2016 = state_jail_pop_2016/state_pop_2016)

# Changes the abbreviations to state names
state_mapping <- setNames(state.name, state.abb)

# Adds the full state name from abbreviation
filtered_jail_pop_2016$full_state_name <- state_mapping[filtered_jail_pop_2016$state]
state_shape <- map_data("state")

# Combines the two data sets
filtered_jail_pop_2016 <- filtered_jail_pop_2016 %>% 
  mutate(full_state_name = tolower(full_state_name))
prison_state_shape <- left_join(filtered_jail_pop_2016, state_shape, by = c("full_state_name" = "region"))

# Plots the map
map <- ggplot(data = prison_state_shape) +
  geom_polygon(mapping = aes(x = long,
                             y = lat,
                             group = group,
                             fill = jail_pop_rate_2016)) +
  scale_fill_continuous(low = "white",
                        high = "firebrick4",
                        limits = c(0, 0.00623)) +
  labs(title="Jail Population Rate Among the States in the U.S. in 2016",
       fill="Jail Pop Rate in 2016") + 
  coord_map()

(map)

mapmap <- plot_usmap(data = filtered_jail_pop_2016, values = "jail_pop_rate_2016", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "firebrick4", name = "Jail Population Rate", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(title = "Total Jail Rate in each state in the U.S. in 2016")