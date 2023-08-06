data_jail_2016 <- data_jail %>%
  filter(year == 2016) 

filtered_jail_pop_2016 <- data_jail_2016 %>%
  group_by(state) %>%
  summarize(state_jail_pop_2016 = sum(total_jail_pop, na.rm = TRUE),
            state_pop_2016 = sum(total_pop, na.rm = TRUE)) %>%
  mutate(jail_pop_rate_2016 = state_jail_pop_2016/state_pop_2016)

map <- plot_usmap(data = filtered_jail_pop_2016, values = "jail_pop_rate_2016", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "firebrick4", name = "Jail Population Rate", label = scales::comma
  ) + theme(legend.position = "right") +
  labs(title = "Total Jail Rate in each state in the U.S. in 2016")