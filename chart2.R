correlation_black <- ggplot(data = correlation_data_set) +
  geom_point(mapping = aes(x = black_jail_pop, y = black_prison_pop), color = "goldenrod3") +
  geom_smooth(method = 'gam', mapping = aes(x = black_jail_pop, y = black_prison_pop), color = "gold2") +
  labs(title = "2016 Black Jail vs Prison Population",
       x = "Jail Population",
       y = "Prison Population")

correlation_white <- ggplot(data = correlation_data_set) +
  geom_point(mapping = aes(x = white_jail_pop, y = white_prison_pop), color = "orchid4") +
  geom_smooth(method = 'gam', mapping = aes(x = white_jail_pop, y = white_prison_pop), color = "mediumorchid") +
  labs(title = "2016 White Jail vs Prison Population",
       x = "Jail Population",
       y = "Prison Population")