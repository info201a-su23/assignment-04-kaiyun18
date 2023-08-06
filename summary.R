data_jail <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv")
data_prison <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")

theme_classic()

year_data_jail <- data_jail %>%
  group_by(year) %>%
  filter(year >= 1990) %>%
  summarise(year_total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
            year_total_pop = sum(total_pop, na.rm = TRUE),
            year_aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
            year_aapi_pop = sum(aapi_pop_15to64, na.rm = TRUE),
            year_black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
            year_black_pop = sum(black_pop_15to64, na.rm = TRUE),
            year_latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
            year_latinx_pop = sum(latinx_pop_15to64, na.rm = TRUE),
            year_native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
            year_native_pop = sum(native_pop_15to64, na.rm = TRUE),
            year_white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
            year_white_pop = sum(white_pop_15to64, na.rm = TRUE),
            year_other_race_jail_pop = sum(other_race_jail_pop, na.rm = TRUE)) %>%
  mutate(year_total_jail_rate = year_total_jail_pop/year_total_pop,
         year_aapi_jail_rate = year_aapi_jail_pop/year_aapi_pop,
         year_black_jail_rate = year_black_jail_pop/year_black_pop,
         year_latinx_jail_rate = year_latinx_jail_pop/year_latinx_pop,
         year_native_jail_rate = year_native_jail_pop/year_native_pop,
         year_white_jail_rate = year_white_jail_pop/year_white_pop)

year_data_prison <- data_prison %>%
  group_by(year) %>%
  filter(year >= 1990) %>%
  summarise(year_total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
            year_aapi_prison_pop = sum(aapi_prison_pop, na.rm = TRUE),
            year_black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
            year_latinx_prison_pop = sum(latinx_prison_pop, na.rm = TRUE),
            year_native_prison_pop = sum(native_prison_pop, na.rm = TRUE),
            year_white_prison_pop = sum(white_prison_pop, na.rm = TRUE),
            year_other_race_prison_pop = sum(other_race_prison_pop, na.rm = TRUE))

correlation_data_set <- data.frame(year_data_jail, year_data_prison)

average_value <- data_jail %>%
  filter(year == max(year, na.rm = TRUE))
  
total_observations_jail <- nrow(data_jail)
total_observations_prison <- nrow(data_prison)

total_features_jail <- ncol(data_jail)
total_features_prison <- ncol(data_prison)


jail_date_recent <- data_jail %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(year)
jail_date_early <- data_jail %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  pull(year)
prison_date_recent <- data_prison %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(year)
prison_date_early <- data_prison %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  pull(year)

most_black_jail_rate_year <- year_data_jail %>%
  filter(year_black_jail_rate == max(year_black_jail_rate, na.rm = TRUE)) %>%
  pull(year)
most_black_jail_rate <- year_data_jail %>%
  filter(year_black_jail_rate == max(year_black_jail_rate, na.rm = TRUE)) %>%
  pull(year_black_jail_rate)

least_black_jail_rate_year <- year_data_jail %>%
  filter(year_black_jail_rate == min(year_black_jail_rate, na.rm = TRUE)) %>%
  pull(year)
least_black_jail_rate <- year_data_jail %>%
  filter(year_black_jail_rate == min(year_black_jail_rate, na.rm = TRUE)) %>%
  pull(year_black_jail_rate)

data_jail_2016 <- data_jail %>%
  filter(year == 2016) %>%
  mutate(location = paste(state, county_name, sep = " "))
data_prison_2016 <- data_prison %>%
  filter(year == 2016) %>%
  mutate(location = paste(state, county_name, sep = " "))

data_jail_prison_2016 <- merge(data_jail_2016, data_prison_2016, by = "location")

correlation_data_set <- na.omit(subset(
  data_jail_prison_2016, select = c(black_jail_pop, black_prison_pop, white_jail_pop, white_prison_pop)))

model_black_2016 <- coef(lm(correlation_data_set$black_prison_pop ~ correlation_data_set$black_jail_pop))
model_white_2016 <- coef(lm(correlation_data_set$white_prison_pop ~ correlation_data_set$white_jail_pop))


data_jail_2016 <- data_jail %>%
  filter(year == 2016)
  
most_jail_rate_state_2016 <- data_jail_2016 %>%
  group_by(state) %>%
  summarize(state_jail_pop_2016 = sum(total_jail_pop, na.rm = TRUE),
            state_pop_2016 = sum(total_pop, na.rm = TRUE)) %>%
  mutate(jail_pop_rate_2016 = state_jail_pop_2016/state_pop_2016) %>%
  filter(jail_pop_rate_2016 == max(jail_pop_rate_2016)) %>%
  pull(state)
most_jail_rate_2016 <- data_jail_2016 %>%
  group_by(state) %>%
  summarize(state_jail_pop_2016 = sum(total_jail_pop, na.rm = TRUE),
            state_pop_2016 = sum(total_pop, na.rm = TRUE)) %>%
  mutate(jail_pop_rate_2016 = state_jail_pop_2016/state_pop_2016) %>%
  filter(jail_pop_rate_2016 == max(jail_pop_rate_2016)) %>%
  pull(jail_pop_rate_2016)
least_jail_rate_state_2016 <- data_jail_2016 %>%
  group_by(state) %>%
  summarize(state_jail_pop_2016 = sum(total_jail_pop, na.rm = TRUE),
            state_pop_2016 = sum(total_pop, na.rm = TRUE)) %>%
  mutate(jail_pop_rate_2016 = state_jail_pop_2016/state_pop_2016) %>%
  filter(jail_pop_rate_2016 == min(jail_pop_rate_2016)) %>%
  pull(state)
least_jail_rate_2016 <- data_jail_2016 %>%
  group_by(state) %>%
  summarize(state_jail_pop_2016 = sum(total_jail_pop, na.rm = TRUE),
            state_pop_2016 = sum(total_pop, na.rm = TRUE)) %>%
  mutate(jail_pop_rate_2016 = state_jail_pop_2016/state_pop_2016) %>%
  filter(jail_pop_rate_2016 == min(jail_pop_rate_2016)) %>%
  pull(jail_pop_rate_2016)