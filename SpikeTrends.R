library(dplyr)

#Charting Spikes for the States and the Years----

#Checking how many years each state has
year_count_by_state <- stranding_data %>%
  group_by(State) %>%
  summarise(num_years = n_distinct(Year_of_Observation))

# View the result
print(year_count_by_state)

# Filter out North Carolina (NC) and Pennsylvania (PA)
filtered_data <- stranding_data %>%
  filter(!State %in% c("NC", "PA"))

# Check the filtered dataset
head(filtered_data)

# Count strandings by state and year (assuming "state" and "year" columns)
summary_data <- filtered_data %>%
  group_by(State, Year_of_Observation) %>%
  summarise(count_strandings = n(), .groups = "drop")

# View the summarized count data
head(summary_data)

#install.packages("zoo")
library(zoo)

# Calculate a moving average (e.g., over 3 years) for each state
summary_data <- summary_data %>%
  group_by(State) %>%
  arrange(Year_of_Observation) %>%
  mutate(moving_avg = rollmean(count_strandings, 3, fill = NA, align = "center"))

# View the data with moving average
#head(summary_data)


summary_data <- summary_data %>%
  group_by(State) %>%
  mutate(moving_avg = zoo::rollapply(count_strandings, width = 3, FUN = mean, fill = NA, align = "center"))

# Check the result
#head(summary_data)

#View(summary_data)

summary_data <- summary_data %>%
  mutate(is_spike = ifelse(count_strandings > 1.5 * moving_avg, TRUE, FALSE))   # spike = 1.5 is when the count is more than 1.5x moving average 

# Filter for the years with spikes
spike_years <- summary_data %>%
  filter(is_spike == TRUE)

# View the spike years
View(spike_years)

# Plot strandings by year and state----
spike_stateyr_plot <- ggplot(summary_data, aes(x = Year_of_Observation, y = count_strandings, color = State)) +
  geom_line() + 
  geom_point(data = spike_years, aes(x = Year_of_Observation, y = count_strandings), color = "red") + 
  labs(title = "Strandings by Year and State", y = "Total Strandings", x = "Year")

#ggsave(filename = "State Spike Trends Plot.png", plot = spike_stateyr_plot, width = 7, height = 4, units = "in", dpi = 300)
