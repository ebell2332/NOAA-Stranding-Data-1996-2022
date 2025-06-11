

# --------------------------------------State Trends-----------------------------
------------------------------------------------------------------

##.-- Number of Strandings by State
state_trends <- final_clean %>%
    group_by(year_of_observation, state) %>%
    summarise(Count = n(), .groups = "drop")
  head(state_trends)

  #. Bar Chart- Strandings by State  
  state_strandings <- ggplot(state_trends, aes(x = state, y = Count, fill = state)) +
  geom_col() +
  labs(title = "Total Strandings by State", x = "State", y = "Total Strandings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
state_strandings

#.--Table summarizing # of Strandings per State by species group
grouped_trends <- stranding_data %>%
  group_by(Year_of_Observation, State, Group_species) %>%
  summarise(Count = n(), .groups = "drop")

print(grouped_trends)

##Stranding Groups by State-- Plotted each by species (not a fan of this graph) ----
ggplot(grouped_trends, aes(x = Year_of_Observation, y = Count, color = State, group = State)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Group_species) +
  labs(title = "Yearly Trends of Species Groups by State", x = "Year", y = "Number of Strandings", color = "State") +
  theme_minimal()

## Single Line Chart with Color 
ggplot(grouped_trends, aes(x = Year_of_Observation, y = Count, color = Group_species, group = Group_species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Yearly Trends of Species Groups Across States", x = "Year", y = "Number of Strandings", color = "Species Group") +
  theme_minimal()

##Line Chart by Species Group--Plot by state
ggplot(grouped_trends, aes(x = Year_of_Observation, y = Count, color = Group_species, group = Group_species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~State, scale = "free") +
  labs(title = "Yearly Trends of Species Group by State", x = "Year", y = "Number of Strandings", color = "Species Group") +
  theme_minimal()

# -----------------------------------Top 5 States---------------------------

  #. **Top 5 States
top_states <- stranding_data %>%
  group_by(State) %>%
  summarise(Total_strandings = n(), .groups = "drop") %>%
  top_n(5, wt = Total_strandings)

top_states_data <- stranding_data %>%       # filter data for only these states
  filter(State %in% top_states$State)

# Bar Graph--Total Strandings for top 5 States
ggplot(top_states, aes(x = reorder(State, -Total_strandings), y = Total_strandings, fill = State)) +
  geom_col() +
  labs(title = "Top 5 States with the Most Strandings", x = "State", y = "Total Strandings") +
  theme_minimal() +
  theme(legend.position = "none")   # Hide legend since color is just for visualization

# Line Chart--Annual Stranding Trends for Top 5 States
top_states_trends <- stranding_data %>%
  filter(State %in% top_states$State) %>%
  group_by(Year_of_Observation, State) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(top_states_trends, aes(x = Year_of_Observation, y = Count, color = State, group = State)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Yearly Trends in Strandings for Top 5 States", x = "Year", y = "Number of Strandings", color = "State") +
  theme_minimal()

# Top 5 States with Strandings by Group--stacked bar
species_by_state <- top_states_data %>%
  group_by(State, Group_species) %>%
  summarise(Count = n(), .groups = "drop")

state_strandings_top5<- ggplot(species_by_state, aes( x = State, y = Count, fill = Group_species)) +
  geom_col(position = "stack") +
  labs(title = "Strandings by Species Group in Top 5 States", x = "State", y = "Number of Strandings", fill = "Species Group") +
  theme_minimal()
state_strandings_top5

#ggsave(filename = "Top5_State_Strandings-bar.png", plot = state_strandings_top5, width = 5, height=4, units = "in", dpi=300)


# Top 5 States with Strandings by Group--side by side bar----
species_by_state <- top_states_data %>%
  group_by(State, Group_species) %>%
  summarise(Count = n(), .groups = "drop")

state_strandings_2 <- ggplot(species_by_state, aes(x = State, y = Count, fill = Group_species)) +
  geom_col(position = "dodge") +
  labs(title = "Strandings by Species Group in Top 5 States",
       x = "State",
       y = "Number of Strandings",
       fill = "Species Group") +
  theme_minimal()
state_strandings_2

#ggsave(filename = "Top5_State_Strandings-sidebar.png", plot = state_strandings_2, width = 5, height=4, units = "in", dpi=300)


# Top 5 Bodies of Water----

body_water <- stranding_data %>%      #need to group atlantic ocean, and atlantic together
  group_by(Body_of_Water) %>%
  summarise(Count = n(), .groups = "drop")

top_waters <- body_water %>% 
  top_n(5, wt = Count)


library(colorspace)

body_water_plot <- ggplot(top_waters, aes(x = Body_of_Water, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Bodies of Water with the Most Strandings",
       x = "Body of Water",
       y = "Number of Strandings") +
  theme_minimal()
body_water_plot



#. Boxplot of Annual Strandings by State ----

library(ggplot2)

ggplot(state_trends, aes(x = State, y = Count)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Annual Strandings by State",
       x = "State", y = "Annual Stranding Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
  )


#. Boxplot top 10 states by annual strandings
top_states <- stranding_data %>%
  count(State) %>%
  top_n(10, n) %>%
  pull(State)

filtered_data <- state_trends %>%
  filter(State %in% top_states)

ggplot(filtered_data, aes(x = State, y = Count)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Annual Strandings by Top 10 States",
       x = "State", y = "Annual Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#. Boxplot Top 5 States by Annual Strandings
top_states <- stranding_data %>%
  count(State) %>%
  top_n(5, n) %>%
  pull(State)

filtered_data <- state_trends %>%
  filter(State %in% top_states)

ggplot(filtered_data, aes(x = State, y = Count)) +
  geom_boxplot(fill = "skyblue3") +
  labs(title = "Annual Strandings by Top 5 States",
       x = "State", y = "Annual Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




################
#Plotting Maine and Massachusetts separately from total strandings
#both are much higher in total number than all the other states

#maine line plot
#2006, 2007, 2018 have highest numbers
maine_only <- final_clean %>%
  filter(state == "ME")

ggplot(maine_only, aes(x = year_of_observation)) +
  geom_bar(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Marine Mammal Strandings in Maine by Year",
       x = "Year", y = "Count")

#massachusetts line plot
#2017-2019 has highest numbers
ma_only <- final_clean %>%
  filter(state == "MA")

ggplot(ma_only, aes(x = year_of_observation)) +
  geom_bar(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Marine Mammal Strandings in Massachusetts by Year",
       x = "Year", y = "Count")


#plotting all states excluding maine and massachussets
#2013, 2018-2019 have highest numbers
filtered_states <- final_clean %>%
  filter(!(state %in% c("MA", "ME"))) %>%
  filter(!is.na(year_of_observation))

ggplot(filtered_states, aes(x = factor(year_of_observation))) +
    geom_bar(fill = "darkgreen") +
    theme_minimal() +
    labs(
      title = "Marine Mammal Strandings by Year (Excluding Maine & Massachusetts)",
      x = "Year",
      y = "Number of Strandings"
    )
  

