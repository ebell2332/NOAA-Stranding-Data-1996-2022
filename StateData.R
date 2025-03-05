stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")

# --------------------------------------State Trends-----------------------------
------------------------------------------------------------------

##.-- Number of Strandings by State
state_trends <- stranding_data %>%
    group_by(Year_of_Observation, State) %>%
    summarise(Count = n(), .groups = "drop")
  head(state_trends)

  #. Bar Chart- Strandings by State  
  state_strandings <- ggplot(state_trends, aes(x = State, y = Count, fill = State)) +
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

##Stranding Groups by State-- Plotted each by species (not a fan of this graph)
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


# Top 5 States with Strandings by Group--side by side bar
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
