stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")

#install.packages("knitr")
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
#install.packages("gt")
#library(gt)
# ------------------------------Yearly Trends-------------------------------
----------------------------------------------------------
  
##.--Annual Group Species Trend

stranding_data <- stranding_data %>%
    mutate(Group_species = case_when(
      Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
      Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
      TRUE ~ "Unidentified"
    ))  
  
  # Summarize annual strandings by species group19
  annual_strandings <- stranding_data %>%
    group_by(Year_of_Observation, Group_species) %>%
    summarise(Total_Strandings = n(), .groups = "drop")
  annual_strandings
#.--View Summary table
print(annual_strandings)
  

  
#Pivot Summary Table to make Year x Species Group Table
  annual_strandings_wide <- annual_strandings %>%
    pivot_wider(names_from = Group_species, values_from = Total_Strandings, values_fill = 0)
#.--Annual Strandings By Species Group Table 
kable(annual_strandings_wide, caption = "Annual Strandings by Species Group") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#. ** Stranding by Year----
trend_plot <- ggplot(stranding_data, aes(x = Year_of_Observation)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Strandings by year", 
       x = "Year", y = "Number of Strandings") +
  theme_minimal()
print(trend_plot)

table(stranding_data$Species)

#. ** Strandings per year by each species group
yearly_trends <- stranding_data %>%
  group_by(Year_of_Observation, Group_species ) %>%
  summarise(Count = n(), .groups = "drop")

yearly_trends <- yearly_trends %>% filter(!is.na(Count))  #filter out na lines

##. Line Plot Graphing Annual Strandings by Species Group
yearly_group_trends <- ggplot(yearly_trends, aes(x = Year_of_Observation, y = Count, color = Group_species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +        # Points for visibility
  labs(title = "Trends in Marine Mammal Strandings by Group",
       x = "Year",
       y = "Number of Strandings",
       color = "Species Group") +
  theme_minimal()
yearly_group_trends

#ggsave("yearlytrend_by_group-lineplot.png", plot = yearly_group_trends, width = 5, height=4, units = "in", dpi=300)

  ##. Species x Group x Year Table----Still need to export both as tables 
#** Strandings per year by each species group
yearly_trends <- stranding_data %>%
  group_by(Year_of_Observation, Group_species ) %>%
  summarise(Count = n(), .groups = "drop")

#Filter for Cetaceans Species Group
cetaceans_strandings <- yearly_trends %>% filter(Group_species=="Cetaceans")

# Annual Strandings by Cetaceans Species Group Table
kable(cetaceans_strandings, caption = "Annual Strandings by Cetaceans Species Group") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#Filter for Pinnipeds
pinnipeds_strandings <- yearly_trends %>% filter(Group_species=="Pinnipeds")

# Annual Strandings by Cetaceans Species Group Table
kable(pinnipeds_strandings, caption = "Annual Strandings by Pinnipeds Species Group") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



# --------------------------------------------Yearly Trend By State-------------------------
-------------------------------------------------------------------------------------
#.--Yearly State Trend
  state_trends <- stranding_data %>%
  group_by(Year_of_Observation, State) %>%
  summarise(Count = n(), .groups = "drop")
head(state_trends)

#. Line Plot For Year and State
state_trends <- ggplot(state_trends, aes(x = Year_of_Observation, y = Count, color = State)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Yearly Trends in Strandings by State", x = "Year", y = "Number of Strandings", color = "State") +
  theme_minimal()
state_trends

#ggsave(filename = "state_trends_yearly-line.png", plot = state_trends, width = 5, height = 4, units = "in", dpi=300)


