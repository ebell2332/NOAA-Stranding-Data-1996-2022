stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")

# Grouping Stranding Data by Seasons----
stranding_data <- stranding_data %>%
  mutate(Seasons = case_when(
    Month_of_Observation %in% c("Mar", "Apr", "May") ~ "Spring",
    Month_of_Observation %in% c("Jun", "Jul", "Aug") ~ "Summer",
    Month_of_Observation %in% c("Sep", "Oct", "Nov") ~ "Fall",
    Month_of_Observation %in% c("Dec", "Jan", "Feb") ~ "Winter"
  ))

season_order <- c("Spring", "Summer", "Fall", "Winter")
stranding_data$Seasons <- factor(stranding_data$Seasons, levels = season_order)

# Grouping Stranding Data by Species----
stranding_data <- stranding_data %>%
  mutate(Group_species = case_when(
    Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
    Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))

#---------------Seasonal Trends---------------
----------------------------------------------
  ##-Grouping Species
group_species_counts <- stranding_data %>%
  group_by(Group_species) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##-Grouping Species and Seasons together
seasonal_species_counts <- stranding_data %>%
  group_by(Group_species, Seasons) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Group_species, Seasons, fill = list(Count = 0))
seasonal_species_counts

##. Bar Chart-SpeciesxSeasonsxCount
seasons_plot <- ggplot(seasonal_species_counts, aes(x = Seasons, y = Count, fill = Group_species)) +
  geom_bar(stat = "identity", position = "stack") +      #Separate bar for each species group
  labs(title = "Seasonal Trend in Strandings Per Species Group", x = "Season", y = "Number of Strandings", fill = "Species Group") +
  theme_minimal() +
  theme(legend.position=c("right")) +
  theme(axis.text.x=element_text(size=14,color="black")) +
  theme(axis.text.y=element_text(size=14,color="black")) +
  theme(axis.title.x=element_text(size=16)) +
  theme(axis.title.y=element_text(size=16)) +
  theme(plot.title=element_text(size=18))
seasons_plot



##. Line Chart-SpeciesxSeasonsxCount
seasons_plot1 <- ggplot(seasonal_species_counts, aes(x = Seasons, y = Count, color = Group_species, group = Group_species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Seasonal Trends in Strandings by Species", x = "Season", y = "Number of Strandings", color = "Species Group") +
  theme_minimal()
seasons_plot1

#ggsave(filename = "Seasonal_Species_linechart.png", plot = seasons_plot1, width = 7, height = 4, units = "in", dpi = 300)

##--Group Species, Release Status, by Season

#. Grouping
species_healthy_season <- stranding_data %>%
  group_by(Seasons, Group_species, `Deemed_Healthy/Releasable_Flag`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Seasons, Group_species) %>%
  mutate(Proportion = Count / sum(Count))

##--Bar Chart-Seasonal Trends x Releases x Species
healthy_species_season_plot <- ggplot(species_healthy_season, aes(x = Seasons, y = Proportion, fill = `Deemed_Healthy/Releasable_Flag`)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Group_species) +
  labs(title = "Proportion of Releasable Strandings by Season", 
       x = "Season", 
       y = "Proportion of Strandings", 
       fill = "Releasable") +
  theme_minimal()
healthy_species_season_plot

#ggsave(filename = "Seasonal_Releasable_Trend-bar.png", plot = healthy_species_season_plot, width = 7, height = 5, units = "in", dpi = 300)