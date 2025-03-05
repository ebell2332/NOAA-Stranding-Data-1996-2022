stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")

# -----------Species Data, Common Name, Species Groups----------

#1. #. **Charting Different Species

#-- Count strandings per species
Species_counts <- stranding_data %>%
  group_by(Species) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)   #Calculates %

#. Create pie chart
ggplot(Species_counts, aes(x = "", y = Count, fill = Species)) +
  geom_col() +
  coord_polar(theta = "y") +  # Converts bar chart to pie chart
  labs(title = "Proportion of Strandings by Species") +
  theme_void()

# Only doing Top 5 species 
Species_counts <- stranding_data %>%
  count(Species, name = "Count") %>%    # Create count column
  arrange(desc(Count))    # Sort by highest count

top_species <- Species_counts %>% 
  top_n(5, wt = Count)  # Select top 5 species

#.--Grouping Top 5 species
Species_counts <- Species_counts %>%
  mutate(Species = ifelse(Species %in% top_species$Species, Species, "Other")) %>%      # If not in top 5 species, label as other
  group_by(Species) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%     # Ensure Count exists before summarizing
  mutate(Percentage = (Count / sum(Count)) * 100)       # Calculate percentage

species_pie_chart <- ggplot(Species_counts, aes(x = "", y = Count, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +  # Converts bar chart to pie chart
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4) +
  labs(title = "Proportion of Strandings by Species (Top 5 and Other") +
  theme_void()
species_pie_chart

#ggsave("Top5_Species_Strandings-piechart.png", plot = species_pie_chart, width=5, height=4, units="in", dpi=300)

# --------------------------------Grouping Species by Common Name---------------------------
-------------------------------------------------------------------------
### Grouping By Common Name
stranding_data1 <- stranding_data %>%
  mutate(Species_group = case_when(
    Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima") ~ "Whales",
    Species %in% c("acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus") ~ "Dolphins",
    Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Seals",
    Species %in% c("phocoena") ~ "Porpoise",
    TRUE ~ "Unidentified"
  ))

species_group_counts <- stranding_data1 %>%
  group_by(Species_group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##-Bar Chart--By Common Species
common_name <- ggplot(species_group_counts, aes(x = Species_group, y = Count, fill = Species_group)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  labs(title = "Strandings by Species Common Name", x = "Common Name", y = "Count") +
  theme_minimal()
common_name

#ggsave("Common_Name_Groupings-barchart.png", plot = common_name, width = 7, height = 7, units = "in", dpi=300)

# ------------------------------------Grouping By Species Group-------------------------------------------
---------------------------------------------------------------------------
###---By Species Groups
stranding_data <- stranding_data %>%
  mutate(Group_species = case_when(
    Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
    Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))

group_species_counts <- stranding_data %>%
  group_by(Group_species) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

#. Bar Graph--By Species Group
group_name <- ggplot(group_species_counts, aes(x = Group_species, y = Count, fill = Group_species)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  labs(title = "Strandings by Species Group", x = "Group", y = "Count") +
  theme_minimal()
group_name

#ggsave("SpeciesGroup_name-barchart.png", plot = group_name, width = 7, height=6, units = "in", dpi=300)

# ---------------------------------------Species Group Age Class and Sex ----------------------------
------------------------------------------------------------------------------
  #. **Species Age/Class - not a fan, hard to see data clearly
  
  species_age_class <- stranding_data %>%
  group_by(Age_Class, Sex, Group_species, Year_of_Observation ) %>%
  summarise(Count = n(), .groups = "drop")

species_age_class

species_age_class <- ggplot(species_age_class, aes(x = Year_of_Observation, y = Count, color = Age_Class)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Group_species) +
  labs(title = "Trends in Age Class by Species Group", x = "Year", y = "Number of Strandings", color = "Age Class") +
  theme_minimal()

species_age_class