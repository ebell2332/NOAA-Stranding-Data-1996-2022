stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")

# -----------Species Data, Common Name, Species Groups----------

#. **Charting Different Species----

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

# Only doing Top 5 species----
Species_counts <- stranding_data %>%
  count(Species, name = "Count") %>%    # Create count column
  arrange(desc(Count))    # Sort by highest count

top_species <- Species_counts %>% 
  top_n(5, wt = Count)  # Select top 5 species

#.Grouping Top 5 species----
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

# -----------------------------------------------------------
# Grouping Species by Common Name----

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

# -------------------------------------------------------------------------------
# Grouping By Species Group-----
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




#. Charting top 5 species for Each Species Group----
stranding_data <- stranding_data %>%
  mutate(Group_species = case_when(
    Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
    Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))

species_counts2 <- stranding_data %>%
  filter(Group_species != "Unidentified") %>%   #exclude unidentified species
  group_by(Group_species, Species) %>%
  summarise(Count = n(), .group = "drop") %>%
  arrange(Group_species, desc(Count))


# Top 5 species per group
top_species <- species_counts2 %>%
  group_by(Group_species) %>%
  slice_max(Count, n = 5) %>%
  ungroup()

print(top_species)

ggplot(top_species, aes(x = reorder(Species, Count), y = Count, fill = Group_species)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Group_species, scales = "free") +
  coord_flip() +
  labs(title = "Top 5 Species per Group",
       x = "Species",
       y = "Count") +
  theme_minimal()

#

#. Box Plot--By Species Group



#. Top 5 Species & Common Names----

  # Combine Genus and Species into one scientific name
stranding_data_gs <- stranding_data %>%
  mutate(Scientific_Name = paste(Genus, Species))

  # Count occurrences
top_sci <- stranding_data_gs %>%
  group_by(Scientific_Name) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  slice_head(n = 5)

  # Bar Plot
bar_plot_gs_top5 <- ggplot(top_sci, aes(x = reorder(Scientific_Name, Count), y = Count, fill = Scientific_Name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 5 Stranded Species (Scientific Names)",
       x = "Scientific Name",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

#ggsave("Top5_Total_Species-piechart.png", plot = bar_plot_gs_top5, width=5, height=4, units="in", dpi=300)

    # Grouping Common Names
top_common <- stranding_data %>%  
  group_by(Common_Name) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  slice_head(n = 5)
  
#Bar Chart
ggplot(top_common, aes(x = reorder(Common_Name, Count), y = Count, fill = Common_Name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 5 Most Frequent Species (Common Names)",
       x = "Common Name",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


# ---------------------------------------Species Group Age Class and Sex ----------------------------
------------------------------------------------------------------------------
  #. **Species Age/Class 
#. **Use this first then graph it by species group
  species_age_class <- stranding_data %>%
  group_by(Age_Class, Sex, Group_species, Year_of_Observation ) %>%
  summarise(Count = n(), .groups = "drop")

species_age_class


#. **Cetacean Age Class Bar Graph
cetacean_age <- species_age_class %>%
  filter(Group_species == "Cetaceans")


cetacean_age_graph <- ggplot(cetacean_age, aes(x = Age_Class, y = Count, fill = Age_Class)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Age Class Distribution of Cetacean Strandings",
       x = "Age Class",
       y = "Number of Strandings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
cetacean_age_graph
ggsave("Cetacean_Age_Class-barchart.png", plot = cetacean_age_graph, width = 7, height=6, units = "in", dpi=300)

#. Running Chi-Square Test for Cetaceans with age classes----

# Create a contingency table
c_age_species_table <- table(stranding_data$Age_Class[stranding_data$Group_species=="Cetaceans"], stranding_data$Group_species[stranding_data$Group_species=="Cetaceans"])

# Perform Chi-Square Test
chisq.test(c_age_species_table)


#. **Pinniped Age Class Strandings Bar Graph
pinniped_age <- species_age_class %>%
  filter(Group_species == "Pinnipeds")

pinniped_age_graph <- ggplot(pinniped_age, aes(x = Age_Class, y = Count, fill = Age_Class)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Age Class Distribution of Pinniped Strandings",
       x = "Age Class",
       y = "Number of Strandings") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
pinniped_age_graph
ggsave("Pinniped_Age_Class-barchart.png", plot = pinniped_age_graph, width = 7, height=6, units = "in", dpi=300)


#. --Running Chi-Square Test for Cetaceans with age classes----

# Create a contingency table
p_age_species_table <- table(stranding_data$Age_Class[stranding_data$Group_species=="Pinnipeds"], stranding_data$Group_species[stranding_data$Group_species=="Pinnipeds"])

# Perform Chi-Square Test
chisq.test(p_age_species_table)




