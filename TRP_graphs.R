#------------------------North Atlantic Right Whales----------------------------
#1. Graph for NARW  and Strandings
filtered_narw <- final_set %>%
  filter(genus == "Eubalaena"& species == "glacialis")  # North atlantic right whales

yearly_narw <- filtered_narw %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(yearly_narw, aes(x = year_of_observation, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  labs(
    title = "Number of North Atlantic Right Whale Strandings",
    subtitle = "From 1996-2022",
    x = "Year of Observation",
    y = "Number of Strandings"
  )
theme_minimal()

print(yearly_narw)

#2. Plotting NARW strandings and Policy Year Implementation
# Step 1: Filter for the desired genus and species
filtered_narw <- final_set %>%
  filter(genus == "Eubalaena"& species == "glacialis")  # North Atlantic right whales

# Step 2: Count strandings by year
yearly_narw <- filtered_narw %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

# Step 3: Plot
narw_graph <- ggplot(yearly_narw, aes(x = year_of_observation, y = Count)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Annual Strandings of North Atlantic Right Whales",
    subtitle = "North Atlantic Right Whale Strandings and the Years of Policies Implemented",
    x = "Year",
    y = "Number of Strandings"
  ) +
  geom_segment(aes(x = 2001, xend= 2001, y = 0, yend = 8),
  linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  1998, y = 3, 
            label = "Mandatory Ship Reporting System", angle = 0, vjust = -0.5, size = 3.5) +
  geom_segment(aes(x = 2006, xend= 2006, y = 0, yend = 8),
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2004, y = 5, 
            label = "Listed under ESA", angle = 0, vjust = -0.5, size = 4) +
  geom_segment(aes(x = 2008, xend= 2008, y = 0, yend = 8),
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2013, y = 6,
            label = "NARW Vessel Speed Regulations Implemented", angle = 0, vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 8))
  theme_minimal()
narw_graph  

#3. ---Number of NARW Strandings by Areas (per Boundaries)---  --- --- ---
final_set <- final_set %>%
  mutate(
    Area = case_when(
      lat >= 42.044 ~ "Area 1|North of Cape Cod",
      lat >= 40.409 & lat < 42.044 ~ "Area 2|Cape Cod to NJ",
      lat < 40.409 ~ "Area 3|South of Highlands NJ",
      TRUE ~ NA_character_
    )
  )

final_set %>%
  filter(
    genus %in% c("Eubalaena") &
      species %in% c("glacialis")
  ) %>%
  group_by(Area, Year = year_of_observation, genus, species) %>%
  summarise(Strandings = n(), .groups = "drop") %>%
  arrange(Area, Year)   #counting my area & year


final_set %>%
  filter(
    genus %in% c("Eubalaena") &
      species %in% c("glacialis")
  ) %>%
  group_by(Area, genus, species) %>%
  summarise(Strandings = n(), .groups = "drop") %>%
  arrange(Area)   #counting my area & year


#4. ---Plotting # of Interaction Types for NARW--- --- --- --- -- --- --- --- --
  # Step 1: Filter and recode interactions for specific species
interaction_narw <- final_set %>%
  filter(genus == "Eubalaena", species == "glacialis") %>%
  mutate(across(
    c(boat_collision, shot, fishery_interaction, other_human_interaction),
    ~ ifelse(. == "Y", 1, 0)
  ))

  # Step 2: Summarize counts and pivot longer
interaction_summary_narw <- interaction_narw %>%
  summarise(across(
    c(boat_collision, shot, fishery_interaction, other_human_interaction),
    ~ sum(.x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "Interaction_Type", values_to = "Count")

  # Step 3: Plot
ggplot(interaction_summary_narw, aes(x = Interaction_Type, y = Count, fill = Interaction_Type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +  # <- this adds the labels
  theme_minimal() +
  labs(
    title = "Human Interaction Types for North Atlantic Right Whales",
    x = "Interaction Type",
    y = "Number of Strandings"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

  
#----------------------Harbor Porpoise-----------------------------
#Graphing Number of Harbor Porpoise Strandings by Year
filtered_hp <- final_set %>%
  filter(genus == "Phocoena" & species == "phocoena")  # harbor porpoise

yearly_hp <- filtered_hp %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(yearly_hp, aes(x = year_of_observation, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue3") +
  labs(
    title = "Number of Harbor Porpoise Strandings",
    subtitle = "From 1996-2022",
    x = "Year of Observation",
    y = "Number of Strandings"
  )
theme_minimal()

print(yearly_hp)

#Line Plot for Harbor Porpoise Take Reduction Plan
filtered_hp <- final_set %>%
  filter(genus == "Phocoena" & species == "phocoena")  # harbor porpoise

yearly_hp <- filtered_hp %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

# Step 3: Plot
hp_graph <- ggplot(yearly_hp, aes(x = year_of_observation, y = Count)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Annual Strandings of Harbor Porpoises",
    subtitle = "Harbor Porpoises Strandings and the Years of Policies Implemented",
    x = "Year",
    y = "Number of Strandings",
  ) +
  geom_vline(xintercept = 1998, linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  1997, y = max(yearly_hp$Count) * 0.9, 
            label = "HPTRP Implemented", angle = 0, vjust = 5, size = 3) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2008, y = max(yearly_hp$Count) * 0.9, 
            label = "2010 Expansion", angle = 0, vjust = 5, size = 4) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2016, y = max(yearly_hp$Count) * 0.9, 
            label = "2013 Amendendment", angle = 0, vjust = 3, size = 4)
  theme_minimal()
hp_graph

#Number of strandings per boundary area
final_set <- final_set %>%
  mutate(
    Area = case_when(
      lat >= 42.044 ~ "Area 1|North of Cape Cod",
      lat >= 40.409 & lat < 42.044 ~ "Area 2|Cape Cod to NJ",
      lat < 40.409 ~ "Area 3|South of Highlands NJ",
      TRUE ~ NA_character_
    )
  )

final_set %>%
  filter(
    genus %in% c("Phocoena") &
      species %in% c("phocoena")
  ) %>%
  group_by(Area, Year = year_of_observation, genus, species) %>%
  summarise(Strandings = n(), .groups = "drop") %>%
  arrange(Area, Year)   #counting by area


final_set %>%
  filter(
    genus %in% c("Phocoena") &
      species %in% c("phocoena")
  ) %>%
  group_by(Area, genus, species) %>%
  summarise(Strandings = n(), .groups = "drop") %>%
  arrange(Area)   #counting by area

#3.Plotting Interaction Types for HP
interaction_hp <- final_set %>%
  filter(genus == "Phocoena", species == "phocoena") %>%
  mutate(across(
    c(boat_collision, shot, fishery_interaction, other_human_interaction),
    ~ ifelse(. == "Y", 1, 0)
  ))

# Step 2: Summarize counts and pivot longer
interaction_summary_hp <- interaction_hp %>%
  summarise(across(
    c(boat_collision, shot, fishery_interaction, other_human_interaction),
    ~ sum(.x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "Interaction_Type", values_to = "Count")

# Step 3: Plot
ggplot(interaction_summary_hp, aes(x = Interaction_Type, y = Count, fill = Interaction_Type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +  # <- this adds the labels
  theme_minimal() +
  labs(
    title = "Human Interaction Types for Harbor Porpoise",
    x = "Interaction Type",
    y = "Number of Strandings"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )


  #----------------------Atlantic Large Whales-----------------------------
#1. Graphing # of Altantic Large Whale Strandings
filtered_altrp <- final_set %>%
  filter(
    (genus =="Eubalaena"& species == "glacialis") |
      (genus == "Balaenoptera" & species == "physalus") |
      (genus == "Megaptera" & species == "novaeangliae")
  ) %>%
  filter(!is.na(year_of_observation)) %>%
  mutate(scientific_name = paste(genus, species, sep = " "))


yearly_altrp <- filtered_altrp %>%
  group_by(year_of_observation, scientific_name) %>%
  summarise(Count = n(), .groups = "drop")


ggplot(yearly_altrp, aes(x = year_of_observation, y = Count, fill = scientific_name)) +
  geom_col() +
  labs(
    title = "Number of Atlantic Large Whale Strandings",
    subtitle = "From 1996-2022",
    x = "Year of Observation",
    y = "Number of Strandings",
    fill = "Scientific Name"
  )
  theme_minimal()
#---                        ---                       ---              ---
#2. Line Plot for Atlantic Large Whale Take Reduction Plan
filtered_altrp <- final_set %>%
  filter(
    (genus =="Eubalaena"& species == "glacialis") |  #North atlantic right whale
           (genus == "Balaenoptera" & species == "physalus") |   #fin whale
         (genus == "Megaptera" & species == "novaeangliae")      #humpback whale
  ) %>%
  filter(!is.na(year_of_observation)) %>%
  mutate(scientific_name = paste(genus, species, sep = " "))


yearly_altrp <- filtered_altrp %>%
  group_by(year_of_observation, scientific_name) %>%
  summarise(Count = n(), .groups = "drop")

# Step 3: Plot
altrp_graph <- ggplot(yearly_altrp, aes(x = year_of_observation, y = Count, color = scientific_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Annual Strandings of Large Whales",
    subtitle = "Atlantic Large Whales and the Years of Policies Implemented",
    x = "Year",
    y = "Number of Strandings",
    color = "Scientific Name"
  ) +
  geom_segment(aes(x = 1997, xend= 1997, y = 0, yend = 33),
               linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  1998, y = 30, 
            label = "ALWTRP Implemented", angle = 0, vjust = -0.5, size = 3) +
  geom_segment(aes(x = 2002, xend= 2002, y = 0, yend = 33),
               linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  1999, y = 28, 
            label = "Seasonal Area Management", angle = 0, vjust = -0.5, size = 3) +
  geom_segment(aes(x = 2003, xend= 2003, y = 0, yend = 33),
               linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2005, y = 30, 
            label = "Voluntary 15-Day Closure(s)", angle = 0, vjust = -0.5, size = 2.5) +
  geom_segment(aes(x = 2007, xend= 2007, y = 0, yend = 33),
               linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2010, y = 30, 
            label = "2007 Amendment to Plan", angle = 0, vjust = -0.5, size = 3) +
  geom_segment(aes(x = 2014, xend= 2014, y = 0, yend = 8),
               linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2012, y = 28, 
            label = "2014 Amendment to Plan", angle = 0, vjust = -0.5, size = 3) +
  geom_segment(aes(x = 2015, xend= 2015, y = 0, yend = 33),
               linetype = "dashed", color = "red", linewidth = 1) +
  annotate( "text", x =  2017, y = 33, 
            label = "2015 Amendment to Plan", angle = 0, vjust = -0.5, size = 3) +
  theme_minimal()
altrp_graph


#---Number of strandings per boundary area--- --- --- --- ---
final_set <- final_set %>%
  mutate(
    Area = case_when(
      lat >= 42.044 ~ "Area 1|North of Cape Cod",
      lat >= 40.409 & lat < 42.044 ~ "Area 2|Cape Cod to NJ",
      lat < 40.409 ~ "Area 3|South of Highlands NJ",
      TRUE ~ NA_character_
    )
  )

final_set %>%
  filter(
    genus %in% c("Eubalaena", "Balaenoptera", "Megaptera") &
      species %in% c("glacialis", "physalus", "novaeangliae")
  ) %>%
  group_by(Area, Year = year_of_observation, genus, species) %>%
  summarise(Strandings = n(), .groups = "drop") %>%
  arrange(Area, Year)   #counting by area


final_set %>%
  filter(
    genus %in% c("Eubalaena", "Balaenoptera", "Megaptera") &
      species %in% c("glacialis", "physalus", "novaeangliae")
  ) %>%
  group_by(Area, genus, species) %>%
  summarise(Strandings = n(), .groups = "drop") %>%
  arrange(Area)   #counting by area

#3. Plotting INteraction Types for Altantic Large Whales
interaction_alw <- final_set %>%
  filter(
  (genus =="Eubalaena"& species == "glacialis") |  #North atlantic right whale
  (genus == "Balaenoptera" & species == "physalus") |   #fin whale
  (genus == "Megaptera" & species == "novaeangliae")      #humpback whale
 ) %>%
  mutate(across(
    c(boat_collision, shot, fishery_interaction, other_human_interaction),
    ~ ifelse(. == "Y", 1, 0)
  ))

# Step 2: Summarize counts and pivot longer
interaction_summary_alw <- interaction_alw %>%
  summarise(across(
    c(boat_collision, shot, fishery_interaction, other_human_interaction),
    ~ sum(.x, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = "Interaction_Type", values_to = "Count")

# Step 3: Plot
ggplot(interaction_summary_alw, aes(x = Interaction_Type, y = Count, fill = Interaction_Type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +  # <- this adds the labels
  theme_minimal() +
  labs(
    title = "Human Interaction Types for Eubalaena glacialis",
    x = "Interaction Type",
    y = "Number of Strandings"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )






#------------------------------Interaction Types for all TRP Species------------

# Step 1: Clean and reshape data
mortality_data <- final_set %>%
  mutate(species = case_when(
    genus == "Eubalaena" & species == "glacialis" ~ "North Atlantic Right Whale",
    genus == "Balaenoptera" & species == "physalus" ~ "Fin Whale",
    genus == "Megaptera" & species == "novaeangliae" ~ "Humpback Whale",
    genus == "Phocoena" & species == "phocoena" ~ "Harbor Porpoise",
    TRUE ~ NA_character_  # ← exclude everything else
  )) %>%
  filter(!is.na(species)) %>%  # keep only renamed species (your 4 targets)
  mutate(across(
    c(boat_collision, shot, fishery_interaction, other_human_interaction),
    ~ ifelse(. == "Y", 1, 0)
  ))

# Step 1: Manually define the full lists
species_list <- c("North Atlantic Right Whale", "Fin Whale", "Humpback Whale", "Harbor Porpoise")
interaction_types <- c("boat_collision", "shot", "fishery_interaction", "other_human_interaction")

# Step 2: Count actual detections
mortality_long <- mortality_data %>%
  pivot_longer(
    cols = all_of(interaction_types),
    names_to = "Interaction_Type",
    values_to = "Detected"
  ) %>%
  filter(Detected == 1)

mortality_counts <- mortality_long %>%
  group_by(species, Interaction_Type) %>%
  summarise(Total = n(), .groups = "drop")

# Step 3: Force complete grid and fill missing combos with 0
mortality_complete <- expand_grid(
  species = species_list,
  Interaction_Type = interaction_types
) %>%
  left_join(mortality_counts, by = c("species", "Interaction_Type")) %>%
  mutate(Total = replace_na(Total, 0))

mortality_table <- mortality_complete %>%
  group_by(species) %>%
  mutate(
    Species_Total = sum(Total),
    Percent = round(100 * Total / Species_Total, 1),
    Combined = paste0(Total, " (", Percent, "%)")
  ) %>%
  ungroup()

tabel_interaction_trp <- mortality_table %>%
  select(species, Interaction_Type, Combined, Species_Total) %>%
  pivot_wider(
    names_from = Interaction_Type,
    values_from = Combined
  ) %>%
  relocate(Species_Total, .after = last_col())


# View the final table
View(table_interaction_trp)


# Step 6: Plotting 
ggplot(mortality_complete, aes(x = species, y = Total, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = Total),
    position = position_dodge(width = 0.9), #<- this adds labels to each grouped bar
    vjust = -0.5,
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "Total Mortalities by Interaction Type and Species (1996–2022)",
    x = "Species",
    y = "Number of Mortalities",
    fill = "Interaction Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )






