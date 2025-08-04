#Graph for NARW  and Strandings
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

#Plotting NARW strandings and Policy Year Implementation
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

  #----------------------Atlantic Large Whales-----------------------------
#Graphing # of Altantic Large Whale Strandings
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
#Line Plot for Atlantic Large Whale Take Reduction Plan
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
