stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")


###---By Species Groups
stranding_data <- stranding_data %>%
  mutate(Group_species = case_when(
    Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
    Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))


# ----------------------------- Observation Statuses----------------------------
----------------------------------------------------------------------------
  
#. **Observation Status and Number of Strandings
  
obs_status <- stranding_data %>%
  group_by(Observation_Status) %>%
  summarise(Count = n(), .groups = "drop")
  
  
obs_totals <- obs_status %>%          # Get total count per observation status
  group_by(Observation_Status) %>%
  summarise(Total_Count = sum(Count))

#. Bar Plot --Observation Status Total by Number of Strandings
  obs_status_plot <- ggplot(obs_status, aes(x = Observation_Status, y = Count, fill = Observation_Status)) +
    geom_col() +
    geom_text(data = obs_totals, aes(x = Observation_Status, y = Total_Count, label = Total_Count),  # Adds total count labels on bars
              vjust = -0.5,        # Place above bars
              color = "black", size = 3, fontface = "bold") +  
    scale_fill_manual(values = c("coral1", "skyblue2", "brown3", "darkgreen", "darkorchid", "deeppink3")) +
    labs(title = "Distribution of Observation Statuses", x = "Observation Status", y = "Number of Strandings") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),                 # Remove x-axis text labels
          axis.ticks.x = element_blank())             # Remove x-axis tick marks
obs_status_plot  
print(obs_status_plot)
  
#ggsave(filename = "ObsStatus_Dist_Plot.png", plot = obs_status_plot, width = 5, height=4, units = "in", dpi=300)
  
##. Observation Status with Species group
obs_status <- stranding_data %>%
    group_by(Observation_Status, Group_species) %>%
    summarise(Count = n(), .groups = "drop")

#.  Bar Graph--Observation Status by Species Group
obs_group_plot <- ggplot(obs_status, aes(x = Observation_Status, y = Count, fill = Group_species)) +
    geom_col(position = "dodge") +                        
    scale_fill_manual(values = c("coral1", "skyblue2", "brown3", "darkgreen", "darkorchid", "deeppink3")) +
    labs(title = "Distribution of Observation Statuses", x = "Observation Status", y = "Number of Strandings") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank())
obs_group_plot  
  print(obs_group_plot)

  
#---------------------------Release Status-------------------
    -------------------------------------------------
##. ** Released vs Non-Released 
    
length(stranding_data$`Deemed_Healthy/Releasable_Flag`)
  unique(stranding_data$`Deemed_Healthy/Releasable_Flag`)
  
stranding_data <- stranding_data %>%
  group_by(`Deemed_Healthy/Releasable_Flag`) %>%
  summarise(Count = n(), .groups = "drop")
  
 #. --Bar Chart--Released vs Non-released Strandings 
releasable_plot  <- ggplot(stranding_data, aes(x = `Deemed_Healthy/Releasable_Flag`, y = Count, fill = `Deemed_Healthy/Releasable_Flag`)) +
  geom_col() +
  labs(title = "Number of Healthy and Released Stranded Animals", x = "Deemed Healthy and Releasable", y = "Number of Strandings") +
  theme_minimal()
  releasable_plot
  
#ggsave(filename = "Healthy_ReleasedStrandings_plot.png", plot = releasable_plot, width = 5, height=4, units = "in", dpi=300) 

#  -------BY Species Group Release Status  
  
##. By Speceis group--Species group x Releasable x Number of Strandings
  species_healthy <- stranding_data %>%
    group_by(Group_species, `Deemed_Healthy/Releasable_Flag`) %>%
    summarise(Count = n(), .groups = "drop")
  
  print(species_healthy)
 #--- Stacked Bar Plot Species Group x Releasable x Number of Strandings
  healthy_species <- ggplot(species_healthy, aes( x = Group_species, y = Count, fill = `Deemed_Healthy/Releasable_Flag`)) +
    geom_col(position = "stack") +
    labs(title = "Release Status by Species Group", x = "Species Group", y = "Number of Strandings", fill = "Releaseable") +
    theme_minimal()
  healthy_species
  
 ##--Line Plot Proportions of Releasable Strandings over Time
  species_healthy <- stranding_data %>%
    group_by(Group_species, `Deemed_Healthy/Releasable_Flag`, Year_of_Observation) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Year_of_Observation, Group_species) %>%
    mutate(Proportion = Count /sum(Count))
  
healthy_species1 <- ggplot(species_healthy, aes(x = Year_of_Observation, y = Proportion, color = `Deemed_Healthy/Releasable_Flag`, group = `Deemed_Healthy/Releasable_Flag`)) +
    geom_line(linewidth = 1) +
    facet_wrap(~Group_species) +
    labs(title = "Proportions of Releasable Strandings Over Time", x = "Year", y = "Proportion of Strandings", color = "Releasable") +
    theme_minimal()
healthy_species1


#------------------------------------Unusual Mortality Events & Mass Strandings-------------------------------------

##. Unusual Mortality Events

#-- Count rows where "UME", "Unusual Mortality Event" appears in the "GE_Type(s)_GE_Module" column
ume_count <- sum(grepl("UME|Unusual Mortality Event", stranding_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE))

# Print the total count
print(ume_count)


##. Mass Strandings

#-- Count rows where "Mass Strandings" appears in the "GE_Type(s)_GE_Module" column
mass_str_count <- sum(grepl("Mass Stranding", stranding_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE))

# Print the total count
print(mass_str_count)



##. Graphing Both Types from GE_Type(s)_GE_Module

# Identify and count UMEs and Mass Strandings
stranding_data$Event_Type <- "Other"  # Default category

# Classify events (you can add more patterns if needed)
stranding_data$Event_Type[grepl("UME|Unusual Mortality Event", stranding_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE)] <- "UME"
stranding_data$Event_Type[grepl("Mass Stranding", stranding_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE)] <- "Mass Stranding"

# Summarize counts
event_counts <- as.data.frame(table(stranding_data$Event_Type))

# Create a bar chart
ggplot(event_counts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Occurrences of UME and Mass Strandings",
       x = "Event Type",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")


#----------------------------------------Entanglement Count--------------------------------------


#--------------------------------------Ingestion-------------------------------

