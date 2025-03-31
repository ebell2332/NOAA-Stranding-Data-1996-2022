stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")

#Organizing Months into correct format and order----
##Putting Months as a factor and changing labels 
stranding_data$Month_of_Observation = factor(stranding_data$Month_of_Observation, levels=c("JAN", "FEB", "MAR", "APR","MAY",
                                                                                           "JUN", "JUL", "AUG","SEP", "OCT","NOV", "DEC"),
                                             labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                                       "Oct", "Nov", "Dec")) #levels = right order, labels = how it should be labeled

###-----Defining Group Species
stranding_data <- stranding_data %>%
  mutate(Group_species = case_when(
    Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
    Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))
#--------------------------------------------------------------------------------------
  -------------------------------------------------------------------------------------
#Graphing Specific Year & State by Observation Month----

#. Grouping the data together
maine_species_2018 <- stranding_data %>%
  filter(State == "ME" & Year_of_Observation == 2018) %>%
  group_by(Group_species, Month_of_Observation) %>%
  summarise(Count = n(), .groups = "drop")
maine_species_2018

#. --Bar Graph--Month
ggplot(maine_species_2018, aes(x = Month_of_Observation, fill = Group_species)) +
  geom_bar(stat = "identity", aes(y = Count), position = "dodge") +
  theme_minimal() +
  labs(title = "Number of Strandings in Maine (2018) Each Month by Species Group",
       x = "Month of Observation",
       y = "Number of Strandings",
       fill = "Species Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#. Plotting by Month--Line Graph
maine_species_month2018 <- ggplot(maine_species_2018, aes(x = Month_of_Observation, y = Count, color = Group_species)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(aes(group = Group_species), linewidth = 1) + 
  theme_minimal() +
  labs(title = "Number of Strandings in Maine (2018) Each Month by Species Group",
       x = "Month of Observation",
       y = "Number of Strandings",
       color = "Species Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "Maine_species_2018-lineplot.png", plot = maine_species_month2018, width = 5, height = 5, units = "in", dpi = 300)