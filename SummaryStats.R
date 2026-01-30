#Stranding Summary Statistics#

#final_set <- read_excel(file = "/Users/ebell23/Downloads/total_strand.xlsx") #new dataset with datapoints from NC and Canada removed
 

####1. Species####
final_set %>%
    count(species)  #Total Number of Strandings Individual Species

####2. Group Species####
final_set <- final_set %>%
  mutate(Group_species = case_when(
    species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaeangliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena", "mirus", "leucas", "cruciger") ~ "Cetaceans",
    species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))

final_set %>%
  count(Group_species) #Total Number of Strandings per Group Species

group_species_counts <- final_set %>%
  group_by(Group_species, year_of_observation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100) #calculates percentage


group_name <- ggplot(group_species_counts, aes(x = Group_species, y = Count)) +
  geom_col() +
  #geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  labs(title = "Total Number of Strandings by Species Group",
       x = "Species Group", 
       y = "Total Number of Observed Strandings") +
  theme(legend.position = "none")  #remove legend entirely
# Remove all grid lines
group_name + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())



ggplot(group_species_counts, aes(x = year_of_observation, y = Count, color = Group_species, group = Group_species)) +
  geom_line() +
  #geom_line(aes(linetype = Group_species), linewidth = 1.2) + # Different linetypes
  #geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  labs(title = "Yearly Strandings Observed by Species Group", 
       x = "Year of Observation", 
       y = "Number of Strandings",
       color = "Species Group") +
  theme_minimal()

####3. State####
final_set %>%
    count(state)

state_count <- final_set %>%
  group_by(state) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100) #calculates percentage 
print(state_count)

state_strandings <- ggplot(state_count, aes(x = state, y = Count)) +
  geom_col() +
  labs(title = "Total Number of Strandings by State", x = "State", y = "Total Number of Observed Strandings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
state_strandings
####4. Yearly ####

final_set %>%
    count(year_of_observation)

yearly_str <- final_set %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop") #get the count of strandings per year

yr_mean_str <- round(mean(yearly_str$Count), 2) #calculate the average number of strandings per year
 
#4a. graph the yearly observations by number of strandings
yr_stranding <- ggplot(yearly_str, aes(x = year_of_observation, y = Count)) +
  geom_line(linewidth = 1) +
  #geom_hline(yintercept = yr_mean_str, color = "darkred", linetype = "dashed", linewidth = 1 ) +
   labs(title = "Number of Strandings Observed Yearly", 
       x = "Year Observed", y = "Number of Strandings") +
  theme_minimal()

yr_stranding + annotate("text", x = 1996, y = 1250, label = "1198.22", color = "black", size = 3) #add the average number of strandings to a specific x,y spot on the graph


#4b. graph the total yearly observations by number of strandings overlayed with species group yearly strandings
 
#Turn total counts into total group
total_long <- yearly_str %>%
  mutate(
    Group_species = "Total"   # label series
  ) %>%
  rename(year_of_observation = year_of_observation,
         Count = Count)       # keep names consistent

#Combing yearly stranding and group stranding sets
plot_df <- bind_rows(
  total_long %>% select(year_of_observation, Group_species, Count),
  group_species_counts %>% select(year_of_observation, Group_species, Count)
)

#Plot together
ggplot(plot_df,
       aes(x = year_of_observation,
           y = Count,
           color = Group_species,
           group = Group_species)) +
  geom_line()+
  labs(
    title = "Yearly Strandings: Total and by Species Group",
    subtitle = "1996-2022",
    x = "Year of Observation",
    y = "Number of Strandings",
    color = "Stranding Category"
  ) +
  theme_minimal()



####5. Taxonomic group####
#Strandings by year and taxonomic group overalyed together

total <- final_set %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

pin <- final_set %>%
  filter(tax_group == "Pinnipeds") %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

mys <- final_set %>%
  filter(tax_group == "Mysticeti") %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

odo <- final_set %>%
  filter(tax_group == "Odontocetes") %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

uni <- final_set %>%
  filter(tax_group == "Unidentified") %>%
  group_by(year_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

ggplot()+
  geom_bar(data = total, aes(x=year_of_observation, y = Count), stat = "identity", fill = "lightgrey") +
  geom_line(data = pin, aes(x=year_of_observation, y = Count, color = "steelblue"), linewidth = 1)+
  geom_line(data = mys, aes(x=year_of_observation, y = Count, color = "red"), linewidth = 1)+
  geom_line(data = odo, aes(x=year_of_observation, y = Count, color = "darkgreen"), linewidth = 1)+
  geom_line(data = uni, aes(x=year_of_observation, y = Count, color = "grey4"), linewidth = 1)+
  scale_color_identity(name = "Taxonomic Group",
                       breaks = c("steelblue", "red", "darkgreen", "grey4"),
                       labels = c("Pinnipeds", "Mysticeti", "Odontocetes", "Unidentified"),
                       guide = "legend") +
  labs(x = "Year of Observation", y = "Total No. of Strandings", title = "Total Number of Strandings Annually by Taxonomic Group")+
  theme_minimal()

#2. Total Counts for each taxonomic group
tax_species_counts <- final_set %>%
  group_by(tax_group) %>%
  summarise(Count = n(), .groups = "drop") 

custom_colors <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40") #creating color-blind friendly pallete for strandings

tax_count <- ggplot(tax_species_counts, aes(x = tax_group, y = Count, fill = tax_group)) +
  geom_col() +
  scale_fill_manual(values = custom_colors, labels = c("Mysticeti (baleen whales)", "Odontocetes (toothed whales)","Pinnipeds (seals/sea lions)", "Unidentified")) + #assign colors to each taxonomic group and change the labels for the legend
  geom_text(aes(label = Count), vjust = -0.5, size = 3) + #adds count to bars
  labs(
       x = "Taxonomic Group", 
       y = "Number of Observed Strandings", fill = "Taxonomic Group") +
  theme(legend.position = "none",
        legend.text = element_text(size = 8), 
        #plot.title = element_text(color = "black", size = 10),      # Title color and size
        axis.title.x = element_text(color = "black", size = 7),   # X-axis title
        axis.title.y = element_text(color = "black", size = 7),   # Y-axis title
        axis.text = element_text(size = 0), # Axis tick labels
        #legend.key.size = unit(0.3, "cm") #legend size
  )
   
#combine map and chart
library(patchwork)
(tax_map + tax_count) + plot_layout(ncol =1)

tax_map + inset_element(tax_count, left = 0.55, right =0.98, bottom = 0.01, top = 0.40) #inserted the bar chart in the bottom right corner of the map

#----Monthly Strandings----####
final_set %>%
  count(month_of_observation)

#convert to numberical order instead of alphabetical order
final_set$month_of_observation <- factor(
  final_set$month_of_observation,
  levels = c("JAN","FEB","MAR","APR","MAY","JUN",
             "JUL","AUG","SEP","OCT","NOV","DEC"),
  ordered = TRUE
) 

month_str <- final_set %>%
  group_by(month_of_observation) %>%
  summarise(Count = n(), .groups = "drop") #get the count of strandings per month

ggplot(month_str, aes(x = month_of_observation, y = Count)) + 
  geom_col(fill = "steelblue") +
  labs(title = "Number of Strandings Observed Monthly", 
       x = "Month of Observation", y = "Number of Strandings") +
  theme_minimal()

#Monthly Stranding Count by Group Species
monthly_counts <- final_set %>%
  group_by(Group_species, month_of_observation) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(monthly_counts, aes(x = month_of_observation, y = Count, color = Group_species, group = Group_species))+
  geom_line(linewidth = 2) +
  labs(x = "Month of Observation", y = "Number of Strandings", title = "Number of Strandings Observed Monthly by Species Group", color = "Species Group") +
  theme_minimal()


#Policy Summary Statistics#####
pol_data <- read_excel(path = "/Users/ebell23/Downloads/policy_09.xlsx")

Sys.setenv(SHAPE_RESTORE_SHX = "YES")
policies_data <- st_read("/Users/ebell23/Downloads/policy_merge09_ExportFeatures.shp", quiet = TRUE) 

#1. Sum of each policy category within target
length(unique(pol_data$PolicType))

policies_data %>%
  count(PolCategor) #count how many records per category


pol_cat_count <- policies_data %>%
  group_by(PolCategor, Target) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100) #calculates percentage
print(pol_cat_count)


ggplot(pol_cat_count, aes(x = PolCategor, y = Count, fill = Target)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = Count), vjust = -0.5) + #adds count to bars
  labs(title = "Policy counts by target and category",
       x = "Policy Category", 
       y = "Number of Policies",
       fill = "Policy Target") +
  theme(
    legend.text = element_text(size = 11), 
    legend.position = "bottom",
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10, color = "black")      # Axis tick labels
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust =1))



#2. Number of Policies by Year
pol_year <- pol_data %>%
  group_by(Year) %>%
  summarise(Count = n(), .groups = "drop")


pol_yr <- ggplot(pol_year, aes(x = Year, y = Count)) +
  geom_col() +
  labs(title = "Number of Polices Implemented Annually",
       x = "Implementation Year", 
       y = "Number of Policies") +
  theme(legend.position = "none")  #remove legend entirely
# Remove all grid lines
pol_yr + theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank())

ggplot(pol_year, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") + #add color thats not black
  labs(title = "Number of Polices Implemented Annually",
       x = "Implementation Year", 
       y = "Number of Policies") +
  theme(legend.position = "none")  

#2a. Number of Policies by Year and Type
pol_year_ty <- pol_data %>%
  group_by(Year, PolicType) %>%
  summarise(Count = n(), .groups = "drop")


ggplot(pol_year_ty, aes(x = Year, y = Count, fill = PolicType)) +
  geom_col() +
  labs(title = "Number of Polices Implemented Annually",
       x = "Implementation Year", 
       fill = "Policy Type",
       y = "Number of Policies") +
  theme_minimal()



#3. Policy Type by Total Area
plot_data <- pol_data %>%
  group_by(PolicType) %>%
  summarise(total_area = sum(Area_km2, na.rm = TRUE))

ggplot(plot_data, aes(x = reorder(PolicType, -total_area), 
                      y = total_area)) +
  geom_col() +
  labs(x = "Policy Type",
       y = "Total Area (sq km)",
       title = "Total Area by Policy Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Policy Type and Year by Square Km
plot_data2 <- pol_data %>%
  group_by(PolicType, Year) %>%
  summarise(total_area = sum(Area_km2, na.rm = TRUE), .groups = "drop")

#a.Stacked bar chart over time
ggplot(plot_data2, aes(x = factor(Year), y = total_area,fill = PolicType)) +
  geom_col() +
  labs(
    x = "Active Year",
    y = "Total Area (sq km)",
    fill = "Policy Type",
    title = "Total Policy Area (sq km) by Active Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#b.Stacked area chart over time
ggplot(plot_data2, aes(x = Year, y = total_area, fill = PolicType)) +
  geom_area() +
  labs(
    x = "Year",
    y = "Total Area (sq km)",
    fill = "Policy Type",
    title = "Total Policy Area (sq km) by Category Over Time"
  ) +
  theme_minimal() 


