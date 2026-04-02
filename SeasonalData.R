coast_str02 <- read_excel("/Users/ebell23/Downloads/update_coast_strands02.xlsx") 
unique(coast_str02$month)
# Grouping Stranding Data by Seasons----

coast_str02 <- coast_str02 %>%
  mutate(Seasons = case_when(
    month_of_observation %in% c("Mar", "Apr", "May") ~ "Spring",
    month_of_observation %in% c("Jun", "Jul", "Aug") ~ "Summer",
    month_of_observation %in% c("Sep", "Oct", "Nov") ~ "Fall",
    month_of_observation %in% c("Dec", "Jan", "Feb") ~ "Winter"
  ))

season_order <- c("Spring", "Summer", "Fall", "Winter")
coast_str02$Seasons <- factor(coast_str02$Seasons, levels = season_order)

# Grouping Stranding Data by Species----
stranding_szn <- stranding_szn %>%
  mutate(Group_species = case_when(
    Species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
    Species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))

#---------------Seasonal Trends---------------
----------------------------------------------
  ##-Grouping Species
tax_szn_counts <- coast_str02 %>%
  group_by(tax_group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

##-Grouping Species and Seasons together
seasonal_tax_counts <- coast_str02 %>%
  group_by(tax_group, Seasons) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(tax_group, Seasons, fill = list(Count = 0))
seasonal_species_counts

##. Bar Chart-SpeciesxSeasonsxCount
ggplot(seasonal_tax_counts, aes(x = Seasons, y = Count, fill = tax_group)) +
  geom_bar(stat = "identity", position = "stack") +      #Separate bar for each species group
  labs(title = "Seasonal Trend in Strandings ", x = "Season", y = "Number of Strandings", fill = "Taxonomic Group") +
  theme_minimal() +
  theme(legend.position=c("right")) +
  theme(axis.text.x=element_text(size=14,color="black")) +
  theme(axis.text.y=element_text(size=14,color="black")) +
  theme(axis.title.x=element_text(size=16)) +
  theme(axis.title.y=element_text(size=16)) +
  theme(plot.title=element_text(size=18))

#Bar Chart and Overlay lines for total and each group
total_szn <- coast_str02 %>% 
  group_by(Seasons) %>% summarise(Count = n(), .groups = "drop")

lines_df <- coast_str02 %>%
  filter(tax_group %in% c("Mysticeti", "Odontocetes", "Pinnipeds", "Unidentified")) %>%
  group_by(Seasons, tax_group) %>%
  summarise(Count = n(), .groups = "drop")

seasons_plot <- ggplot() +
  geom_bar(data = total_szn, aes(x = Seasons, y = Count),
           stat = "identity", fill = "darkgrey") +
  geom_line(data = lines_df,
            aes(x = Seasons, y = Count, color = tax_group, group = tax_group),
            linewidth = 1) +
  scale_color_manual(values = c(
    "Mysticeti" = "#f4a582",
    "Odontocetes" = "#0571b0",
    "Pinnipeds" = "red",
    "Unidentified" = "#5e3c99"
  )) + 
  labs(x = "Seasons", y = NULL, color = "Taxonomic group", subtitle = "C) Seasonal strandings") + 
  theme(
    legend.position = "none", 
    legend.text = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 13),   # X-axis title
    #axis.title.y = element_text(color = "black", size = 13),   # Y-axis title
    axis.text.x = element_text(color = "black", size = 11), # Axis tick labels
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  ) 

library(patchwork)
(yeartax_plot) / (montax_plot + seasons_plot) 


##. Line Chart-SpeciesxSeasonsxCount
 ggplot(seasonal_tax_counts, aes(x = Seasons, y = Count, color = tax_group, group = tax_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Seasonal Trends in Strandings", x = "Season", y = "Number of Strandings", color = "Taxonomic Group") +
  theme_minimal()


#ggsave(filename = "Seasonal_Species_linechart.png", plot = seasons_plot1, width = 7, height = 4, units = "in", dpi = 300)

##--Group Species, Release Status, by Season----

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