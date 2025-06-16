#final_clean <- read.csv(file = "/Users/ebell23/Downloads/final_clean.csv")

#Boxplot
# Define theme and colors first
wes_darj <- c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6", "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE")

statz_ggplot <- theme(
  legend.position = "none",
  axis.text.x = element_text(size = 14, color = "gray30"),
  axis.text.y = element_text(size = 14, color = "gray30"),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  plot.title = element_text(size = 18),
  strip.text.x = element_text(size = 16, colour = "black")
)

# -------------------------------Human Interaction (HI) --------------------
-------------------------------------------------------------------
  #. **Findings of Human Interactions

#Adding Interaction Type into dataset by combining columns  
  hi_data <- final_clean %>%
    select(shot, boat_collision, fishery_interaction, other_human_interaction) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Interaction_Type",
      values_to = "Findings_of_Human_Interaction"
    ) %>%
    filter(!is.na(Findings_of_Human_Interaction)) %>%
    group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
    summarise(Count = n(), .groups = "drop")
  
  HI_graph <- ggplot(data = hi_data, aes(x = Findings_of_Human_Interaction, y = Count, fill = Interaction_Type)) +
    geom_col(alpha = 0.4) +
    ggtitle("Findings of Human Interaction from 1996–2022") +
    labs(x = "Findings of Human Interaction", y = "# of Strandings") +
    theme(legend.position = "right",
          axis.text.x = element_text(size = 14, color = "gray30"),
          axis.text.y = element_text(size = 14, color = "gray30"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 18),
          strip.text.x = element_text(size = 16, colour = "black"))
HI_graph  
  
#. HI vs Non-HI Findings Grouping
plot_data <- all_strandings %>%
    group_by(Findings_of_Human_Interaction) %>%
    summarise(Count = sum(Count), .groups = "drop")
plot_data

#. Bar Chart---Findings of HI vs Non-HI
  HI_non_HI_plot <- ggplot(plot_data, aes(x = Findings_of_Human_Interaction, y = Count, fill = Findings_of_Human_Interaction)) +
    geom_col() +
    geom_text(aes(label = Count), 
              vjust = -0.5,        # Place above bars
              color = "black", size = 5, fontface = "bold") + 
    scale_fill_manual(values = c("red2", "skyblue3", "darkgreen")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +  # Adds space at the top
    labs(title = "Number of Strandings: HI vs Non-HI", x = "Findings of HI", y = "Number of Strandings") +
    theme_minimal()
  HI_non_HI_plot
#ggsave(filename = "HI_nonHI_barchart.png", plot = HI_non_HI_plot, width = 8, height = 6, units = "in", dpi = 300)
  
##--Summary Table of Hi vs Non HI  
summary_table <- plot_data %>%
    group_by(Findings_of_Human_Interaction) %>%
    summarise(Total_Count = sum(Count), .groups = "drop" )
  summary_table
  
#. Stacked Bar Graph--Types of Human Interactons 
HI_graph <- ggplot(data = hi_data, aes(x = Findings_of_Human_Interaction, y = Count, fill = Interaction_Type)) + #simple way to do it
    geom_col(alpha = 0.4) +
    ggtitle("Findings of Human Interaction from 1996-2022") +
    labs(x = "Findings of Human Interaction", y = "# of Strandings")  + 
    theme(legend.position=c("right")) +
    theme(axis.text.x=element_text(size=14,color="gray30")) +
    theme(axis.text.y=element_text(size=14,color="gray30")) +
    theme(axis.title.x=element_text(size=16)) +
    theme(axis.title.y=element_text(size=16)) +
    theme(plot.title=element_text(size=18)) +
    theme(strip.text.x = element_text(size = 16, colour = "black"))
HI_graph 
  #ggsave("PLOT_HI_Findings-histo.png", plot = HI_graph, width=5, height=4, units="in", dpi=300)
  
#. Bar graph for Types of Human Interaction, separate graphs for each type
HI_graph1 <- ggplot(data = hi_data, aes(x = Findings_of_Human_Interaction, y = Count, fill = Interaction_Type)) + #simple way to do it
    geom_col(alpha = 0.4) +
    ggtitle("Findings of Human Interaction from 1996-2022") +
    labs(x = "Findings of Human Interaction", y = "# of Strandings") +
    facet_wrap(~Interaction_Type, scales = "free") 
  HI_graph1
  print(HI_graph1)
  
#ggsave(filename = "HI_Findings_facet-histo.png", plot = HI_graph1, width=5, height=4, units="in", dpi=300)
  
##--Summary Table
summary_table <- all_strandings %>%
    group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
    summarise(Total_Count = sum(Count), .groups = "drop")
summary_table

################################################################################
##Human Interaction Stranding Cases By State Trends ###
#Filter Data Set to add all HI cases into one column
hi_data <- final_clean %>%
  select(year_of_observation, state, shot, boat_collision, fishery_interaction, other_human_interaction) %>%  #add state & year of observation because of plotting 
  pivot_longer(
    cols = c(shot, boat_collision, fishery_interaction, other_human_interaction),
    names_to = "Interaction_Type",
    values_to = "Findings_of_Human_Interaction"
  ) %>%
  filter(!is.na(Findings_of_Human_Interaction))

#### Massachussetts Trends ####
#Number of Strandings during 2017 - MA
hi_2017_ma <- hi_data %>%
  filter(year_of_observation == 2017, state == "MA", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2017_ma, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in Massachusetts (2017)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Number of Strandings during 2018 - MA
hi_2018_ma <- hi_data %>%
  filter(year_of_observation == 2018, state == "MA", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2018_ma, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in Massachusetts (2018)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Number of Strandings during 2019 - MA
hi_2019_ma <- hi_data %>%
  filter(year_of_observation == 2019, state == "MA", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2019_ma, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in Massachusetts (2019)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Bar Plot all 3 years for MA
hi_multi_year_ma <- hi_data %>%
  filter(year_of_observation %in% c(2017, 2018, 2019), state == "MA", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(year_of_observation, Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_multi_year_ma, aes(x = factor(year_of_observation), y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Findings_of_Human_Interaction) +   #grouped bar chart (years on x-axis)
  labs(
    title = "Findings of Human Interaction in Massachusetts (2017–2019)",
    x = "Year", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Box Plot 
  #MA strandings 2017-2019
interaction_counts_ma <- hi_data %>%
  filter(year_of_observation %in% c(2017, 2018, 2019),
         state == "MA", # filter to MA
         Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(year_of_observation, Interaction_Type) %>%
  summarise(count = n(), .groups = "drop")

  # Now build the boxplot
ggplot(interaction_counts_ma, aes(x = count, y = Interaction_Type)) +
  geom_boxplot(aes(fill = Interaction_Type), color = "black", outlier.shape = NA) +  # color outlines, suppress outliers
  geom_jitter(width = 0.25, height = 0, size = 2.5, alpha = 0.7, color = "darkblue") +
  scale_fill_manual(values = wes_darj) +
  labs(
    title = "Distribution of Strandings by Interaction Type",
    subtitle = "Massachusetts (2017-2019)",
    x = "Number of Strandings",
    y = "Interaction Type"
  ) +
  theme_minimal() +
  statz_ggplot


### Maine Trends #####
#Number of Strandings during 2005 - ME
hi_2005_me <- hi_data %>%
  filter(year_of_observation == 2005, state == "ME", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2005_me, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in Maine (2005)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Number of Strandings during 2006 - ME
hi_2006_me <- hi_data %>%
  filter(year_of_observation == 2006, state == "ME", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2005_me, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in Maine (2006)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Number of Strandings during 2018 - ME
hi_2018_me <- hi_data %>%
  filter(year_of_observation == 2018, state == "ME", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2005_me, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in Maine (2018)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Plot all 3 years for ME
  #Bar Plot
hi_multi_year_me <- hi_data %>%
  filter(year_of_observation %in% c(2005, 2006, 2018), state == "ME", Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(year_of_observation, Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_multi_year_me, aes(x = factor(year_of_observation), y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Findings_of_Human_Interaction) +   #grouped bar chart (years on x-axis)
  labs(
    title = "Findings of Human Interaction in Maine (2005, 2006, 2018)",
    x = "Year", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#BoxPlot
  #ME Strandings 2005, 2006, 2018
interaction_counts_me <- hi_data %>%
  filter(year_of_observation %in% c(2005, 2006, 2018),
         state == "ME", # filter to ME
         Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  group_by(year_of_observation, Interaction_Type) %>%
  summarise(count = n(), .groups = "drop")

  # Now build the boxplot
ggplot(interaction_counts_me, aes(x = count, y = Interaction_Type)) +
  geom_boxplot(aes(fill = Interaction_Type), color = "black", outlier.shape = NA) +  # color outlines, suppress outliers
  geom_jitter(width = 0.25, height = 0, size = 2.5, alpha = 0.7, color = "darkblue") +
  scale_fill_manual(values = wes_darj) +
  labs(
    title = "Distribution of Strandings by Interaction Type",
    subtitle = "Maine (2005, 2006, 2018)",
    x = "Number of Strandings",
    y = "Interaction Type"
  ) +
  theme_minimal() +
  statz_ggplot

### Excluding MA and ME Trends #######

#Number of Strandings during 2013 - Everywhere else
hi_2013 <- hi_data %>%
  filter(year_of_observation == 2013, Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  filter(!(state %in% c("MA", "ME"))) %>% 
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2013, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in 2013",
    subtitle = "(Excluding Maine and Massachusetts)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Number of Strandings during 2018 - Everywhere else
hi_2018 <- hi_data %>%
  filter(year_of_observation == 2018, Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  filter(!(state %in% c("MA", "ME"))) %>% 
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2018, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in 2018",
    subtitle = "(Excluding Maine and Massachusetts)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Number of Strandings during 2019 - Everywhere else
hi_2019 <- hi_data %>%
  filter(year_of_observation == 2019, Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  filter(!(state %in% c("MA", "ME"))) %>% 
  group_by(Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")

ggplot(hi_2019, aes(x = Findings_of_Human_Interaction, y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Findings of Human Interaction in 2019",
    subtitle = "(Excluding Maine and Massachusetts)",
    x = "Finding of Human Interaction", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

#Plotting all 3 years together (excluding MA and ME)

  #Bar Chart Colors Separating each Interaction Type
hi_multi_year <- hi_data %>%
  filter(year_of_observation %in% c(2013, 2018, 2019), Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  filter(!(state %in% c("MA", "ME"))) %>% 
  group_by(year_of_observation, Interaction_Type, Findings_of_Human_Interaction) %>%
  summarise(count = n(), .groups = "drop")


ggplot(hi_multi_year, aes(x = factor(year_of_observation), y = count, fill = Interaction_Type)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Findings_of_Human_Interaction) +   #grouped bar chart (years on x-axis)
  labs(
    title = "Findings of Human Interaction in 2013, 2018-2019",
    subtitle = "(Excluding Maine and Massachusetts)",
    x = "Year", y = "Number of Strandings",
    fill = "Interaction Type"
  ) +
  theme_minimal()

 #Box Plot
  #All other Strandings excluing ME/MA 2013, 2018-2019
interaction_counts_allyrs <- hi_data %>%
  filter(year_of_observation %in% c(2013, 2018, 2019), Findings_of_Human_Interaction %in% c("Yes", "Y")) %>%
  filter(!(state %in% c("MA", "ME"))) %>% 
  group_by(year_of_observation, Interaction_Type) %>%
  summarise(count = n(), .groups = "drop")

  # Now build the boxplot
ggplot(interaction_counts_allyrs, aes(x = count, y = Interaction_Type)) +
  geom_boxplot(aes(fill = Interaction_Type), color = "black", outlier.shape = NA) +  # color outlines, suppress outliers
  geom_jitter(width = 0.25, height = 0, size = 2.5, alpha = 0.7, color = "darkblue") +
  scale_fill_manual(values = wes_darj) +
  labs(
    title = "Distribution of Strandings by Interaction Type",
    subtitle = "Excluding Maine and Massachusetts (2005, 2006, 2018)",
    x = "Number of Strandings",
    y = "Interaction Type"
  ) +
  theme_minimal() +
  statz_ggplot








