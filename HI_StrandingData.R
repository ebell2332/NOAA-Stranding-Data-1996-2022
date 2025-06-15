#stranding_data <- read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")

# -------------------------------Human Interaction --------------------
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