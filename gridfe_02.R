#Combining all grid model outputs into one chart

#1. Shore dist 25km

all_25_grids <- bind_rows(in25_5x25_mod, in25_5x50_mod, in25_5x100_mod,
                          in25_15x25_mod, in25_15x50_mod, in25_15x100_mod,
                          in25_25x25_mod, in25_25x50_mod, in25_25x100_mod) 

colnames(all_25_grids)

library(writexl)
write_xlsx(all_25_grids, "/Users/ebell23/Downloads/grid_models/in25_allmodels.xlsx") #inshore 25 distance all model outputs saved so I don't need to re-run, already saved

all_50_grids <- bind_rows(in50_5x25_mod, in50_5x50_mod, in50_5x100_mod,
                          in50_15x25_mod, in50_15x50_mod, in50_15x100_mod,
                          in50_25x25_mod, in50_25x50_mod, in50_25x100_mod)

write_xlsx(all_50_grids, "/Users/ebell23/Downloads/grid_models/in50_allmodels.xlsx") #already saved

all_75_grids <- bind_rows(in75_5x25_mod, in75_5x50_mod,in75_5x100_mod,
                          in75_15x25_mod, in75_15x50_mod, in75_15x100_mod,
                          in75_25x25_mod, in75_25x50_mod, in75_25x100_mod)

write_xlsx(all_75_grids, "/Users/ebell23/Downloads/grid_models/in75_allmodels.xlsx") #already saved



all_grid_combos <- bind_rows(all_25_grids, all_50_grids, all_75_grids)
write_xlsx(all_grid_combos, "/Users/ebell23/Downloads/grid_models/all_grid_combos.xlsx")


colnames(all_grid_combos)


grid_levels <- c(
  "5x5 + 25x25",
  "5x5 + 50x50",
  "5x5 + 100x100",
  "15x15 + 25x25",
  "15x15 + 50x50",
  "15x15 + 100x100",
  "25x25 + 25x25",
  "25x25 + 50x50",
  "25x25 + 100x100"
)

policy_levels <- c("1 Policy category present", "2+ Policy categories present", "Direct policy present", "Indirect policy present")

plot_df2 <- all_grid_combos %>%
  mutate(
    shore_dist = factor(shore_dist, levels = c("25", "50", "75")),
    grid_combo = factor(grid_combo, levels = grid_levels),
    term_lab = factor(term_lab, levels = rev(policy_levels)),
    fe_model = factor(fe_model, levels = c("Without Grid FE", "With Grid FE"))
  ) %>%
  mutate(
    y_base = as.numeric(term_lab),
    y_plot = ifelse(fe_model == "Without Grid FE", y_base + 0.15, y_base - 0.15)
  )


grid_modelplot <- ggplot(plot_df2, aes(x = estimate, y = term_lab, color = fe_model)) +
  geom_vline(xintercept = 0, color = "black", linetype = "longdash", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.15,
                 position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  scale_color_manual(values = c(
    "Without Grid FE" = "#2C7BE5",
    "With Grid FE"    = "#D81B60"
  )) +
  facet_grid(grid_combo ~ shore_dist) +
  labs(
    x = "Coefficient estimate",
    y = NULL,
    color = NULL
  ) +
  theme_bw() +
  theme(aspect.ratio = 0.3, #changes the space to see the coefficients better
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = 10),
    strip.text.y = element_text(angle = 0, size = 10, face = "bold"),#makes grid combo labels horizontal
    strip.placement.y = "outside", #grid combo placement
    strip.text.x = element_text(size = 14, color = "black", face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("/Users/ebell23/Downloads/gridmodel_plot.png", 
       plot = grid_modelplot, 
       width = 14, 
       height = 16, 
       units = "in", 
       dpi = 600) # Specify resolution here
