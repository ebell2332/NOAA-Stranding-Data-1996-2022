Sys.setenv(SHAPE_RESTORE_SHX = "YES")
policies_data <- st_read("/Users/ebell23/Downloads/AP_Data/Tables/policy_merge09_ExportFeatures.shp", quiet = TRUE) 

st_crs(policies_data)



shape_simple <- st_simplify(policies_data, preserveTopology = TRUE, dTolerance = 100)
# Plot only the geometry
plot(st_geometry(shape_simple))

shape_simple <- st_transform(shape_simple, 4326) #change CRS to match east coast map

east_coast_map <- east_coast_map %>% #adding abbreviation column to be easier to label on the map
  mutate(state_abb = case_when(
    region == "connecticut" ~ "CT",
    region == "maryland" ~"MD",
    region == "new york" ~ "NY",
    region == "virginia" ~ "VA",
    region == "delaware" ~ "DE",
    region == "massachusetts" ~ "MA",
    region == "pennsylvania" ~ "PA",
    region == "district of columbia" ~ "DC",
    region == "new hampshire" ~ "NH",
    region == "rhode island" ~ "RI",
    region == "maine" ~ "ME", 
    region == "new jersey" ~ "NJ",
    region == "vermont" ~ "VT"
  ))

state_centroids <- east_coast_map %>%
  group_by(state_abb) %>% 
  summarise(long = mean(long), lat = mean(lat))

custom_colors1 <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40","#E3A754", "#64F626")

main_map <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
         fill = "gray85", color = "black") +
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  geom_sf(data = shape_simple, aes(fill = PolCategor)) + 
  scale_fill_manual(values = custom_colors1) +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  labs(title = "Policy categories implemented along the U.S. East Coast (1996-2022)",
       x = "Longitude", y = "Latitude", fill = "Policy Category") +
  theme(
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    plot.title = element_text(color = "black", size = 16),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10), # Axis tick labels
  ) +
  theme_gray() #makes background grey


 

#Filter PolCategory to show certain categories
inset_map <- shape_simple %>%
  filter(PolCategor %in% c("Closed Area", "MPA", "NARW-specific"))

group_colors <- c("#1E88E5","#004D40","#E3A754")

inset_map <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "gray85", color = "black") +
  geom_sf(data = inset_map, aes(fill = PolCategor)) + 
  scale_fill_manual(values = group_colors, guide = "none") +
  coord_sf(xlim = c(-76, -65.80), ylim = c(36.5, 44.5)) + #adjusts the zoom 
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  ) +
  theme_gray() #makes background grey
  

#combine both policy category maps  
library(patchwork)
pol_maps <- main_map + inset_element(inset_map, left = 0.55, right =0.98, bottom = 0.01, top = 0.40)



