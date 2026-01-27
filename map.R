Sys.setenv(SHAPE_RESTORE_SHX = "YES")
policies_data <- st_read("/Users/ebell23/Downloads/policy_merge09_ExportFeatures.shp", quiet = TRUE) 

st_crs(policies_data)



shape_simple <- st_simplify(policies_data, preserveTopology = TRUE, dTolerance = 100)
# Plot only the geometry
plot(st_geometry(shape_simple))

shape_simple <- st_transform(shape_simple, 4326) #change CRS to match east coast map


ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
         fill = "gray85", color = "black") +
  geom_sf(data = shape_simple, aes(fill = PolCategor)) + 
  coord_sf() +
  theme_minimal() +
  labs(title = "Policy categories implemented along the U.S. East Coast (1996-2022)",
       x = "Longitude", y = "Latitude", color = "Policy Category") +
  theme(
    plot.title = element_text(color = "black", size = 16),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10), # Axis tick labels
  ) +
  theme_gray() #makes background grey


 
ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "gray85", color = "black", inherit.aes = FALSE) +
  geom_sf(data = shape_simple, fill = "steelblue", color = "black") +
  facet_wrap(~ PolCategory) +
  coord_sf() +
  theme_minimal() +
  labs(title = "Policies implemented along the U.S. East Coast (1996-2022)",
       x = "Longitude", y = "Latitude") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(color = "black", size = 16),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10), # Axis tick labels
  )


