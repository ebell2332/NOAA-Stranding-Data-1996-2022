#Creating shapefile with filtered date while preserving the existing structure 

  #current shapefile
grids_map <- st_read("/Users/ebell23/Downloads/strand_in6_grid25x25km_.shp")


  #filtering for new shapefile
library(sf)
library(dplyr)

# Keep only the desired columns + geometry
grid_filtered <- grids_map %>%
  select(
    grid_id,
    grid_ky,
    Shape_Leng,
    Shape_Area,
    geometry
  )

# Write as a new shapefile
st_write(
  grid_filtered,
  "/Users/ebell23/Downloads/filtered_grid.shp",
  delete_dsn = TRUE
)

  #double checking that structure didnt change
st_geometry_type(grids_map)
st_crs(grids_map)


st_geometry_type(grid_filtered)
st_crs(grid_filtered)

nrow(grids_map)
nrow(grid_filtered)
colnames(grid_filtered)


ggplot(grid_filtered) +
  geom_sf(fill = NA, color = "black") +
  theme_minimal()

grids_filt_uni <- grid_filtered %>% #makes it one grid per row instead of by year
  group_by(grid_id) %>%
  slice(1) %>%
  ungroup()



  #mapping to make sure it looks the same
  #old shapefile
ggplot() +
  geom_sf(data = grids_map, fill = NA, color = "black") +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black") +
  theme_void()

  #filtered shapefile

ggplot() +
  geom_sf(data = grids_filt_uni, fill = NA, color = "black") +
  geom_sf(
  data = east_coast,
  fill = NA,
  color = "black") +
  theme_void()



  #ensuring that the maps are the same using data
odon_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    Odontocetes = sum(Odontocetes, na.rm = TRUE),
    .groups = "drop"
  )

odon_map_ct <- odon_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

odo_ct_data <- grids_filt_uni %>%
  left_join(odon_map_ct, by = "grid_id")


 ggplot() +
  geom_sf(
    data = odo_ct_data,
    aes(fill = Odontocetes),
    color = NA
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Odontocetes\nstrandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black"
  ) +
  theme_void()

