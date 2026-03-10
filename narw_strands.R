#NARW Strandings and Policy Coverage with NARW-specific policies
  #Packages Needed
library(stringr)

#Read in datasets
narw_strands <- read_excel("/Users/ebell23/Downloads/narw_strandsTable.xlsx")
narw_str <- narw_strands %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) #keeping all stranding years

length(unique(narw_strands$OBJECTID)) #counts all the unique objectID = 47 total narw strandiings

grid_cells <- st_read("/Users/ebell23/Downloads/grid_25kmClip_ExportFeatures.shp", quiet = TRUE)
#grid_fixed <- st_set_crs(grid_cells, 5070) 
grid <- st_transform(grid_cells, 4326)

#grid_id x year
grid_str <- read_excel("/Users/ebell23/Downloads/grid25_strand_pol_all.xlsx")
sum(tapply(grid_str$n_strandings, grid_str$grid_id, sum) == 0) #total number of grid_ids where stranding count = 0 

#Map only NARW Strandings
#1. NARW strandings

narw <- narw_strands %>%
  group_by(year_of_observation, lat, lon) %>%
  summarise(Count = n(), .groups = "drop")

narw_map <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "gray85", color = "black") +
  geom_point(data = narw, aes(x = lon, y = lat), size = 2, color = "black") +
  labs(title = "North Atlantic right whale strandings", x = "Longitude (°W)", y = "Latitude(°N)") +
  theme(
    axis.title = element_text(size = 17, color = "black"),
    axis.text = element_text(size = 15, color = "black"),
    plot.title = element_text(size = 20, color = "black"),
    panel.grid = element_blank()
  ) 
   

#Filter Policy datasets to specific years for NARW-policies and map individually----
#1996
yr_keep <- 1996

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_narw_1996 <- grid_pol_year %>%
  filter(Year_num == yr_keep) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "NARW-specific")), .groups = "drop")

grid_narw_1996 <- grid %>%
  left_join(pol_narw_1996, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid7 <- ggplot() +
  geom_sf(data = grid_narw_1996, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_narw_1996 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = narw_str, size = 0.9, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  scale_y_continuous(breaks = seq(38, 46, by = 4), labels = scales::label_number(suffix = "°N")) +
  labs(
    subtitle = "1996",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 17, color = "black"),
        axis.title = element_text(color = "black", size = 17),   # Axis title
        axis.text  = element_text(size = 15, color = "black"),      # Axis tick labels
  ) 



#2009
yr_keep1 <- 2009
cat_keep <- "NARW-specific"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_narw_2009 <- grid_pol_year %>%
  filter(Year_num == yr_keep1) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "NARW-specific")), .groups = "drop")

grid_narw_2009 <- grid %>%
  left_join(pol_narw_2009, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid8 <- ggplot() +
  geom_sf(data = grid_narw_2009, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_narw_2009 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = narw_str, size = 0.9, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    subtitle = "2009",
    x = "Longitude" , y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(size = 17, color = "black"),
    axis.title.x = element_text(color = "black", size = 17),   # Axis title
    axis.text.x  = element_text(size = 15, color = "black"),
    axis.title.y = element_blank(),   # Y-axis title
    axis.text.y  = element_blank(),      # Axis tick labels
    axis.ticks.y = element_blank(),
  ) 


#2022
yr_keep2 <- 2022
cat_keep <- "NARW-specific"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_narw_2022 <- grid_pol_year %>%
  filter(Year_num == yr_keep2) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "NARW-specific")), .groups = "drop")

grid_narw_2022 <- grid %>%
  left_join(pol_narw_2022, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid9 <- ggplot() +
  geom_sf(data = grid_narw_2022, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_narw_2022 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = narw_str, size = 0.9, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    subtitle = "2022",
    x = "Longitude" , y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(size = 17, color = "black"),
    axis.title.x = element_text(color = "black", size = 17),   # Axis title
    axis.text.x  = element_text(size = 15, color = "black"),
    axis.title.y = element_blank(),   # Y-axis title
    axis.text.y  = element_blank(),      # Axis tick labels
    axis.ticks.y = element_blank(),
  ) 


library(patchwork)
(grid7 + grid8 + grid9) + plot_annotation(
  title = 'Strandings in active NARW-specific policy areas',
  theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
)


#Filter Stranding and Policy datasets to specific years for NARW-policies and map individually----
#1996
yr_keep <- 1996
str_1996 <- narw_str %>%
  filter(Year_num == 1996)

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_narw_1996 <- grid_pol_year %>%
  filter(Year_num == yr_keep) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "NARW-specific")), .groups = "drop")

grid_narw_1996 <- grid %>%
  left_join(pol_narw_1996, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

na_96 <- ggplot() +
  geom_sf(data = grid_narw_1996, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_narw_1996 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = str_1996, size = 0.9, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  scale_y_continuous(breaks = seq(38, 46, by = 4), labels = scales::label_number(suffix = "°N")) +
  labs(
    subtitle = "1996",
    x = NULL, y = "Latitude"
  ) +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 17, color = "black"),
        axis.title.x = element_blank(),   # X-axis title
        axis.title.y = element_text(color = "black", size = 17),   # Y-axis title
        axis.text.y  = element_text(size = 15, color = "black"),      # Axis tick labels
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()
  ) 



#2009
yr_keep1 <- 2009
str_2009 <- narw_str %>%
  filter(Year_num == 2009)

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_narw_2009 <- grid_pol_year %>%
  filter(Year_num == yr_keep1) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "NARW-specific")), .groups = "drop")

grid_narw_2009 <- grid %>%
  left_join(pol_narw_2009, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

na_09 <- ggplot() +
  geom_sf(data = grid_narw_2009, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_narw_2009 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = str_2009, size = 0.9, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    subtitle = "2009",
    x = NULL , y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(size = 17, color = "black"),
    axis.title.x = element_blank(),   # X-axis title
    axis.title.y = element_blank(),   # Y-axis title
    axis.text.y  = element_blank(),      # Axis tick labels
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) 


#2022
yr_keep2 <- 2022
str_22 <- narw_str %>%
  filter(Year_num == 2022)

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_narw_2022 <- grid_pol_year %>%
  filter(Year_num == yr_keep2) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "NARW-specific")), .groups = "drop")

grid_narw_2022 <- grid %>%
  left_join(pol_narw_2022, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

na_22 <- ggplot() +
  geom_sf(data = grid_narw_2022, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_narw_2022 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = str_22, size = 0.9, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    subtitle = "2022",
    x = NULL , y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = element_text(size = 17, color = "black"),
    axis.title.x = element_blank(),   # X-axis title
    axis.title.y = element_blank(),   # Y-axis title
    axis.text.y  = element_blank(),      # Axis tick labels
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  ) 

library(patchwork)
(na_96 + na_09 + na_22)