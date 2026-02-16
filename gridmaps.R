#Mapping Strandings Overlayed with Policy Categories and Grid Cells
  #------------load packages
library(readxl) #reading in excel files
library(dplyr) #data manipulation with data frames and tables
library(ggplot2) #visual plots
library(tidyr) #create tidy data
library(lubridate) #manipulating and parsing date-time data
library(ggrepel) # provides geoms to automaticalls position overlapping text and labels
library(vegan) #ecological functions for diversity analysis
library(sf) #spatial vector data like polygons, points, and lines
library(tibble)

#Read in Data----
  #strandings
coast_str <- read_excel(path = "/Users/ebell23/Downloads/coastal_strandings.xlsx")  #final dataset withour error datapoints
  #policies
policies_data <- st_read("/Users/ebell23/Downloads/AP_Data/Tables/policy_merge09_ExportFeatures.shp", quiet = TRUE) 
shape_simple <- st_simplify(policies_data, preserveTopology = TRUE, dTolerance = 100)
shape_simple1 <- st_transform(shape_simple, 4326) #change CRS to match east coast map
  #grids
grid_cells <- st_read("/Users/ebell23/Downloads/grid_25kmClip_ExportFeatures.shp", quiet = TRUE)
#grid_fixed <- st_set_crs(grid_cells, 5070) 
grid <- st_transform(grid_cells, 4326)

  #grid_id x year
grid_str <- read_excel("/Users/ebell23/Downloads/grid25_strand_pol_all.xlsx")
  sum(tapply(grid_str$n_strandings, grid_str$grid_id, sum) == 0) #total number of grid_ids where stranding count = 0 
  length(unique(grid_str$grid_id)) #total number of unique grid_ids
  
#Filter all Strandings to years
str_1996 <- coast_str %>% filter(Year_num == 1996)
str_2009 <- coast_str %>% filter(Year_num == 2009)
str_2022 <- coast_str %>% filter(Year_num == 2022)

#Filter NARW- Policy years 
#------
  #1996
narw_1996 <- shape_simple1 %>% filter(PolCategor == "NARW-specific", Year_Impl == 1996)

ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") +
  geom_sf(data = grid, color = "black") +
  geom_point(data = str_1996, aes(x = lon, y = lat), size = 1, color = "lightgrey", fill = NA) +
  geom_sf(data = narw_1996, color = "darkblue", fill = NA, size = 1) + 
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  #geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  labs(title = "Strandings during NARW-specific policies in 1996",
       x = "Longitude", y = "Latitude") +
  theme_light()

  #2009
narw_2009 <- shape_simple1 %>% filter(PolCategor == "NARW-specific", Year_Impl == 2009)

ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") +
  geom_point(data = str_2009, aes(x = lon, y = lat), size = 1, color = "lightgrey") +
  geom_sf(data = narw_2009, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  #coord_fixed(1.3) +
  scale_color_manual(values = "black") +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  labs(title = "Strandings during NARW-specific policies in 2009",
       x = "Longitude", y = "Latitude") +
  theme_light()

  #2022
narw_2022 <- shape_simple1 %>% filter(PolCategor == "NARW-specific", Year_Impl == 2022)

ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") +
  geom_point(data = str_2022, aes(x = lon, y = lat), size = 1, color = "lightgrey") +
  geom_sf(data = narw_2022, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  #coord_fixed(1.3) +
  #geom_point(data = coast_str, aes(x = lon, y = lat), size = 1) +
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  labs(title = "Strandings during NARW-specific policies in 2022",
       x = "Longitude", y = "Latitude") +
  theme_light()



#Filter during TRP years
 
  #-----
#1996
trp_1996 <- shape_simple1 %>% filter(PolCategor == "TRP", Year_Impl == 1996)
str_1996 <- coast_str %>% filter(Year_num == 1996)

trp4 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_1996, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = trp_1996, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()


  #2009
trp_2009 <- shape_simple1 %>% filter(PolCategor == "TRP", Year_Impl == 2009)
str_2009 <- coast_str %>% filter(Year_num == 2009)

trp5 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2009, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = trp_2009, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

  #2022
trp_2022 <- shape_simple1 %>% filter(PolCategor == "TRP", Year_Impl == 2022)
str_2022 <- coast_str %>% filter(Year_num == 2022)

trp6 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2022, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = trp_2022, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

library(patchwork)
(trp1 + trp3 + trp4 + trp6) + plot_layout(ncol = 2)

#Gear Restrictions and years Filtered 
  #----
  #1996
gear_1996 <- shape_simple1 %>% filter(PolCategor == "Gear-related", Year_Impl == 1996)

gear1 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_1996, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = gear_1996, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()
  
gear1

  #2009
gear_2009 <- shape_simple1 %>% filter(PolCategor == "Gear-related", , Start_yr <= 2009, End_yr >= 2009)

gear2 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2009, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = gear_2009, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()
  
gear2

  #2022-no gear policies implemented this year
gear_2022 <- shape_simple1 %>% filter(PolCategor == "Gear-related", Year_Impl == 2022)

gear3 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2022, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = gear_2022, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

gear3


#MPA Policies
  #----
#1996
mpa_1996 <- shape_simple1 %>% filter(PolCategor == "MPA", Year_Impl == 1996)

mpa1 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_1996, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = mpa_1996, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

mpa1

  #2009
mpa_2009 <- shape_simple1 %>% filter(PolCategor == "MPA", Year_Impl == 2009)

mpa2 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2009, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = mpa_2009, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

mpa2

  #2022
mpa_2022 <- shape_simple1 %>% filter(PolCategor == "MPA", Year_Impl == 2022)

mpa3 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2022, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = mpa_2022, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

mpa3

#Closed Area Policies
  #----
  #1996-none implemented
ca_1996 <- shape_simple1 %>% filter(PolCategor == "Closed Area", Year_Impl == 1996)

ca1 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_1996, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = mpa_1996, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

ca1

  #2009-none implemented
ca_2009 <- shape_simple1 %>% filter(PolCategor == "Closed Area", Start_yr <= 2009, End_yr >= 2009)

ca2 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2009, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = ca_2009, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

ca2

  #2022-none implemented
ca_2022 <- shape_simple1 %>% filter(PolCategor == "Closed Area", Year_Impl == 2022)

ca3 <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = str_2022, aes(x = lon, y = lat), size = 1, color = "black") +
  geom_sf(data = mpa_2022, aes(fill = PolCategor), color = "darkblue", fill = NA, size = 1) + 
  #scale_fill_manual(values = "darkblue") +
  coord_sf(xlim = c(-80.6, -65.80), ylim = c(36.5, 47)) + #adjusts the zoom
  geom_text_repel(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 4, force = 1) + #adds state abbreviations to middle of state
  theme(
    legend.position = "bottom", 
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10),      # Axis tick labels
    #panel.grid.major = element_blank(), # Remove major grid lines
    #panel.grid.minor = element_blank(), # Remove minor grid lines
  ) +
  theme_light()

ca3

#------
#Gear-related years----
strand <- coast_str %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) #keeping all stranding years


strand1 <- coast_str %>% 
  filter(Year_num == 1996) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) #if you filter to specific stranding year

yr_keep <- 1996
cat_keep <- "Gear-related"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_1996_gear <- grid_pol_year %>%
  filter(Year_num == yr_keep) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "Gear-related")), .groups = "drop")


grid_gear_1996 <- grid %>%
  left_join(pol_1996_gear, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid1 <- ggplot() +
  geom_sf(data = grid_gear_1996, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_gear_1996 %>% filter(has_policy == 1),
    fill = "indianred3", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "1996",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#Gear-related in 2009
strand2 <- coast_str %>% 
  filter(Year_num == 2009) %>%
st_as_sf(coords = c("lon", "lat"), crs = 4326)  #if you filter to specific stranding year

yr_keep1 <- 2009
cat_keep <- "Gear-related"


grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_2009_gear <- grid_pol_year %>%
  filter(Year_num == yr_keep1) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "Gear-related")), .groups = "drop")


grid_gear_2009 <- grid %>%
  left_join(pol_2009_gear, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid2 <- ggplot() +
  geom_sf(data = grid_gear_2009, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_gear_2009 %>% filter(has_policy == 1),
    fill = "indianred3", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2009",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  #Gear-related in 2022
strand3 <- coast_str %>% 
  filter(Year_num == 2022) %>%
st_as_sf(coords = c("lon", "lat"), crs = 4326) #if you filter to specific stranding year

yr_keep2 <- 2022
cat_keep <- "Gear-related"


grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_2022_gear <- grid_pol_year %>%
  filter(Year_num == yr_keep2) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "Gear-related")), .groups = "drop")


grid_gear_2022 <- grid %>%
  left_join(pol_2022_gear, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid3 <- ggplot() +
  geom_sf(data = grid_gear_2022, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_gear_2022 %>% filter(has_policy == 1),
    fill = "indianred3", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2022",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  #Combine all 3 maps into 1
library(patchwork)
gear_maps <- (grid1 + grid2 + grid3) +
plot_annotation(
  title = 'Strandings in active Gear-related policy areas',
  theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
)


#MPA by years----
  #1996
yr_keep <- 1996
cat_keep <- "MPA"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_1996_mpa <- grid_pol_year %>%
  filter(Year_num == yr_keep) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "MPA")), .groups = "drop")

grid_mpa_1996 <- grid %>%
  left_join(pol_1996_mpa, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid10 <- ggplot() +
  geom_sf(data = grid_mpa_1996, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_mpa_1996 %>% filter(has_policy == 1),
    fill = "indianred3", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "1996",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  #2009
yr_keep1 <- 2009
cat_keep <- "MPA"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_2009_mpa <- grid_pol_year %>%
  filter(Year_num == yr_keep1) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "MPA")), .groups = "drop")


grid_mpa_2009 <- grid %>%
  left_join(pol_2009_mpa, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid11 <- ggplot() +
  geom_sf(data = grid_mpa_2009, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_mpa_2009 %>% filter(has_policy == 1),
    fill = "indianred3", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2009",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  #2022
yr_keep2 <- 2022
cat_keep <- "MPA"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_2022_mpa <- grid_pol_year %>%
  filter(Year_num == yr_keep2) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "MPA")), .groups = "drop")

grid_mpa_2022 <- grid %>%
  left_join(pol_2022_mpa, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid12 <- ggplot() +
  geom_sf(data = grid_mpa_2022, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_mpa_2022 %>% filter(has_policy == 1),
    fill = "indianred3", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2022",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

mpa_maps <- (grid10 + grid11 + grid12) +
  plot_annotation(
    title = 'Strandings in active Marine Protected Areas',
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )




#TRP by years----
#1996
yr_keep <- 1996
cat_keep1 <- "TRP"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_1996_trp <- grid_pol_year %>%
  filter(Year_num == yr_keep) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "TRP")), .groups = "drop")

grid_trp_1996 <- grid %>%
  left_join(pol_1996_trp, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid4 <- ggplot() +
  geom_sf(data = grid_trp_1996, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_trp_1996 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "1996",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#2009
yr_keep1 <- 2009
cat_keep <- "TRP"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_trp_2009 <- grid_pol_year %>%
  filter(Year_num == yr_keep1) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "TRP")), .groups = "drop")


grid_trp_2009 <- grid %>%
  left_join(pol_trp_2009, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid5 <- ggplot() +
  geom_sf(data = grid_trp_2009, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_trp_2009 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2009",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


#2022
yr_keep2 <- 2022
cat_keep <- "TRP"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_trp_2022 <- grid_pol_year %>%
  filter(Year_num == yr_keep2) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "TRP")), .groups = "drop")

grid_trp_2022 <- grid %>%
  left_join(pol_trp_2022, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid6 <- ggplot() +
  geom_sf(data = grid_trp_2022, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_trp_2022 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2022",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

library(patchwork)
trp_maps <- (grid4 + grid5 + grid6) +
  plot_annotation(
    title = 'Strandings in active Take Reduction Plan areas',
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )

#NARW by years----
yr_keep <- 1996
cat_keep <- "NARW-specific"

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
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "1996",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
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
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2022",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
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
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2022",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

library(patchwork)
narw_maps <- (grid7 + grid8 + grid9) +
  plot_annotation(
    title = 'Strandings in active NARW-specific policy areas',
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )


ggsave("narw_maps.png", plot = narw_maps, dpi = 500)

ggsave("mpa_maps.png", plot = mpa_maps, dpi = 500)

ggsave("gear_maps.png", plot = gear_maps, dpi = 500)

ggsave("trp_maps.png", plot = trp_maps, dpi = 500)




#Closed Areas by years----
  #1996
yr_keep <- 1996
cat_keep <- "Closed Area"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_ca_1996 <- grid_pol_year %>%
  filter(Year_num == yr_keep) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "Closed Area")), .groups = "drop")

grid_ca_1996 <- grid %>%
  left_join(pol_ca_1996, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid13 <- ggplot() +
  geom_sf(data = grid_ca_1996, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_ca_1996 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "1996",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  #2009
yr_keep1 <- 2009
cat_keep <- "Closed Area"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_ca_2009 <- grid_pol_year %>%
  filter(Year_num == yr_keep1) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "Closed Area")), .groups = "drop")

grid_ca_2009 <- grid %>%
  left_join(pol_ca_2009, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid14 <- ggplot() +
  geom_sf(data = grid_ca_2009, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_ca_2009 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2009",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

  #2022
yr_keep2 <- 2022
cat_keep <- "Closed Area"

grid_fixed <- grid %>%
  mutate(grid_id = as.character(grid_id))

grid_pol_year <- grid_str %>%
  mutate(grid_id = as.character(grid_id),
         polcat_list = as.character(PolCat_List))

pol_ca_2022 <- grid_pol_year %>%
  filter(Year_num == yr_keep) %>%
  separate_rows(PolCat_List, sep = ";") %>%
  mutate(PolCat_List = str_trim(PolCat_List)) %>%
  group_by(grid_id) %>%
  summarise(has_policy = as.integer(any(PolCat_List == "Closed Area")), .groups = "drop")

grid_ca_2022 <- grid %>%
  left_join(pol_ca_2022, by = "grid_id") %>%
  mutate(has_policy = if_else(is.na(has_policy), 0L, has_policy))

grid15 <- ggplot() +
  geom_sf(data = grid_ca_2022, fill = NA, color = "grey65", linewidth = 0.15) +
  geom_sf(
    data = grid_ca_2022 %>% filter(has_policy == 1),
    fill = "steelblue", color = NA, alpha = 0.6
  ) +
  geom_sf(data = strand, size = 0.1, color = "grey8", fill = "NA") +
  coord_sf(xlim = c(-79, -66), ylim = c(36.5, 46)) + #zooms into the map more
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "grey", color = "black") +
  labs(
    title = "2022",
    x = "Longitude", y = "Latitude"
  ) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

library(patchwork)
(grid13 + grid14 + grid15) +
  plot_annotation(
    title = 'Strandings in closed areas',
    theme = theme(plot.title = element_text(size = 18, hjust = 0.5))
  )






(trp_maps / narw_maps / gear_maps / mpa_maps) 

