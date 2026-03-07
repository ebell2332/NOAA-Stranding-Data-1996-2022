#Hot Spot Analysis Results from ArcGIS Pro
library(readxl) #reading in excel files
library(dplyr) #data manipulation with data frames and tables
library(ggplot2) #visual plots
library(tidyr) #create tidy data
library(lubridate) #manipulating and parsing date-time data
library(ggrepel) # provides geoms to automaticalls position overlapping text and labels
library(vegan) #ecological functions for diversity analysis
library(sf) #spatial vector data like polygons, points, and lines
library(tibble)

#Read in shapefile from ArcGIS
  #Currently only has positive Gi_Bin = no cold spots
strand_hotspot <- st_read("/Users/ebell23/Downloads/grid_25km_coaststrand_HotSpots.shp")
names(strand_hotspot)
strand_htspt <- st_transform(strand_hotspot, 4326) #transform to CRS to overlay with east coast map


unique(strand_hotspot$Gi_Bin) #find unique numbers for scale

#Mapping Gi_Bin shows Confidence interval ranging from 90%, 95%, 99% (1-3 bin)
ggplot(strand_htspt) +
  geom_sf(aes(fill = factor(Gi_Bin)), color = "black", size = 0.1) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = "grey90",
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Hot/Cold Spot") +
  theme_minimal()


#Mapping Gi_Bin overlayed with East Coast States Map
ggplot() +
  geom_sf(data = strand_htspt, aes(fill = factor(Gi_Bin)), color = NA, size = 0.1) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = "grey90",
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Hot/Cold Spot",
       title = "Stranding Hot Spots",
       y = "Latitude", x = "Longitude") +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = NA , color = "black", size = 0.1) +
  geom_text(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 6.5) + #adds state abbreviations to middle of state +
  theme_minimal()


#Mapping Z-Score shows continuous intensity of clustering
ggplot(strand_htspt) +
  geom_sf(aes(fill = GiZScore)) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  theme_minimal()
