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

library(tigris)
states <- tigris::states(cb = TRUE) %>%
  sf::st_transform(4326)

east_coast <- states %>%
  filter(NAME %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island","Connecticut", "New York", "New Jersey", "Pennsylvania", "Delaware", "Maryland", "District of Columbia", "Virginia")
  )

#Mapping Gi_Bin shows Confidence interval ranging from 90%, 95%, 99% (1-3 bin)----
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


(strand_htspt[strand_htspt$Gi_Bin == 3, ]) # to get table of grid cells where GI_bin >0
(strand_htspt[strand_htspt$GiZScore > 3, ])

htspt <- strand_htspt %>%
  filter(GiZScore < 3) %>%
  filter (Gi_Bin > 2)

#Mapping Gi_Bin overlayed with East Coast States Map and Policies----
library(ggspatial)

full_ht <- ggplot() +
  geom_sf(data = strand_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  coord_sf(xlim = c(-81, -68), ylim = c(36.5, 45)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       title = "Stranding Hotspots",
       y = "Latitude", x = "Longitude") +
  #coord_sf(crs = 5070, default_crs = sf::st_crs(4326)) + #ensures true north is calculated correctly
  #annotation_scale(location = "br", width_hint = 0.4) + #adds scale bar in km
  #annotation_north_arrow(location = "br", which_north = "true", 
                         #pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         #style = north_arrow_fancy_orienteering())+
  #geom_text(data = state_centroids, aes(x = long, y = lat, label = state_abb), size = 6.5) + #adds state abbreviations to middle of state +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(color = "black", size = 14),   
    #axis.ticks = element_text(color = "black", size = 14),   
    axis.text = element_text(size = 10, color = "black") 
  )


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

#Mapping Optimized hotspot of all coastal strandings
coast_str_htsp <- st_read("/Users/ebell23/Downloads/coast_strand_OHSA.shp")
coast_htspt <- st_transform(coast_str_htsp, 4326)


htspt <- coast_htspt %>%
  filter(GiZScore > 1) %>%
  filter(GiZScore < 8)
  filter (Gi_Bin == 3) #use to look at the range of z-scores and were they are located

strand_htspt <- ggplot() +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  geom_sf(data = coast_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       title = "Stranding Hotspots",
       y = "Latitude", x = "Longitude") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(color = "black", size = 14),   
    #axis.ticks = element_text(color = "black", size = 14),   
    axis.text = element_text(size = 10, color = "black") 
  )

#Mapping Optimized hot spots by taxonomic group----
odon_hotspot <- st_read("/Users/ebell23/Downloads/odon_strand_OpHSA.shp")
names(odon_hotspot)
odon_htspt <- st_transform(odon_hotspot, 4326)


odon_ht <- ggplot() +
  geom_sf(data = odon_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       title = "Odontocete Hotspots",
       y = "Latitude", x = "Longitude") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(color = "black", size = 14),   
    #axis.ticks = element_text(color = "black", size = 14),   
    axis.text = element_text(size = 10, color = "black") 
  )


#Mysticeti Hot Spots
mys_hotspot <- st_read("/Users/ebell23/Downloads/mys_strand_OpHSA.shp")
colnames(mys_hotspot)
mys_htspt <- st_transform(mys_hotspot, 4326)

mys_ht <- ggplot() +
  geom_sf(data = mys_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       title = "Mysticeti Hotspots",
       y = "Latitude", x = "Longitude") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(color = "black", size = 14),   
    #axis.ticks = element_text(color = "black", size = 14),   
    axis.text = element_text(size = 10, color = "black") 
  )


#Pinniped hot spots
pinn_hotspot <- st_read("/Users/ebell23/Downloads/pinn_strand_OpHSA.shp")
colnames(pinn_hotspot)
pinn_htspt <- st_transform(pinn_hotspot, 4326)

pinn_ht <- ggplot() +
  geom_sf(data = pinn_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       title = "Pinniped Hotspots",
       y = "Latitude", x = "Longitude") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(color = "black", size = 14),   
    #axis.ticks = element_text(color = "black", size = 14),   
    axis.text = element_text(size = 10, color = "black") 
  )

library(patchwork)
(strand_htspt + pinn_ht) / (mys_ht + odon_ht)


#Combining all hotspot maps into 1----
  #all strandings


strand_htspt <- ggplot() +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  geom_sf(data = coast_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       subtitle = "A) All stranding hotspots",
       y = "Latitude", x = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(color = "black", size = 14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),   
    axis.text.y = element_text(size = 10, color = "black"),
    legend.position = "none"
  )
    
    #pinnipeds
pinn_ht <- ggplot() +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  geom_sf(data = pinn_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       subtitle = "B) Pinniped hotspots",
       y = "Latitude", x = "Longitude") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(color = "black", size = 14),   
    #axis.ticks = element_text(color = "black", size = 14),   
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "none"
  )
  
    
    #Odontocete
odon_ht <- ggplot() +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  geom_sf(data = odon_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       subtitle = "A) Odontocete hotspots",
       y = "Latitude", x = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),   
    axis.title.y = element_text(color = "black", size = 14),  
    axis.text.y = element_text(size = 10, color = "black"),
    legend.position = "right"
  )

  #Mysticeti
mys_ht <- ggplot() +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  geom_sf(data = mys_htspt, aes(fill = factor(Gi_Bin)), color = NA, linewidth = 0.1) +
  coord_sf(xlim = c(-81, -67), ylim = c(36.5, 46)) +
  scale_fill_manual(values = c(
    "-3" = "darkblue",
    "-2" = "lightblue",
    "-1" = "skyblue",
    "0"  = NA,
    "1"  = "pink",
    "2"  = "red",
    "3"  = "darkred"
  )) +
  labs(fill = "Gi_Bin",
       subtitle = "C) Mysticeti hotspots",
       y = NULL, x = "Longitude") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(color = "black", size = 14), 
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),  
    axis.text.x = element_text(size = 10, color = "black"),
    legend.position = "right"
  )


(strand_htspt + pinn_ht) / (odon_ht + mys_ht)


(plot_spacer() + pinn_ht + plot_spacer()) / (odon_ht + mys_ht)


(plot_spacer() + odon_ht + plot_spacer()) / (pinn_ht + mys_ht)

(pinn_ht + mys_ht) / (plot_spacer() + odon_ht + plot_spacer())
