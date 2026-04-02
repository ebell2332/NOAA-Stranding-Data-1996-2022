Sys.setenv(SHAPE_RESTORE_SHX = "YES")
coast_pol <- st_read("/Users/ebell23/Downloads/coastal_policies_ExportFeatures.shp", quiet = TRUE) 
colnames(coast_pol)

st_crs(coast_pol)
library(ggspatial)

#Study Area Map Steps----
#(disregard chaning crs to 5070 for now until you can figure out how to map it properly without distorting your work)
  #Read in EEZ boundary
eez <- st_read("/Users/ebell23/Downloads/eez/eez.shp") #read in eez boundaries from maritime regions
#eez <- st_transform(eez, crs = 5070)


library(tigris)
states <- tigris::states(cb = TRUE) %>%
  sf::st_transform(4326)

east_coast <- states %>%
  filter(NAME %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island","Connecticut", "New York", "New Jersey", "Pennsylvania", "Delaware", "Maryland", "District of Columbia", "Virginia")
)

eez_label <- st_sfc(
  st_point(c(-66.5, 42)),
  crs = 4326
)

unique(east_coast$STUSPS)
label_sf <- sf::st_sf(label = "US/Canada Border", geometry = eez_label)

state_label <- east_coast %>%
  filter(!STUSPS %in% c("DE", "DC", "MD", "RI"))

#Plot East Coast map with EEZ border---scale bar might ne inaccurate
inset_map <- ggplot() +
  geom_sf(data = eez, color = "black", fill = NA, size = 0.5) +
  geom_sf(data = east_coast, color = "black", fill = "gray85") + 
  geom_sf_text(data = state_label, aes(label = STUSPS), size = 4) +
  geom_sf_text(data = label_sf, aes(label = label), angle = -60) + #adds text to the edge of the EEZ border
  coord_sf(xlim = c(-83, -66),
           ylim = c(37, 48)) +
           #default_crs = sf::st_crs(4326)) +
   #annotation_scale(location = "br") +
  #annotation_north_arrow(location = "br", which_north = "true") +
  labs(x = "Longitude", y = "Latitude") + 
  theme(
    panel.grid = element_blank(),
    plot.subtitle = element_text(color = "black", size = 16),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10) # Axis tick labels
  )



#Plot US Map
states_conus <- states %>% #from tigris dataset
  filter(STUSPS %in% state.abb) %>%
  filter(!STUSPS %in% c("AK", "HI"))

state_map <- ggplot() +
  geom_sf(data = states_conus, color = "black", fill = NA) +
  geom_sf(data = east_coast, color = "black", fill = "gray43") + 
  geom_rect(aes(xmin = -84, xmax = -66, ymin = 36.5, ymax = 48), color = "red", fill = NA) +
  #coord_sf(crs = 5070, default_crs = sf::st_crs(4326)) +
  theme_void()
  

#Combine both maps 
library(cowplot)
inset_map <- inset_map + 
  coord_sf(crs = 5070,
           xlim = c(-83, -65),
           ylim = c(36.5, 48),
           default_crs = sf::st_crs(4326)
  ) + annotation_scale(location = "br") +
  annotation_north_arrow(location = "br", which_north = "true") 

state_map <- state_map +
  coord_sf(crs = 5070, default_crs = sf::st_crs(4326))

 ggdraw() + 
  draw_plot(inset_map) +
  draw_plot(state_map, x = 0.13, y = 0.73, width = 0.30, height = 0.25) 



#Map of Policies----
 
 east_coast <- states %>%
   filter(NAME %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island","Connecticut", "New York", "New Jersey", "Pennsylvania", "Delaware", "Maryland", "District of Columbia", "Virginia")
   )
 
 eez_label <- st_sfc(
   st_point(c(-66.5, 42)),
   crs = 4326
 )
 
policies_data %>% count(PolCategor) #number of policies per category

shape_simple <- st_simplify(coast_pol, preserveTopology = TRUE, dTolerance = 100)
# Plot only the geometry
plot(st_geometry(shape_simple))


shape_simple1 <- st_transform(shape_simple, 4326) #change CRS to match east coast map

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

east_coast_sf <- st_as_sf(east_coast_map, coords = c("long", "lat"), 
                          crs = 4326,
                          agr = "constant") #making map into an sf


custom_colors1 <- c("#D81B60", "#1E88E5", "#FFC107", "#004D40","darkred", "#64F626")

direct_pol <- shape_simple1 %>%
  filter(Target == "Direct")

indirect_pol <- shape_simple1 %>%
  filter(Target == "Indirect")

#Direct policies mapped
 dir_pol <- ggplot() +
  geom_sf(data = direct_pol, fill = "steelblue", color = NA) +
   geom_sf(data = direct_pol, fill = NA, color = "black", linewidth = 0.3) + # draws outline of each individual polygon in the second layer
   geom_sf(data = east_coast, color = "black", fill = "gray85") + 
   coord_sf(xlim = c(-83, -66),
            ylim = c(37, 48)) +
   #coord_sf(crs = 5070, default_crs = sf::st_crs(4326)) +
  labs(subtitle = "A) Direct policies",
       x = "Longitude", y = "Latitude") +
  theme(
    plot.subtitle = element_text(color = "black", size = 16),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10), # Axis tick labels
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

  #Indirect Policies mapped
indir_pol <- ggplot() +
  geom_sf(data = indirect_pol, fill = "indianred3", color = NA) +
  geom_sf(data = indirect_pol, fill = NA, color = "black", linewidth = 0.3) + # draws outline of each individual polygon in the second layer
  geom_sf(data = east_coast, color = "black", fill = "gray85") +
  coord_sf(xlim = c(-83, -66),
           ylim = c(37, 48)) +
  #coord_sf(crs = 5070, default_crs = sf::st_crs(4326)) +
  #annotation_scale(location = "br", width_hint = 0.4) + #adds scale bar in km
  #annotation_north_arrow(location = "br", which_north = "true", 
                         #pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         #style = north_arrow_fancy_orienteering()) + #adds true north
  labs(subtitle = "B) Indirect policies ",
       x = "Longitude", y = NULL) +
  theme(
    plot.subtitle = element_text(color = "black", size = 16),      # Title color and size
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),   # Y-axis text
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.text.x = element_text(size = 10), # Axis tick labels
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 


library(patchwork)
(dir_pol + indir_pol)

####----Ignore for now----####
study_area <- st_read("/Users/ebell23/Downloads/AP_Data/Tables/Study_Area1_ExportFeatures.shp", quiet = TRUE)

study_simple <- st_simplify(study_area, preserveTopology = TRUE, dTolerance = 100)
st_crs(study_simple)
plot(study_simple)

ggplot() +
  geom_sf(data = shape_simple, aes(fill = PolCategor)) + 
  scale_fill_manual(values = custom_colors1) +
  geom_sf(data = study_simple, fill = NA, color = "black") +
  coord_sf() + 
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

group_colors <- c("#1E88E5","#004D40","darkred")

inset_map <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "gray85", color = "black") +
  geom_sf(data = inset_map, aes(fill = PolCategor)) + 
  scale_fill_manual(values = group_colors, guide = "none") +
  coord_sf(xlim = c(-76, -65.80), ylim = c(36.5, 44.5)) + #adjusts the zoom 
  theme_gray() + #makes background grey
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) 

inset_map  

#combine both policy category maps  
library(patchwork)
main_map + inset_element(inset_map, left = 0.65, right =1.00, bottom = 0.02, top = 0.50) #left/right = horizontal size and position, top/bottom = vertical size and position


