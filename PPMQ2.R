library(spatstat.geom)
library(spatstat.explore)
library(purrr)
library(spatstat.model)

q2 <- read_excel("/Users/ebell23/Downloads/AP_Data/Tables/strand_q2_allyrs.xlsx")


colnames(q2)
####Question 2###
#Set Up for Model####
#add study area
library(sf)
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
study_sf <- st_read("/Users/ebell23/Downloads/AP_Data/Tables/Study_Area1_ExportFeatures.shp", quiet = TRUE) 
st_crs(study_sf) #check coordinate system
st_bbox(study_sf) #check geometry of study area
W_q2 <- as.owin(study_sf)

#check points and crs with study area
q2_sf <- st_as_sf(q2, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
q2_sf <- st_transform(q2_sf, st_crs(study_sf))
st_crs(q2_sf)


#create points with overlap class (marks)
q2_sf_filt <- q2_sf %>% mutate(overlap_class = case_when(
  overlap_class %in% c("1") ~ "Single", overlap_class %in% c("2plus") ~ "Multiple", TRUE ~ NA_character_)) %>%
  filter(!is.na(overlap_class)) %>%
  mutate(overlap_class = factor(overlap_class, levels = c("Single", "Multiple")
))


coords <- st_coordinates(q2_sf_filt) #coordinates
inside <- spatstat.geom::inside.owin(coords[,1], coords[,2], W_q2) #points inside the window

q2_sf2  <- q2_sf_filt[inside, ]
coords2 <- coords[inside, , drop = FALSE]

strand_ppp <- spatstat.geom::ppp(x = coords2[,1], y = coords2[,2], window = W_q2, marks = q2_sf2$overlap_class)


npoints(strand_ppp)
length(marks(strand_ppp))
table(marks(strand_ppp))

spatstat.geom::npoints(strand_ppp)
anyNA(spatstat.geom::marks(strand_ppp))
table(spatstat.geom::marks(strand_ppp))

#1. Run Per-year model####
cv_q2 <- function(sf_data2, W, year_val, overlap_val, min_n = 50) {
  
  sub <- sf_data2 %>%
    filter(
      Year_num == year_val,
      overlap_class == overlap_val
    )
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- spatstat.geom::ppp(xy[,1], xy[,2], window = W)
  
  fit <- spatstat.model::ppm(X ~ x + y)
  
  Z <- as.numeric(predict(fit))
  
  tibble(
    year = year_val,
    overlap_class = overlap_val,
    n = n,
    cv_intensity = sd(Z) / mean(Z)
  )
}


years_all <- sort(unique(q2_sf2$Year_num)) #sorting by years
overlap <- sort(unique(q2_sf2$overlap_class)) #sorting "1" and "2plus"

results_q2 <- list()

for (yr in years_all) {
  for (ov in overlap) {
    
    res <- cv_q2(
      sf_data = q2_sf2,
      W = W_q2,
      year_val = yr,
      overlap_val = ov,
      min_n = 50 #minimum of 50 strandings in this year x overlap class
    )
    
    if (!is.null(res)) {
      results_q2[[length(results_q2) + 1]] <- res
    }
  }
}

cv_q2_df <- bind_rows(results_q2)
cv_q2_df


cv_q2_df %>% count(overlap_class)
cv_q2_df %>% count(year)
cv_q2_df %>% group_by(overlap_class) %>%
  summarise(min_n = min(n),
  median_n = median(n),
  max_n = max(n))

#Lineplot of CV intensities by year
ggplot(cv_q2_df,
       aes(x = year, y = cv_intensity, color = overlap_class, group = overlap_class)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  labs(
    x = "Year",
    y = "Coefficient of variation of spatial concentration",
    color = "Policy Category Overlap",
    title = "Yearly spatial concentration of strandings in overlapping policy areas"
  ) +
  theme(
      plot.title = element_text(color = "black", size = 17),      # Title color and size
      axis.title.x = element_text(color = "black", size = 15),   # X-axis title
      axis.title.y = element_text(color = "black", size = 15),   # Y-axis title
      axis.text = element_text(size = 10), # Axis tick labels
      legend.position = "bottom",
      legend.title = element_text(size = 12,),
      legend.text = element_text(size = 10)) 



####By Taxonomic group####

taxon_gp <- "tax_group"  #new name for column 

cv_q2_tax <- function(sf_data2, W, year_val, overlap_val, taxon_val, min_n = 40) 
  #min_n = 40 because taxon group reduces sample size and it helps to account for the smaller numbers per year
  {
  sub <- sf_data2 %>%
    filter(
      Year_num == year_val,
      overlap_class == overlap_val,
      .data[[taxon_gp]] == taxon_val
    )
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- spatstat.geom::ppp(xy[,1], xy[,2], window = W)
  
  fit <- spatstat.model::ppm(X ~ x + y)
  
  Z <- as.numeric(predict(fit))
  
  tibble(
    year = year_val,
    overlap_class = overlap_val,
    taxon_gp = taxon_val,
    n = n,
    cv_intensity = sd(Z) / mean(Z)
  )
}


years_all <- sort(unique(q2_sf2$Year_num)) #sorting by years
overlap <- sort(unique(q2_sf2$overlap_class)) #sorting "1" and "2plus"
taxa <- sort(unique(q2_sf2$tax_group)) #sorting by taxonomic group

results_q2_tax <- list()

for (yr in years_all) {
  for (ov in overlap) {
    for (tx in taxa) {
    
    res <- cv_q2_tax(
      sf_data2 = q2_sf2,
      W = W_q2,
      year_val = yr,
      overlap_val = ov,
      taxon_val = tx,
      min_n = 40 #minimum of 40 strandings in this year x overlap class x tax group
    )
    
    if (!is.null(res)) {
      results_q2_tax[[length(results_q2_tax) + 1]] <- res
    }
    }
  }
}


cv_q2_tax <- bind_rows(results_q2_tax)
cv_q2_tax

#sanity check and summarise
cv_q2_tax %>% count(taxon_gp)
cv_q2_tax %>% count(overlap_class)

cv_q2_tax %>% group_by(taxon_gp, overlap_class) %>%
  summarise(
    min_n = min(n),
    median_n = median(n),
    max_n = max(n),
    .groups = "drop"
  )




#Column chart of mean CV by taxonomic group and overlap class
cv_q2_tax_mn <- cv_q2_tax %>%
  group_by(taxon_gp, overlap_class) %>%
  summarise(
    mean_cv = mean(cv_intensity, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(cv_q2_tax_mn, aes(x = overlap_class, y = mean_cv, fill = overlap_class)) +
  geom_col(alpha = 0.7) +
  facet_wrap(~ taxon_gp, scales = "free_y") +
  labs(
    x = "Policy category overlap",
    y = "Avg CV",
    title = "Average spatial concentration by taxonomic group and policy category overlap"
  ) +
  theme(
    strip.background = element_rect(fill = "white", color = "white"),  #facet wrap background and color
    strip.text = element_text(color = "black", size = 10,face = "italic"), #facet wrap text size and font
    plot.title = element_text(color = "black", size = 18),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10)      # Axis tick labels
  ) +
  theme(legend.position = "none")


#2. Run Model with map output for CV pixels----
cv2_q2 <- function(sf_data2, W, year_val, overlap_val, min_n = 50) {
  
  sub <- sf_data2 %>%
    filter(
      Year_num == year_val,
      overlap_class == overlap_val
    )
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  #fit ppm in same CRS as study area/W
  xy <- st_coordinates(sub)
  X  <- ppp(xy[,1], xy[,2], window = W)
  
  fit <- ppm(X ~ x + y)
  
  lam_im <- predict(fit, type = "trend") #predicted intensity surface
  #Z <- as.numeric(predict(fit))
  
  
  lam_vals <- as.vector(lam_im$v) #CV computed from predicted pixel values
  lam_vals <- lam_vals[is.finite(lam_vals) & lam_vals > 0] #CV computed from predicted pixel values
  cv_val <- sd(lam_vals) / mean(lam_vals)
  
  library(terra)
  r_planar <- terra::rast(lam_im) #convert intensity surface to lat/long for consistent mapping
  terra::crs(r_planar) <- sf::st_crs(study_sf)$wkt #assign CRS 
  
  r_ll <- project(r_planar, "EPSG:4326") #reproject to lat/long
  
  map_val <- as.data.frame(r_ll, xy = TRUE, na.rm = TRUE) #raster to df for ggplot
  names(map_val) <- c("lon", "lat", "intensity")
  

  map_val <- map_val %>%
    mutate(
      Year_num = year_val,
      overlap_class = overlap_val,
      n = n,
      cv_intensity = cv_val
    )
  
  list(
    summary = tibble(
    Year_num = year_val,
    overlap_class = overlap_val,
    n = n,
    cv_intensity = cv_val
  ),
  map = map_val
  )
}


years_all <- sort(unique(q2_sf2$Year_num)) #sorting by years
overlap <- sort(unique(q2_sf2$overlap_class)) #sorting "1" and "2plus"

results2_q2 <- list()

for (yr in years_all) {
  for (ov in overlap) {
    
    res <- cv2_q2(
      sf_data2 = q2_sf2,
      W = W_q2,
      year_val = yr,
      overlap_val = ov,
      min_n = 50 #minimum of 50 strandings in this year x overlap class
    )
    
    if (!is.null(res)) {
      results2_q2[[paste(yr, ov, sep = "_")]] <- res
    }
  }
}

cv2_df <- bind_rows(lapply(results2_q2, '[[', "summary"))
map_val <- bind_rows(lapply(results2_q2, '[[', "map"))

map_val$overlap_class <- factor(map_val$overlap_class, levels = c("Single", "Multiple"),
                                labels = c("Single", "Multiple")) #re-order so that single is first and its not by alphabetical order

yr_spec <- 2017 #choose specific year to filter results to

map_yr <- map_val %>%
  filter(Year_num == yr_spec) #filter data set to one year



ggplot(map_yr) + #plot the filtered year
  geom_tile(aes(x = lon, y = lat, fill = intensity)) +
  coord_fixed() +
  #scale_fill_viridis_c(name = "Intensity") +
  scale_fill_distiller(palette = "YlOrRd", direction =1, name = "Intensity") +
  #facet_wrap(~ overlap_class, ncol = 1)+ #makes the overlap classes vertical
  facet_grid(~overlap_class) + #makes overlap classes horizontal
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") + #adds east coast outline to the map
  theme_minimal()+
  labs(fill = "Intensity", title = "Spatial stranding concentration in 2017 by overlap categories") +
  theme_gray() #makes background grey

#Multiple years (small maps, one per year)
yrs_use <- 2015:2017

map_multiyr <- map_val %>%
  filter(Year_num == yrs_use)

ggplot(map_multiyr) + #plot the filtered year
  geom_tile(aes(x = lon, y = lat, fill = intensity)) +
  coord_fixed() +
  scale_fill_viridis_c(name = "Intensity") +
  facet_grid(Year_num ~ overlap_class)+
  theme_minimal()+
  labs(fill = "Intensity", title = "Spatial stranding concentration in 2020 by overlap categories") +
  theme_gray() #makes background grey

#Multiple years, average mean per pixel across years
  #average yearly ppm spatial prediction of each overlap class

yrs_mult <- 2017:2022

map_mean <- map_val %>%
  filter(Year_num %in% yrs_mult) %>%
  mutate(lon_r = round(lon, 3),
         lat_r = round(lat, 3)) %>%
  group_by(overlap_class, lon_r, lat_r) %>%
  summarise(mean_intensity = mean(intensity, na.rm = TRUE),
            n_years = n_distinct(Year_num),
            .groups = "drop")

map_mn <- ggplot(map_mean)+
  geom_tile(aes(x = lon_r, y = lat_r, fill = mean_intensity)) +
  facet_wrap(~overlap_class) +
  coord_equal()+
  #scale_fill_viridis_c(name = "Average Intensity") +
  scale_fill_distiller(palette = "YlOrRd", direction =1, name = "Average Intensity") +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") + #adds east coast outline to the map
  theme_minimal()+
  labs(title = "Average predicted spatial concentration from 2017-2022",
       x = "Longitude", y = "Latitude") +
  theme_gray()
  
yrs_mult1 <- 1996:2002

map_mean1 <- map_val %>%
  filter(Year_num %in% yrs_mult1) %>%
  mutate(lon_r = round(lon, 3),
         lat_r = round(lat, 3)) %>%
  group_by(overlap_class, lon_r, lat_r) %>%
  summarise(mean_intensity = mean(intensity, na.rm = TRUE),
            n_years = n_distinct(Year_num),
            .groups = "drop")

map_mn1 <- ggplot(map_mean1)+
  geom_tile(aes(x = lon_r, y = lat_r, fill = mean_intensity)) +
  facet_wrap(~overlap_class) +
  coord_equal()+
  #scale_fill_viridis_c(name = "Average Intensity") +
  scale_fill_distiller(palette = "YlOrRd", direction =1, name = "Average Intensity") +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") + #adds east coast outline to the map
  theme_minimal()+
  labs(title = "Average predicted spatial concentration from 1996-2002",
       x = "Longitude", y = "Latitude") +
  theme_gray()


library(patchwork)
(map_mn1 + map_mn) + plot_layout(ncol =1)


