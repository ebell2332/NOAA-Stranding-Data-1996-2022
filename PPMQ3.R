q3 <- read_excel("/Users/ebell23/Downloads/minimpl_str_dirpol_TableToExcel.xlsx")


study_sf <- st_read("/Users/ebell23/Downloads/Study_Area1_ExportFeatures.shp", quiet = TRUE) 
st_crs(study_sf) #check coordinate system
st_bbox(study_sf) #check geometry of study area
W_q3 <- as.owin(study_sf)

#check points and crs with study area
q3_sf <- st_as_sf(q3, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
q3_sf <- st_transform(q3_sf, st_crs(study_sf))
st_crs(q3_sf)

#fit all points within the study window
coords <- st_coordinates(q3_sf) #coordinates
inside <- spatstat.geom::inside.owin(coords[,1], coords[,2], W_q3) #points inside the window

q3_sf3  <- q3_sf[inside, ]
coords3 <- coords[inside, , drop = FALSE]
nrow(q3_sf3)

q3_sf3 %>% st_drop_geometry() %>% #how many strandings per event_time bin
  count(event_time) %>%
  arrange(event_time) %>%
  print(n = 100)

q3_sf3 %>% st_drop_geometry() %>%
  summarise(
    n_total = n(),
    n_event_na = sum(is.na(event_time)),
    n_unique = n_distinct(event_time, na.rm = TRUE),
    min_et = min(event_time, na.rm = TRUE),
    max_et = max(event_time, na.rm = TRUE)
  )

q3_sf3 %>% st_drop_geometry() %>%
  summarise(
    n_total = n(),
    n_post = sum(event_time >= 1, na.rm = TRUE),
    min_event = min(event_time, na.rm = TRUE),
    max_event = max(event_time, na.rm = TRUE)
  )

#1. Run Model For Event Time ####

#create event-time bins
q3_sf3 <- q3_sf3 %>%
mutate(event_bin = case_when(
event_time <= -1 ~"Pre",
event_time == 0 ~ "Impl",
event_time >= 1 ~ "Post",
TRUE ~NA_character_
))


q3_sf3 <- q3_sf3 %>%
  mutate(event_bin = case_when(
   event_time <= -6 ~"Pre_far",
    event_time >= -5 & event_time <= -1 ~ "Pre_near",
   event_time == 0 ~"Impl",
   event_time >= 1 & event_time <= 5 ~ "Post_near",
   event_time >= 6 ~"Post_far",
    TRUE ~NA_character_
 ))
    
class(q3_sf3$event_bin)
unique(q3_sf3$event_bin)

cv_event_q3 <- function(sf_data3, W, bin_val, min_n = 50) {
  
  sub <- sf_data3 %>%
    filter(event_bin == bin_val)
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- spatstat.geom::ppp(xy[,1], xy[,2], window = W)
  
  fit <- spatstat.model::ppm(X ~ x + y)
  
  Z <- as.numeric(predict(fit))
  
  tibble(
    event_bin = bin_val,
    n = n,
    cv_intensity = sd(Z) / mean(Z)
  )
}

bins <- c("Pre_far", "Pre_near", "Impl", "Post_near", "Post_far")

results_q3 <- list()


for (b in bins) {
  res <- cv_event_q3(q3_sf3, W_q3, b, min_n = 50)
  if (!is.null(res)) results_q3[[length(results_q3) + 1]] <- res
}

cv_q3_df <- dplyr::bind_rows(results_q3) %>%
  mutate(event_bin = factor(event_bin, levels = c("Pre_far", "Pre_near", "Impl", "Post_near", "Post_far")))

cv_q3_df <- bind_rows(results_q3)
cv_q3_df


bin_levels <- c("Pre_far", "Pre_near", "Impl", "Post_near", "Post_far")

cv_q3_df <- cv_q3_df %>%
  mutate(event_bin = factor(event_bin, levels = bin_levels)) #reorder by bins instead of default

#CV Plot
ggplot(cv_q3_df, aes(x = event_bin, y = cv_intensity)) +
  geom_col(alpha = 0.7, fill = "steelblue") +
  labs(
    x = "Implementation period",
    y = "CV of spatial concentration",
    title = "Spatial concentration of stranding relative to direct-only implemented policies",
    subtitle = "Before and after policy implementation") +
theme(
  strip.background = element_rect(fill = "white", color = "white"),  #facet wrap background and color
  strip.text = element_text(color = "black", size = 10,face = "italic"), #facet wrap text size and font
  plot.title = element_text(color = "black", size = 18),# Title color and size
  plot.subtitle = element_text(size = 15), 
  axis.title.x = element_text(color = "black", size = 14),   # X-axis title
  axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
  axis.text = element_text(size = 10)      # Axis tick labels
) 


####Run with taxonomic group####
q3_sf3 %>% #checking numbers of strandings by tax group before running
  st_drop_geometry() %>%
  count(tax_group, event_bin, name = "n") %>%
  tidyr::pivot_wider(names_from = event_bin, values_from = n, values_fill = 0)

q3_sf3 <- q3_sf3 %>%
  mutate(event_bin = case_when(
    event_time < 0 ~"Pre",
    event_time > 0  ~ "Post",
    TRUE ~NA_character_
  ))


taxon_gp <- "tax_group"  #new name for column 

cv_q3_tax <- function(sf_data4, W, bin_val, taxon_val, min_n = 40) 
  #min_n = 40 because taxon group reduces sample size and it helps to account for the smaller numbers per year
{
  sub <- sf_data4 %>%
    filter(
      event_bin == bin_val,
      .data[[taxon_gp]] == taxon_val
    )
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- spatstat.geom::ppp(xy[,1], xy[,2], window = W)
  
  fit <- spatstat.model::ppm(X ~ x + y)
  
  Z <- as.numeric(predict(fit))
  
  tibble(
    event_bin = bin_val,
    taxon_gp = taxon_val,
    n = n,
    cv_intensity = sd(Z) / mean(Z)
  )
}

taxa <- sort(unique(q3_sf3$tax_group)) #sorting by taxonomic group
bins <- c("Pre", "Post")

results_q3_tax <- list()

for (b in bins) {
    for (tx in taxa) {
      
      res <- cv_q3_tax(
        sf_data4 = q3_sf3,
        W = W_q3,
        bin_val = b,
        taxon_val = tx,
        min_n = 40 #minimum of 20 strandings in this year x overlap class x tax group
      )
      
      if (!is.null(res)) {
        results_q3_tax[[length(results_q3_tax) + 1]] <- res
      }
    }
}

cv_q3_tax <- bind_rows(results_q3_tax)
cv_q3_tax
  
bin_levels <- c("Pre", "Post")
cv_q3_tax <- cv_q3_tax %>%
  mutate(event_bin = factor(event_bin, levels = bin_levels)) #reorder by bins instead of default
  


#CV by taxonomic group
ggplot(cv_q3_tax, aes(x = event_bin, y = cv_intensity, fill = event_bin)) +
  geom_col(alpha = 0.7) +
  facet_wrap(~ taxon_gp, scales = "free_y") +
  labs(
    x = "Implementation period",
    y = "CV of spatial concentration",
    title = "Stranding spatial concentration by taxonomic group and implementation period"
  ) +
  theme(
    strip.background = element_rect(fill = "white", color = "white"),  #facet wrap background and color
    strip.text = element_text(color = "black", size = 10,face = "italic"), #facet wrap text size and font
    plot.title = element_text(color = "black", size = 15),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10)      # Axis tick labels
  ) +
  theme(legend.position = "none")
  
  