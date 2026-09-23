sum_table <- read_excel("/Users/ebell23/Downloads/in6_grid25x25km_masttable.xlsx")
colnames(sum_table)

sum_table <- sum_table %>%
  replace_na(list(Closed_Area = 0, Gear_related = 0, TRP = 0, MPA = 0, NARW_specific = 0, Mysticeti = 0, Pinnipeds = 0, Odontocetes = 0, Unidentified = 0)) #replace all NA with 0s

library(dplyr)
library(purrr)

grids_map <- st_read("/Users/ebell23/Downloads/strand_in6_grid25x25km_.shp")

grids_uni <- grids_map %>% #makes it one grid per row instead of by year
  group_by(grid_id) %>%
  slice(1) %>%
  ungroup()

#1. pinnipeds & NARW specific 
results_11 <- map_dfr(unique(sum_table$grid_id), function(g){
  
  df <- sum_table %>%
    filter(grid_id == g)
  
  # Need at least 3 years of data
  if(nrow(df) < 3){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  test_11 <- tryCatch(
    cor.test(df$Pinnipeds,
             df$NARW_specific,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_11)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_11$estimate),
    p = test_11$p.value
  )
  
})

results_11 <- results_11 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_11 <- results_11 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_11 <- results_11 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_11 <- results_11 %>%
  mutate(significant = p < 0.05)

results_11 <- results_11 %>%
  mutate(grid_id = as.numeric(grid_id))

pin_narw_map <- grids_uni %>%
  left_join(results_11, by = c("grid_id")) 

narw_pin <- ggplot() +
  geom_sf(data = pin_narw_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(pin_narw_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outlne arounf statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0, 
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Pinnipeds & NARW policy correlation") +
  theme_void()

#7 Pinnipeds and TRP
results_12 <- map_dfr(unique(sum_table$grid_id), function(g){
  
  df <- sum_table %>%
    filter(grid_id == g)
  
  # Need at least 3 years of data
  if(nrow(df) < 3){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  test_12 <- tryCatch(
    cor.test(df$Pinnipeds,
             df$TRP,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_12)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_12$estimate),
    p = test_12$p.value
  )
  
})

results_12 <- results_12 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_12 <- results_12 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_12 <- results_12 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant'

results_12 <- results_12 %>%
  mutate(significant = p < 0.05)

results_12 <- results_12 %>%
  mutate(grid_id = as.numeric(grid_id))

pin_trp_map <- grids_uni %>%
  left_join(results_12, by = c("grid_id")) 

trp_pin <- ggplot() +
  geom_sf(data = pin_trp_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(pin_trp_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Pinnipeds & TRP policy correlation") +
  theme_void()



#Pinnipeds and Gear-related
results_13 <- map_dfr(unique(sum_table$grid_id), function(g){
  
  df <- sum_table %>%
    filter(grid_id == g)
  
  # Need at least 3 years of data
  if(nrow(df) < 3){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  test_13 <- tryCatch(
    cor.test(df$Pinnipeds,
             df$Gear_related,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_13)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_13$estimate),
    p = test_13$p.value
  )
  
})

results_13 <- results_13 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_13 <- results_13 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_13 <- results_13 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_13 <- results_13 %>%
  mutate(significant = p < 0.05)

results_13 <- results_13 %>%
  mutate(grid_id = as.numeric(grid_id))

pin_gr_map <- grids_uni %>%
  left_join(results_13, by = c("grid_id")) 

gear_pin <- ggplot() +
  geom_sf(data = pin_gr_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(pin_gr_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Pinnipeds & Gear-related policies correlation") +
  theme_void()



#4 Pinnipeds & MPA 
results_14 <- map_dfr(unique(sum_table$grid_id), function(g){
  
  df <- sum_table %>%
    filter(grid_id == g)
  
  # Need at least 3 years of data
  if(nrow(df) < 3){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  test_14 <- tryCatch(
    cor.test(df$Pinnipeds,
             df$MPA,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_14)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_14$estimate),
    p = test_14$p.value
  )
  
})

results_14 <- results_14 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_14 <- results_14 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_14 <- results_14 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_14 <- results_14 %>%
  mutate(significant = p < 0.05)

results_14 <- results_14 %>%
  mutate(grid_id = as.numeric(grid_id)) 

pin_mpa_map <- grids_uni %>%
  left_join(results_14, by = c("grid_id")) 

mpa_pin <- ggplot() +
  geom_sf(data = pin_mpa_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(pin_mpa_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Pinnipeds & MPAs correlation") +
  theme_void() 


#5 Pinnipeds & Closed Area
results_15 <- map_dfr(unique(sum_table$grid_id), function(g){
  
  df <- sum_table %>%
    filter(grid_id == g)
  
  # Need at least 3 years of data
  if(nrow(df) < 3){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  test_15 <- tryCatch(
    cor.test(df$Pinnipeds,
             df$Closed_area,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_15)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_15$estimate),
    p = test_15$p.value
  )
  
})

results_15 <- results_15 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_15 <- results_15 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_15 <- results_15 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_15 <- results_15 %>%
  mutate(significant = p < 0.05)

results_15 <- results_15 %>%
  mutate(grid_id = as.numeric(grid_id))

pin_ca_map <- grids_uni %>%
  left_join(results_15, by = c("grid_id")) 

ca_pin <- ggplot() +
  geom_sf(data = pin_ca_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(pin_ca_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Pinnipeds & Closed Areas correlation") +
  theme_void()


library(patchwork)
(trp_pin + narw_pin) / (gear_pin + mpa_pin)

(ca_odo + mpa_odo)

(trp_pin + narw_pin + gear_pin)/ (ca_pin + mpa_pin)

####Trying to put all plots together----

library(patchwork)
(trp_mys + trp_odo + trp_pin) / (narw_mys + narw_odo + narw_pin) / (gear_mys + gear_odo + gear_pin) / (mpa_mys + mpa_odo + mpa_pin)


(trp_mys + narw_mys + gear_mys + mpa_mys) / (trp_odo + narw_odo + gear_odo + mpa_odo) / (trp_pin + narw_pin + gear_pin +  mpa_pin)




#Adding all plots together - hard to see where the actual values are; add legend title----

  #add titles to columns by policy category
trp_mys_coln <- trp_mys + ggtitle("TRP")
narw_mys_coln <- narw_mys + ggtitle("NARW-specific")
gear_mys_coln <- gear_mys + ggtitle("Gear-related")
mpa_mys_coln <- mpa_mys + ggtitle("MPA")
ca_mys_coln <- ca_mys + ggtitle("Closed Area")

  #removing titles and axis labels from plots
clean_plot <- function(p) {
  p +
    labs(
      title = NULL,
      subtitle = NULL,
      x = NULL,
      y = NULL
    ) +
    theme(legend.position = "bottom")
}

trp_odo  <- clean_plot(trp_odo)
narw_odo <- clean_plot(narw_odo)
gear_odo <- clean_plot(gear_odo)
mpa_odo  <- clean_plot(mpa_odo)
ca_odo   <- clean_plot(ca_odo)

trp_pin  <- clean_plot(trp_pin)
narw_pin <- clean_plot(narw_pin)
gear_pin <- clean_plot(gear_pin)
mpa_pin  <- clean_plot(mpa_pin)
ca_pin   <- clean_plot(ca_pin)


  #row labels by taxonomic group
library(grid)
mys_label <- wrap_elements(
  textGrob("Mysticeti",
           rot = 90,
           gp = gpar(fontsize = 14,
                     fontface = "bold")))

odo_label <- wrap_elements(
  textGrob("Odontoceti",
           rot = 90,
           gp = gpar(fontsize = 14,
                     fontface = "bold")))

pin_label <- wrap_elements(
  textGrob("Pinnipeds",
           rot = 90,
           gp = gpar(fontsize = 14,
                     fontface = "bold")))


final_plot <- 
  (mys_label | trp_mys_coln | narw_mys_coln | gear_mys_coln | mpa_mys_coln | ca_mys_coln)/
  (odo_label | trp_odo | narw_odo | gear_odo | mpa_odo | ca_odo)/
  (pin_label | trp_pin | narw_pin | gear_pin | mpa_pin | ca_pin) #with labels

final_plot <- 
  (trp_mys | narw_mys | gear_mys | mpa_mys | ca_mys)/
  (trp_odo | narw_odo | gear_odo | mpa_odo | ca_odo)/
  (trp_pin | narw_pin | gear_pin | mpa_pin | ca_pin) #without labels


final_plot +
  plot_layout(
    widths = c(1, 1, 1, 1, 1), #previously had 0.08,1,1,1,1,1 when labels are added
    guides = "collect"
  ) &
  plot_annotation(title = "Spatial correlations between strandings and policy categories by taxonomic group") & 
  theme(
    legend.position = "bottom"
  )

print(final_plot)
