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

#1. odontocetes & NARW specific 
results_6 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_6 <- tryCatch(
    cor.test(df$Odontocetes,
             df$NARW_specific,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_6)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_6$estimate),
    p = test_6$p.value
  )
  
})

results_6 <- results_6 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_6 <- results_6 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_6 <- results_6 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_6 <- results_6 %>%
  mutate(significant = p < 0.05)

results_6 <- results_6 %>%
  mutate(grid_id = as.numeric(grid_id))

odo_narw_map <- grids_uni %>%
  left_join(results_6, by = c("grid_id")) 

narw_odo <- ggplot() +
  geom_sf(data = odo_narw_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(odo_narw_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outlne arounf statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0, 
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Odontocetes & NARW policy correlation") +
  theme_void()



#7 Odontocetes and TRP
results_7 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_7 <- tryCatch(
    cor.test(df$Odontocetes,
             df$TRP,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_7)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_7$estimate),
    p = test_7$p.value
  )
  
})

results_7 <- results_7 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_7 <- results_7 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_7 <- results_7 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant'

results_7 <- results_7 %>%
  mutate(significant = p < 0.05)

results_7 <- results_7 %>%
  mutate(grid_id = as.numeric(grid_id))

odo_trp_map <- grids_uni %>%
  left_join(results_7, by = c("grid_id")) 

trp_odo <- ggplot() +
  geom_sf(data = odo_trp_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(odo_trp_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Odontocetes & TRP policy correlation") +
  theme_void()



#Odontocetes and Gear-related
results_8 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_8 <- tryCatch(
    cor.test(df$Odontocetes,
             df$Gear_related,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_8)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_8$estimate),
    p = test_8$p.value
  )
  
})

results_8 <- results_8 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_8 <- results_8 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_8 <- results_8 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_8 <- results_8 %>%
  mutate(significant = p < 0.05)

results_8 <- results_8 %>%
  mutate(grid_id = as.numeric(grid_id))

odo_gr_map <- grids_uni %>%
  left_join(results_8, by = c("grid_id")) 

gear_odo <- ggplot() +
  geom_sf(data = odo_gr_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(odo_gr_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Odontocetes & Gear-related policies correlation") +
  theme_void()



#4 Odontocetes & MPA 
results_9 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_9 <- tryCatch(
    cor.test(df$Odontocetes,
             df$MPA,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_9)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_9$estimate),
    p = test_9$p.value
  )
  
})

results_9 <- results_9 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_9 <- results_9 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_9 <- results_9 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_9 <- results_9 %>%
  mutate(significant = p < 0.05)

results_9 <- results_9 %>%
  mutate(grid_id = as.numeric(grid_id)) 

odo_mpa_map <- grids_uni %>%
  left_join(results_9, by = c("grid_id")) 

mpa_odo <- ggplot() +
  geom_sf(data = odo_mpa_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(odo_mpa_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Odontocetes & MPAs correlation") +
  theme_void() 


#5 Odontocetes & Closed Area
results_10 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_10 <- tryCatch(
    cor.test(df$Odontocetes,
             df$Closed_area,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_10)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_10$estimate),
    p = test_10$p.value
  )
  
})

results_10 <- results_10 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_10 <- results_10 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_10 <- results_10 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_10 <- results_10 %>%
  mutate(significant = p < 0.05)

results_10 <- results_10 %>%
  mutate(grid_id = as.numeric(grid_id))

odo_ca_map <- grids_uni %>%
  left_join(results_10, by = c("grid_id")) 

ca_odo <- ggplot() +
  geom_sf(data = odo_ca_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(odo_ca_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Odontocetes & Closed Areas correlation") +
  theme_void()


library(patchwork)
(trp_odo + narw_odo) / (gear_odo + mpa_odo)

(ca_odo + mpa_odo)

(trp_odo + narw_odo + gear_odo)/(ca_odo + mpa_odo)
