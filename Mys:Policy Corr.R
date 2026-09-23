
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

#1. mysticeti & NARW specific 
results_1 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_1 <- tryCatch(
    cor.test(df$Mysticeti,
             df$NARW_specific,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_1)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_1$estimate),
    p = test_1$p.value
  )
  
})

results_1 <- results_1 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_1 <- results_1 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_1 <- results_1 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_1 <- results_1 %>%
  mutate(significant = p < 0.05)

results_1 <- results_1 %>%
  mutate(grid_id = as.numeric(grid_id))

mys_narw_map <- grids_uni %>%
  left_join(results_1, by = c("grid_id")) 

narw_mys <- ggplot() +
  geom_sf(data = mys_narw_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(mys_narw_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outlne arounf statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0, 
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Mysticeti & NARW policy correlation") +
  theme_void()


#2. mysticeti & TRP
results_2 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_2 <- tryCatch(
    cor.test(df$Mysticeti,
             df$TRP,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_2)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_2$estimate),
    p = test_2$p.value
  )
  
})

results_2 <- results_2 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_2 <- results_2 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_2 <- results_2 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant'

results_2 <- results_2 %>%
  mutate(significant = p < 0.05)

results_2 <- results_2 %>%
  mutate(grid_id = as.numeric(grid_id))

mys_trp_map <- grids_uni %>%
  left_join(results_2, by = c("grid_id")) 

trp_mys <- ggplot() +
  geom_sf(data = mys_trp_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(mys_trp_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Mysticeti & TRP policy correlation") +
  theme_void()

#3 mysticeti & gear_related
results_3 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_3 <- tryCatch(
    cor.test(df$Mysticeti,
             df$Gear_related,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_3)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_3$estimate),
    p = test_3$p.value
  )
  
})

results_3 <- results_3 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_3 <- results_3 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_3 <- results_3 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_3 <- results_3 %>%
  mutate(significant = p < 0.05)

results_3 <- results_3 %>%
  mutate(grid_id = as.numeric(grid_id))

mys_gr_map <- grids_uni %>%
  left_join(results_3, by = c("grid_id")) 

gear_mys <- ggplot() +
  geom_sf(data = mys_gr_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(mys_gr_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Mysticeti & Gear-related policies correlation") +
  theme_void()
  

  
  #4 Mysticeti & MPA - results showing no statistical significance
  results_4 <- map_dfr(unique(sum_table$grid_id), function(g){
    
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
    
    test_4 <- tryCatch(
      cor.test(df$Mysticeti,
               df$MPA,
               method = "spearman"),
      error = function(e) NULL
    )
    
    if(is.null(test_4)){
      return(tibble(
        grid_id = g,
        cor = NA,
        p = NA
      ))
    }
    
    tibble(
      grid_id = g,
      cor = unname(test_4$estimate),
      p = test_4$p.value
    )
    
  })

results_4 <- results_4 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_4 <- results_4 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_4 <- results_4 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_4 <- results_4 %>%
  mutate(significant = p < 0.05)

results_4 <- results_4 %>%
  mutate(grid_id = as.numeric(grid_id)) 

mys_mpa_map <- grids_uni %>%
  left_join(results_4, by = c("grid_id")) 

mpa_mys <- ggplot() +
  geom_sf(data = mys_mpa_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(mys_mpa_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Mysticeti & MPAs correlation") +
  theme_void() 


#5 Mysticeti & Closed Area
results_5 <- map_dfr(unique(sum_table$grid_id), function(g){
  
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
  
  test_5 <- tryCatch(
    cor.test(df$Mysticeti,
             df$Closed_area,
             method = "spearman"),
    error = function(e) NULL
  )
  
  if(is.null(test_5)){
    return(tibble(
      grid_id = g,
      cor = NA,
      p = NA
    ))
  }
  
  tibble(
    grid_id = g,
    cor = unname(test_5$estimate),
    p = test_5$p.value
  )
  
})

results_5 <- results_5 %>%
  mutate(
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

results_5 <- results_5 %>%
  mutate(
    cor_label = paste0(round(cor, 2), sig)
  )

results_5 <- results_5 %>%
  mutate(cor_sig = ifelse(p < .05, cor, NA_real_)) #noted correlations that are statistically significant

results_5 <- results_5 %>%
  mutate(significant = p < 0.05)

results_5 <- results_5 %>%
  mutate(grid_id = as.numeric(grid_id))

mys_ca_map <- grids_uni %>%
  left_join(results_5, by = c("grid_id")) 

ca_mys <- ggplot() +
  geom_sf(data = mys_ca_map, aes(fill = cor), color = NA) + #fill every grid by correlation coefficient
  geom_sf(data = subset(mys_ca_map, significant), fill = NA, color = "red", linewidth = 0.5) + #draw outline around statistically significant grids only
  scale_fill_gradient2(
    midpoint = 0,
    na.value = "grey90",
    name = "correlation") +
  geom_sf(data = east_coast, color = "black", fill = NA) +
  labs(title = "Mysticeti & Closed Areas correlation") +
  theme_void()




library(patchwork)
(trp_mys + narw_mys) / (gear_mys)
  
(ca_mys + mpa_mys)


(trp_mys + narw_mys + gear_mys) / (ca_mys + mpa_mys)