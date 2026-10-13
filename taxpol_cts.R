sum_table <- read.csv("/Users/ebell23/Downloads/25x25_master_table.csv")
colnames(sum_table)

sum_table <- sum_table %>%
  replace_na(list(Closed_Area = 0, Gear_related = 0, TRP = 0, MPA = 0, NARW_specific = 0, Mysticeti = 0, Pinnipeds = 0, Odontocetes = 0, Unidentified = 0)) #replace all NA with 0s

#save sum_table with 0s instead of 1s -- saved
sum_table$Closed_Area <- as.integer(sum_table$Closed_Area) #changing closed areas information from true/false to 0/1

write.csv(
  sum_table,
  file = "/Users/ebell23/Downloads/25x25_mast_table.csv",
  row.names = FALSE
)
  #new file name is 25x25_master_table

library(dplyr)
library(purrr)

#Reading in the grid maps that were made in ArcGIS 
grids_map <- st_read("/Users/ebell23/Downloads/strand_in6_grid25x25km_.shp")

grids_uni <- grids_map %>% #makes it one grid per row instead of by year
  group_by(grid_id) %>%
  slice(1) %>%
  ungroup()

#Using tigris to get updated map of the US states to add to the map 
#only mapping specific states (east coast states from maine to virginia)
library(tigris)
states <- tigris::states(cb = TRUE) %>%
  sf::st_transform(4326)

east_coast <- states %>%
  filter(NAME %in% c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island","Connecticut", "New York", "New Jersey", "Pennsylvania", "Delaware", "Maryland", "District of Columbia", "Virginia")
  )

#A. Mapping each Taxonomic group by count and grid_id----

#1. All taxonomic groups together using facet wrap
#mapping each taxonomic group by count and using facet wrap with a single legend

tax_long <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    across(
      c(Odontocetes, Mysticeti, Pinnipeds),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      Odontocetes, Mysticeti, Pinnipeds
    ),
    names_to = "taxon_group",
    values_to = "tax_count"
  )

tax_long <- tax_long %>%
  mutate(grid_id = as.numeric(grid_id))

map_tax_ct <- grids_uni %>%
  left_join(tax_long, by = "grid_id")


ggplot(map_tax_ct) +
  geom_sf(
    aes(fill = tax_count),
    color = NA
  ) +
  facet_wrap(~ taxon_group) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Number of strandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black"
  ) +
  theme_void()



  #1 map of all stranding counts
str_all <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    strand_count = sum(
      Mysticetes, Odontocetes, Pinnipeds,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

str_all <- str_all %>%
  mutate(grid_id = as.numeric(grid_id))

map_str_all <- grids_uni %>%
  left_join(str_all, by = "grid_id")


str_map <- ggplot() +
  geom_sf(data = map_str_all, 
          aes(fill = strand_count),
          color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "Number of strandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()


#1a. mapping counts by taxonomic group
#Odontocetes

odon_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    Odontocetes = sum(Odontocetes, na.rm = TRUE),
    .groups = "drop"
  )

odon_map_ct <- odon_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

odo_ct_data <- grids_uni %>%
  left_join(odon_map_ct, by = "grid_id")


odon_ct <- ggplot() +
  geom_sf(
    data = odo_ct_data,
    aes(fill = Odontocetes),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "Odontocetes\nstrandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()

#Mysticeti
mys_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    Mysticetes = sum(Mysticetes, na.rm = TRUE),
    .groups = "drop"
  )

mys_map_ct <- mys_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

mys_ct_data <- grids_uni %>%
  left_join(mys_map_ct, by = "grid_id")


mys_ct <- ggplot() +
  geom_sf(
    data = mys_ct_data,
    aes(fill = Mysticetes),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "Mysticetes\nstrandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()

#Pinnipeds
pin_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    Pinnipeds = sum(Pinnipeds, na.rm = TRUE),
    .groups = "drop"
  )

pin_map_ct <- pin_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

pin_ct_data <- grids_uni %>%
  left_join(pin_map_ct, by = "grid_id")


pin_ct <- ggplot() +
  geom_sf(
    data = pin_ct_data,
    aes(fill = Pinnipeds),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "Pinnipeds\nstrandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()


library(patchwork)

odon_ct + mys_ct + pin_ct

#B. Mapping each policy by count and grid_id----

#1. NARW-specific

narw_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    NARW_policies = sum(NARW_specific, na.rm = TRUE),
    .groups = "drop"
  )

narw_map_ct <- narw_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

map_ct_data <- grids_uni %>%
  left_join(narw_map_ct, by = "grid_id")


narw_ct <- ggplot() +
  geom_sf(
    data = map_ct_data,
    aes(fill = NARW_policies),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "NARW-specific\npolicies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()


 
#TRP
trp_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    TRP = sum(TRP, na.rm = TRUE),
    .groups = "drop"
  )

trp_map_ct <- trp_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

trpmap_ct_data <- grids_uni %>%
  left_join(trp_map_ct, by = "grid_id")


trp_ct <- ggplot() +
  geom_sf(
    data = trpmap_ct_data,
    aes(fill = TRP),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "TRP\npolicies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()



#Gear-related
gear_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    Gear_related = sum(Gear_related, na.rm = TRUE),
    .groups = "drop"
  )

gear_map_ct <- gear_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

grmap_ct_data <- grids_uni %>%
  left_join(gear_map_ct, by = "grid_id")


gr_ct <- ggplot() +
  geom_sf(
    data = grmap_ct_data,
    aes(fill = Gear_related),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "Gear-related\npolicies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()



#MPA
mpa_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    MPA = sum(MPA, na.rm = TRUE),
    .groups = "drop"
  )

mpa_map_ct <- mpa_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

mpamap_ct_data <- grids_uni %>%
  left_join(mpa_map_ct, by = "grid_id")


mpa_ct <- ggplot() +
  geom_sf(
    data = mpamap_ct_data,
    aes(fill = MPA),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "MPA\npolicies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()



#Closed Area -- update scale manually to be whole numbers
ca_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    Closed_Area = sum(Closed_Area, na.rm = TRUE),
    .groups = "drop"
  )

ca_map_ct <- ca_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

camap_ct_data <- grids_uni %>%
  left_join(ca_map_ct, by = "grid_id")


ca_ct <- ggplot() +
  geom_sf(
    data = camap_ct_data,
    aes(fill = Closed_Area),
    color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    breaks = c(0, 5, 10),
    name = "Closed Area\npolicies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()




library(patchwork)
(narw_ct + trp_ct + gr_ct) / (mpa_ct + ca_ct)


#2. Mapping all policy counts together using facet wrap
#separate maps for each policy category using facet wrap, all with a single scale legend

library(tidyr)

policy_long <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    across(
      c(Closed_Area, Gear_related, TRP, MPA, NARW_specific),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      Closed_Area,
      Gear_related,
      TRP,
      MPA,
      NARW_specific
    ),
    names_to = "policy_type",
    values_to = "policy_count"
  )

policy_long <- policy_long %>%
  mutate(grid_id = as.numeric(grid_id))

map_policy_ct <- grids_uni %>%
  left_join(policy_long, by = "grid_id")


ggplot() +
  geom_sf(data = map_policy_ct, 
    aes(fill = policy_count),
    color = NA
  ) +
  facet_wrap(~ policy_type) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "Number of policies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()



  #1 map of policy counts together

policy_all <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    policy_count = sum(
      Closed_Area + Gear_related + TRP + MPA + NARW_specific,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

policy_all <- policy_all %>%
  mutate(grid_id = as.numeric(grid_id))

map_policy_all <- grids_uni %>%
  left_join(policy_all, by = "grid_id")


policy_map <- ggplot() +
  geom_sf(data = map_policy_all, 
          aes(fill = policy_count),
          color = NA
  ) +
  scale_fill_gradient(
    low = "plum",
    high = "purple3",
    name = "Number of policies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "grey90"
  ) +
  theme_void()


#map of stranding counts and map of policy counts
library(patchwork)
str_map + policy_map



#C. Correlation matrices----
corr_data <- sum_table[, c("Mysticeti", "Odontocetes", "Pinnipeds", "Unidentified", "MPA", "Gear_related", "NARW_specific", "TRP", "Closed_Area")] #excludes Year

corr_data <- data.frame(lapply(corr_data, as.numeric)) #makes sure all numbers are numeric

cor_matrix <- cor(corr_data, use = "complete.obs", method = "spearman") 

cor_matrix

p_matrix <- matrix(
  NA,
  nrow = ncol(corr_data),
  ncol = ncol(corr_data)
)

colnames(p_matrix) <- colnames(corr_data)
rownames(p_matrix) <- colnames(corr_data)

for (i in 1:ncol(corr_data)) {
  for (j in 1:ncol(corr_data)) {
    
    test <- cor.test(
      corr_data[[i]],
      corr_data[[j]],
      method = "spearman",
      exact = FALSE
    )
    
    p_matrix[i, j] <- test$p.value
  }
}

sig_matrix <- ifelse(
      p_matrix < 0.05, "*",
      "")

cor_sig_matrix <- matrix(
  paste0(round(cor_matrix, 2), sig_matrix),
  nrow = nrow(cor_matrix),
  dimnames = dimnames(cor_matrix)
)

cor_sig_matrix



  #correlation matrix of strandings and policies by year (not of each type or group)
    #1. grouping policies by year and grid id and combining all policy categories
policy_tbl <- sum_table %>%
  group_by(grid_id, Year_num) %>%
  summarise(
    policy_count = sum(
      Closed_Area + Gear_related + TRP + MPA + NARW_specific,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) 
  #2. grouping all strandings by year and grid id and combining all stranding groups
str_tbl <- sum_table %>%
  group_by(grid_id, Year_num) %>%
  summarise(
    strand_count = sum(
      Mysticetes, Odontocetes, Pinnipeds, Unidentified,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

str_data <- str_tbl %>% #remove sf/geometry for joining
  st_drop_geometry()

pol_data <- policy_tbl %>% #remove sf/geometry for joining
  st_drop_geometry()

combined_data <- str_data %>%
  inner_join(
    pol_data,
    by = c("grid_id", "Year_num")) #double check number of rows for each policy and strandings data with new combined data


cor_by_year <- combined_data %>%
  group_by(Year_num) %>%
  summarise(
    correlation = cor(
      strand_count,
      policy_count,
      method = "spearman",
      use = "complete.obs"
    ),
    .groups = "drop"
  )

cor_by_year #need to add p-value and significance

  #simple line plot of correlaion values over time
  ggplot(data = cor_by_year, aes(x = Year_num, y = correlation)) + 
    geom_line(linetype = 2) +
    theme_classic()




