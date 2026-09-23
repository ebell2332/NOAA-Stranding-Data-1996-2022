sum_table <- read_excel("/Users/ebell23/Downloads/in6_grid25x25km_masttable.xlsx")
colnames(sum_table)

sum_table <- sum_table %>%
  replace_na(list(Closed_Area = 0, Gear_related = 0, TRP = 0, MPA = 0, NARW_specific = 0, Mysticeti = 0, Pinnipeds = 0, Odontocetes = 0, Unidentified = 0)) #replace all NA with 0s

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

#Mapping each Taxonomic group by count and grid_id

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

#1a. mapping counts by taxonomic grou
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
    low = "lightblue",
    high = "darkblue",
    name = "Odontocetes\nstrandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black"
  ) +
  theme_void()

#Mysticeti
mys_map_ct <- sum_table %>%
  group_by(grid_id) %>%
  summarise(
    Mysticeti = sum(Mysticeti, na.rm = TRUE),
    .groups = "drop"
  )

mys_map_ct <- mys_map_ct %>%
  mutate(grid_id = as.numeric(grid_id))

mys_ct_data <- grids_uni %>%
  left_join(mys_map_ct, by = "grid_id")


mys_ct <- ggplot() +
  geom_sf(
    data = mys_ct_data,
    aes(fill = Mysticeti),
    color = NA
  ) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Mysticeti\nstrandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black"
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
    low = "lightblue",
    high = "darkblue",
    name = "Pinnipeds\nstrandings",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black"
  ) +
  theme_void()


library(patchwork)

odon_ct + mys_ct + pin_ct

#Mapping each policy by count and grid_id

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


ggplot() +
  geom_sf(
    data = map_ct_data,
    aes(fill = NARW_policies),
    color = NA
  ) +
  scale_fill_viridis_c(
    name = "NARW-specific\npolicies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black"
  ) +
  theme_void()



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


ggplot(map_policy_ct) +
  geom_sf(
    aes(fill = policy_count),
    color = NA
  ) +
  facet_wrap(~ policy_type) +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Number of policies",
    na.value = "grey90"
  ) +
  geom_sf(
    data = east_coast,
    fill = NA,
    color = "black"
  ) +
  theme_void()


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



# Install and load the package
write.csv(cor_sig_matrix, "my_output.csv", row.names = FALSE)



