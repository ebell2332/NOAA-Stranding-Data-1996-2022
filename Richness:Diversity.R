library(dplyr)
library(tidyr)
library(vegan)    # specnumber() for richness, diversity() for Shannon
library(broom)    # tidy model outputs

#Load in dataset
final_set <- read.csv(file = "/Users/ebell23/Downloads/AP_Data/final_set.csv")

###Species Diversity and Shannon Index by Species####

data_filtered <- final_set %>%
  filter(!is.na(species)) %>%
  mutate(species = trimws(tolower(species)))  # standardize species names


#Species Diversity per time window
#counts per Year x Species
sp_year_comm <- data_filtered %>%
  count(year_of_observation, species, name = "count") %>%
  tidyr::pivot_wider(names_from = species, values_from = count, values_fill = 0)

sp_year_matrix <- sp_year_comm %>% select(-year_of_observation) %>% as.data.frame()

sp_year_div <- tibble(
  year_of_observation = sp_year_comm$year_of_observation,
  richness = vegan::specnumber(sp_year_matrix),
  shannon  = vegan::diversity(sp_year_matrix, index = "shannon"),
  n_records = rowSums(sp_year_matrix)
)

print(year_div)
#write.csv(year_div, "/Users/ebell23/Downloads/yearly_diversity.csv", row.names = FALSE)


##Plotting Shannon Idex by HI
ggplot(sp_year_div, aes(x = year_of_observation, y = shannon)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Year of Observation",
    y = "Shannon Diversity (H')",
    title = "Shannon Diversity of Species Stranded by Year"
  )

sp_lm_trend <- lm(shannon ~ year_of_observation, data = sp_year_div)
summary(sp_lm_trend)



###Species Diversity and Shannon Index by Stranding Type####

hi_data <- final_set %>%
  select(year_of_observation, shot, boat_collision, fishery_interaction, other_human_interaction) %>%
  pivot_longer(
    cols = c(shot, boat_collision, fishery_interaction, other_human_interaction),
    names_to = "Interaction_Type",
    values_to = "HI"
  )

hi_long <- hi_data %>%
  mutate(
    # normalize to character once (handles factors too), lowercase for matching
    HI_chr = tolower(as.character(HI)),
    
    # build a single logical from character (and numeric-like) values
    is_positive = case_when(
      # common "yes" tokens → TRUE
      HI_chr %in% c("y","yes","true","present","1") ~ TRUE,
      
      # numeric-like strings (e.g., "1", "2") → TRUE if > 0
      !is.na(suppressWarnings(as.numeric(HI_chr))) &
        as.numeric(HI_chr) > 0 ~ TRUE,
      
      # common "no" tokens → FALSE
      HI_chr %in% c("n","no","false","absent","0") ~ FALSE,
      
      # everything else → FALSE (or NA if you prefer)
      TRUE ~ FALSE
    )
  ) %>%
  filter(is_positive)

# 2B) Count per year × interaction type (abundances)
year_type_counts <- hi_long %>%
  count(year_of_observation, Interaction_Type, name = "n")

# 2C) Wide “community” matrix (rows = years, cols = interaction types)
year_comm <- year_type_counts %>%
  pivot_wider(names_from = Interaction_Type, values_from = n, values_fill = 0)

hi_year_matrix <- year_comm %>%
  select(-year_of_observation) %>%
  as.data.frame()

# 2D) Diversity metrics by year
hi_year_div <- tibble(
  year_of_observation = year_comm$year_of_observation,
  richness = specnumber(hi_year_matrix),                       # number of interaction types observed
  shannon  = diversity(hi_year_matrix, index = "shannon"),     # Shannon diversity across types
  n_records = rowSums(hi_year_matrix)                          # total positive HI records that year
)

print(hi_year_div)

##Plotting Shannon Idex by HI
ggplot(hi_year_div, aes(x = year_of_observation, y = shannon)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Year of Observation",
    y = "Shannon Diversity (H')",
    title = "Shannon Diversity of Human Interaction Types in Strandings by Year"
  )
##Statistical Test 
lm_trend <- lm(shannon ~ year_of_observation, data = year_div)
summary(lm_trend)
