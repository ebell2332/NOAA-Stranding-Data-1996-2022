

policy_data <- read_excel(path = "/Users/ebell23/Downloads/policy_merge01.xls")

colnames(policy_data)
####Diversity of Policy Types####
##1. Prep and Clean Policy Data 
policy_clean <- policy_data %>%
  mutate(
    # coalesce duplicate name variants
    ImplemDt = coalesce(ImplemDt, ImplDt),
    EffectDt = coalesce(EffectDt, EffDt),
    DeactDt  = coalesce(DeactDt, DeacDt),
    # parse any that are character or numeric serials
    ImplemDt = as.Date(ImplemDt, origin = "1899-12-30"),
    EffectDt = as.Date(EffectDt, origin = "1899-12-30"),
    DeactDt  = as.Date(DeactDt, origin = "1899-12-30"),
    # fallback effective year if missing
    StartYear = year(coalesce(EffectDt, ImplemDt)),
    EndYear   = if_else(!is.na(DeactDt), year(DeactDt), NA_integer_),
    Continuous = coalesce(ContinPol == 1, FALSE)
  ) %>%
  filter(!is.na(PolicType), !is.na(StartYear))

##2. Expand to yearly records of active policies
# determine full study year range (adjust as needed)
study_years <- sort(unique(sp_year_div$year_of_observation)) #make sure study_years is actual years

##3. expand each policy row into one row per active year

policy_years <- policy_clean %>%
  rowwise() %>%
  mutate(
    to_year = ifelse(is.na(EndYear), max(study_years), EndYear),  #default end to the deactivate year or end of study
    ActiveYears = list({ #sequence of active years during the study period
      yrs <- if (Continuous) study_years else seq.int(StartYear, to_year)
      intersect(yrs, study_years)
    })
  ) %>%
  ungroup() %>%
  select(-to_year) %>%
  tidyr::unnest(ActiveYears, keep_empty = FALSE) %>%
  mutate(ActiveYears = as.integer(ActiveYears)) #force to stay as real years not indices

 #Community Table
policy_comm <- policy_years %>%
  count(ActiveYears, PolicType, name = "Count") %>%
  tidyr::pivot_wider(names_from=PolicType, values_from = Count, values_fill = 0)

##4. Year x Policy Type Community Table and Diversity
policy_matrix <- policy_comm %>% dplyr::select(-ActiveYears) %>% as.matrix()

policy_div <- tibble::tibble(
  year_of_observation = as.integer(policy_comm$ActiveYears),
  policy_richness = specnumber(policy_matrix),
  policy_shannon  = diversity(policy_matrix, index = "shannon"),
  total_policies  = rowSums(policy_matrix)
)

print(policy_div)


##5. Plotting Shannon Idex by Policy Type
ggplot(policy_div, aes(x = year_of_observation, y = policy_shannon)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Active Years",
    y = "Shannon Diversity (H')",
    title = "Shannon Diversity of Policy Types during Active Years of Policy Implementation"
  )


ggplot(policy_comm, aes(x = PolicType, y = ActiveYears))+
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  theme_minimal(base_size = 13) 

pol_lm_trend <- lm(policy_shannon ~ year_of_observation, data = policy_div)
summary(pol_lm_trend)



#####Policy and Stranding Data by Year Diversity Indices####
#1. Merge Policy and Stranding diversity indices
merged_div <- 
  sp_year_div %>%
  transmute(
    year_of_observation,
    stranding_shannon  = shannon,
    stranding_richness = richness
  ) %>%
  left_join(
    policy_div %>%
      transmute(
        year_of_observation,
        policy_shannon  = if ("policy_shannon"  %in% names(.)) policy_shannon  else shannon,
        policy_richness = if ("policy_richness" %in% names(.)) policy_richness else richness,
        total_policies  = if ("total_policies"  %in% names(.)) total_policies  else rowSums(across(where(is.numeric)), na.rm = TRUE)
      ),
    by = "year_of_observation"
  )


#Plot Stranding and Policy Type Diversity by Active/Effective Years
ggplot(merged_div, aes(x = year_of_observation)) +
  geom_line(aes(y = stranding_shannon, color = "Stranding Diversity"), linewidth = 1) +
  geom_line(aes(y = policy_shannon, color = "Policy Diversity"), linewidth = 1, linetype = "dashed") +
  theme_minimal(base_size = 13) +
  labs(x = "Year", y = "Shannon Diversity (H')",
       color = "Metric",
       title = "Temporal Comparison of Stranding and Policy-Type Diversity")

#Regression of both shannon indices by team
lm_imp <- lm(stranding_shannon ~ policy_shannon + year_of_observation, data = merged_div)
summary(lm_imp)



plot_df <- merged_div %>%
  select(year_of_observation, stranding_shannon, policy_shannon) %>%
  pivot_longer(cols = c(stranding_shannon, policy_shannon),
               names_to = "Metric", values_to = "Shannon")

ggplot(plot_df, aes(x = year_of_observation, y = Shannon, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed") + #makes separate regression lines for each metric
  theme_minimal(base_size = 13) +
  labs(
    x = "Year",
    y = "Shannon Diversity (H')",
    color = NULL,
    title = "Trends in Policy and Stranding Shannon Diversity over Time"
  )


#Plotting the relationship between policy type and stranding diversity with the lm_model
ggplot(merged_div, aes(x = policy_shannon, y = stranding_shannon)) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Policy Shannon Diversity (H')",
    y = "Stranding Shannon Diversity (H')",
    title = "Relationship between Policy and Stranding Diversity"
  )

cor.test(merged_div$stranding_shannon, merged_div$policy_shannon) #Correlation test
lm_model <- lm(stranding_shannon ~ policy_shannon, data = merged_div)
summary(lm_model)

# Compare models
lm_simple <- lm(stranding_shannon ~ policy_shannon, data = merged_div)
lm_full   <- lm(stranding_shannon ~ policy_shannon + year_of_observation, data = merged_div)

anova(lm_simple, lm_full)



