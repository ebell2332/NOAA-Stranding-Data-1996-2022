library(dplyr)
#install.packages("openxlsx")
library(openxlsx)

#-----------------------Creating Boundaries for Areas 1-3----------------------
# 1. Define latitude breakpoints (update values as needed)
#lat breaks boundaries chosen by ferry terminals on the east coast
lat_breaks <- c(-Inf,40.409, 42.044, Inf)  
  #area 1 = >42.044 (Provincetown, MA)
  #area 2 = 40.409 - 42.044
  #area 3 = <40.409 (Highlands, NJ)

# 2. Assign each point to Area 1, 2, or 3 based on latitude
final_set <- final_set %>%
  mutate(
    Area = cut(lat,
               breaks = lat_breaks,
               labels = c("Area 1|North of Cape Code", "Area 2|Cape Cod to NJ", "Area 3|South of Highlands NJ"),
               right = FALSE)
    )
 # ) %>%
 # filter(!is.na(Policy_Region))  # drop rows that don't match any region

# 3. Count strandings by month, year, and area
summary_table <- final_set %>%
  group_by(Month = month_of_observation,
           Year = year_of_observation,
           Area) %>%
  summarise(Number_of_Strandings = n(), .groups = "drop") %>%
  arrange(Year, Month, Area)


#missing Nov 11, area 1, adding back in
summary_table %>%
  filter(Month == "Nov", Year == "2011")

summary_table %>%
  filter(Month == "Nov", Year == "2011", Area == "Area 1|North of Cape Code")

# 4. Export to Excel
write.xlsx(summary_table, "Stranding_Summary_By_Area.xlsx")


#If I want to have an excel sheet for each Area
# 5. Create workbook and add each area as a separate sheet
wb <- createWorkbook()

# Loop over each area and create a sheet
areas <- unique(summary_table$Area)
for (area in areas) {
  data_subset <- summary_table %>% filter(Area == area) %>% select(-Area)
  addWorksheet(wb, sheetName = as.character(area))
  writeData(wb, sheet = as.character(area), x = data_subset)
}

# 6. Save the Excel file
saveWorkbook(wb, file = "Stranding_Summary_By_Area_Split.xlsx", overwrite = TRUE)





#Mapping Group Species without Unidentified 
ggplot(east_coast_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "gray90", color = "black") +
  coord_fixed(1.3) +
  geom_point(
    data = final_clean %>% filter(Group_species != "Unidentified"),
    aes(x = lon, y = lat, color = Group_species),
    size = 2
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(
    title = "Marine Mammal Observations on the US East Coast",
    x = "Longitude",
    y = "Latitude",
    color = "Species Group"
  ) +
  # Horizontal lines 
  geom_hline(yintercept = 41.992, linetype = "dashed", color = "black", size = 1) + #P-town  
  geom_hline(yintercept = 40.288, linetype = "dashed", color = "black", size = 1)  #Highlands, NJ
  
#geom_hline(yintercept = 41.25, linetype = "dashed", color = "green", size = 1) #Mid-Atlantic Bight

#-------------------------------------------------------------------------------
#Plotting Strandings by # of Policy

# Step 1: Load the Excel file
df <- read_excel("Stranding_Summary_By_Area.xlsx", sheet = 1)

# Step 2: Prepare strandings data
strandings_data <- df %>%
  filter(!is.na(Number_of_Strandings)) %>%
  mutate(
    Type = "Strandings",
    Count = Number_of_Strandings
  ) %>%
  select(Month, Year, Count, Type)

# Step 3: Prepare policies data using real values (NA → 0)
policies_data <- df %>%
  filter(!is.na(Year), !is.na(Month)) %>%
  mutate(
    Count = ifelse(is.na(`# of Policies`), 0, as.numeric(`# of Policies`)),
    Type = "Policies Implemented"
  ) %>%
  select(Month, Year, Count, Type)

# Step 4: Combine both datasets
combined_data <- bind_rows(strandings_data, policies_data) %>%
  mutate(
    Count_signed = ifelse(Type == "Policies Implemented", -Count, Count),
    Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")
  )

# Step 5: Create Split Plot
ggplot(combined_data, aes(x = Date, y = Count_signed, fill = Type)) +
  geom_area() +
  scale_fill_manual(values = c("Strandings" = "red", "Policies Implemented" = "blue")) +
  theme_minimal() +
  labs(
    title = "Monthly Strandings and Policy Implementations",
    x = "Date",
    y = "Count (Strandings / Policies)",
    fill = "Event Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
    #not good graph becuase of the disproportionate in number of strandings and policies implemented

#Stacked Bar Graph by Region/Area
# Step 2: Prepare strandings data
strandings_data <- df %>%
  filter(!is.na(Number_of_Strandings)) %>%
  mutate(
    Type = "Strandings",
    Count = Number_of_Strandings
  ) %>%
  select(Month, Year, Area, Count, Type)

# Step 3: Prepare policies data using real values (NA → 0)
policies_data <- df %>%
  filter(!is.na(Year), !is.na(Month)) %>%
  mutate(
    Count = ifelse(is.na(`# of Policies`), 0, as.numeric(`# of Policies`)),
    Type = "Policies Implemented"
  ) %>%
  select(Month, Year, Area, Count, Type)

# Step 4: Combine both datasets
combined_data <- bind_rows(strandings_data, policies_data) %>%
  mutate(
    Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")
  )

# Step 5: Stacked area plot by area
ggplot(combined_data, aes(x = Date, y = Count, fill = Type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Strandings" = "red", "Policies Implemented" = "blue")) +
  facet_wrap(~ Area, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Monthly Strandings and Policy Implementations by Area",
    x = "Date",
    y = "Count",
    fill = "Event Type"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
#not good graph becuase of the disproportionate in number of strandings and policies implemented

#-----------------------------------Plotting Policies on Map----------------------------------------------------------
#1. Count Number of Policies in Each Area
# Read in the Excel file
df <- read_excel("Stranding_Summary_By_Area.xlsx") #update excel sheet and re-run

# Clean column names
colnames(df) <- trimws(colnames(df))

# Summarize total number of policies by area
policy_totals <- df %>%
  group_by(Area) %>%
  summarise(Total_Policies = sum(`# of Policies`, na.rm = TRUE)) %>%
  arrange(desc(Total_Policies))

print(policy_totals)
  #Area 1 = 159
  #Area 2 = 123
  #Area = 37
  #total approx. = 319

# Create a policy summary with approximate centroids
policy_summary <- data.frame(
  Area = c("Area 1|North of Cape Cod", 
           "Area 2|Cape Cod to NJ", 
           "Area 3|South of Highlands NJ"),
  lat = c(43.0, 41.25, 39.0),          # approximate central latitudes
  lon = c(-70.0, -72.5, -74.5),        # approximate longitudes per area
  total_policies = c(159, 123, 37)     # use your calculated totals
)

# Map with policies adding and boundaries break up
area_policy_map <- ggplot() +
  geom_polygon(data = east_coast_map, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "black") +
  geom_point(data = final_set, aes(x = lon, y = lat), 
             color = "darkkhaki", alpha = 0.5, size = 1.5) +
  geom_point(data = policy_summary, aes(x = lon, y = lat, size = total_policies), 
             shape = 17, color = "darkmagenta", show.legend = TRUE) +
  geom_text(data = policy_summary, aes(x = lon, y = lat, label = total_policies),
            vjust = -1, size = 4, color = "black") +
  coord_fixed(1.3) +
  # Horizontal lines 
  geom_hline(yintercept = 41.992, linetype = "dashed", color = "black", size = 1) + #P-town  
  geom_hline(yintercept = 40.288, linetype = "dashed", color = "black", size = 1) + #Highlands, NJ
  theme_minimal() +
  labs(title = "Strandings and Total Policies by Areas 1:3", subtitle = "From Maine to Virginia (1996-2022)", 
       x = "Longitude", y = "Latitude",
       size = "# of Policies") +
  # Add labels at specific coordinates
  annotate("label", x = -69.0000, y = 44.0000, label = "Area 1|North of Cape Cod", color = "black", size = 4.5) +
  annotate("label", x = -69.0000, y = 41.0000, label = "Area 2|Cape Cod to NJ", color = "black", size = 4.5) +
  annotate("label", x = -69.0000, y = 39.0000, label = "Area 3|South of Highlands NJ", color = "black", size = 4.5)
area_policy_map

#---------------------------Line Plot of Area & Policy--------------------------
v_df <- read_excel("Stranding_Summary_By_Area.xlsx")
#1. prepare data from excel file
summary_v_df <- v_df %>%
  group_by(Year, Area) %>%
  summarise(
    Num_Policies = sum(`# of Policies`, na.rm = TRUE),
    Num_Strandings = sum(`Number_of_Strandings`, na.rm = TRUE),
    .groups = "drop"
  )

long_df <- summary_v_df %>%
  pivot_longer(cols = c(Num_Policies, Num_Strandings),
               names_to = "Metric",
               values_to = "Count")
# Filter out NA area rows before plotting
long_df <- long_df %>% filter(!is.na(Area), Area != "NA")

#2 create line plot
ggplot(long_df, aes(x = Year, y = Count, color = Metric)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Area, scales = "free_y" ) +
  scale_color_manual(values = c("Num_Policies" = "firebrick", "Num_Strandings" = "steelblue")) +
  labs(
    title = "Trend of Policies and Strandings by Area",
    x = "Year",
    y = "Count",
    color = "Metric"
  ) +
  theme_minimal()

####Bar and Line Plot of Area & Policy
# Separate the strandings and policies into two dataframes
strandings_df <- long_df %>% filter(Metric == "Num_Strandings")
policies_df   <- long_df %>% filter(Metric == "Num_Policies")

# Merge them back by Year and Area
combo_df <- left_join(
  strandings_df %>% rename(Strandings = Count),
  policies_df %>% select(Year, Area, Policies = Count),
  by = c("Year", "Area")
)

# Plot: Bars for strandings, line for policies
ggplot(combo_df, aes(x = as.factor(Year), y = Strandings, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_line(aes(y = Policies, group = Area, color = Area), size = 1) +
  geom_point(aes(y = Policies, color = Area), size = 2) +
  scale_fill_manual(values = c(
    "Area 1|North of Cape Code" = "#AED6F1",
    "Area 2|Cape Cod to NJ" = "#F9E79F",
    "Area 3|South of Highlands NJ" = "#D2B4DE"
  )) +
  
  scale_color_manual(values = c(
    "Area 1|North of Cape Code" = "#154360",
    "Area 2|Cape Cod to NJ" = "#7D6608",
    "Area 3|South of Highlands NJ" = "#4A235A"
  )) +
  
  scale_y_continuous(sec.axis = sec_axis(~., name = "# of Policies")) +
  labs(
    title = "Number of Strandings and Policies Over Time by Area",
    x = "Year",
    y = "Number of Strandings",
    fill = "Area",
    color = "Area"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 6),
    legend.position = "bottom"
  )

