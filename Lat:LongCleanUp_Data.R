
################################################################################
# Clean Lat/Lon data 
# My apprach was to ignore latitude_units and longitude_units
# Insead I focus on the data format 
# Ultimately you only lose ~1600 observations 
################################################################################

library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(measurements)
library(ggplot2)
library(maps)

raw_data = read_excel("/Users/ebell23/Downloads/1996-2022_Stranding_working_data.xlsx")%>%
  clean_names()

# only selecting the columns i care about so i can keep track of how lat lon are being transformed 
data = raw_data %>% select(species, locality_detail, latitude, latitude_units, longitude, longitude_units) #34039 observations

# Drop 0 or NAs since they are not useable 
dropped_rows = data[data$latitude == "0.0" | is.na(data$latitude), ] # 331 observations do not have lat data
dropped_rows = data[data$longitude == "0.0" | is.na(data$longitude), ] # 338 observations do not have lat data
data = data[data$latitude != "0.0" & !is.na(data$latitude), ]
data = data[data$longitude != "0.0" & !is.na(data$longitude), ] # after dropping 0 and NAs 33699 data points, not bad!

################################################################################
# convert to numeric 
data$lat = as.numeric(data$latitude)
sum(is.na(data$lat)) # 1080 observations were converted to NAs most of these have two or more decimal places 
data$lon = as.numeric(data$longitude)
sum(is.na(data$lon)) # 1080 observations were converted to NAs most of these have two or more decimal places
# Good news is that even if you ignore these you do not loose a ton of observations 

################################################################################
# Now I want figuring out how many already seem to be in decimal degree format 
# XX.XXXX two digits followed by a decimal and more values 
# checking lat 
data$flagged = ifelse(str_detect(data$lat, "^\\d{2}\\.\\d+$"), 1, 0)
clean = subset(data,flagged ==1) # this is good about 80% of your observations are already in the right lat format 
hist(clean$lat) # they all seem to be in the right numeric range except for one 71 outlier 
clean = clean[clean$lat <= 47 & clean$lat >= 36, ] # dropping that observation 
hist(clean$lat) #checking histogram again
min(clean$lat)
max(clean$lat) # both min and max are in the right range 

#checking lon (negatives)
clean$flagged = ifelse(str_detect(clean$lon, "^-?\\d{2,3}\\.\\d+$"), 1, 0)
clean = subset(clean,flagged ==1) 
hist(clean$lon) # definitely some outliers here 
min(clean$lon) # that -700 is def wrong
max(clean$lon) # and some values are correctly formatted but in positive values 
# i am going to assume that positive values should be negatives 
clean$lon = ifelse(clean$lon>0, -clean$lon, clean$lon)
clean = clean[clean$lon >= -700 & clean$lon <= -60 , ] # dropping the -700 outlier 
hist(clean$lon) # this looks more like it 
min(clean$lon)
max(clean$lon)

################################################################################
# so if you choose to ignore all the problematic formatted lat/lon coordinates 
# you still have 25,943 valid coordinates with clean lat lon formats without needing to do much work
# you could write out the clean df and use that for your analysis 
clean <- data %>%
  mutate(Group_species = case_when(
    species %in% c("acutorostrata", "ampullatus", "attenuata", "bidens", "borealis", "breviceps", "cavirostris", "crassidens", "densirostris", "electra", "europaeus", "glacialis", "macrocephalus", "macrorhynchus", "melas", "musculus", "novaenagliae", "physalus", "sima", "acutus", "albirostris", "bredanensis", "capensis", "clymene", "coeruleoalba","crugiger", "delphis", "frontalis", "griseus", "truncatus","phocoena") ~ "Cetaceans",
    species %in% c("barbatus", "cristata", "groenlandica", "grypus", "hispida", "vitulina") ~ "Pinnipeds",
    TRUE ~ "Unidentified"
  ))

us_states = map_data("state")
east_coast_states = tolower(c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island",
                              "Connecticut", "New York", "New Jersey", "Pennsylvania", "Delaware",
                              "Maryland", "District of Columbia", "Virginia"))

east_coast_map = us_states %>%
  filter(region %in% east_coast_states)


# Plot
ggplot(east_coast_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray", color = "black") +
  coord_fixed(1.3) +
  geom_point(data = clean, aes(x = lon, y = lat, group = species, color= species), size = 2) +
  theme_minimal() +
  labs(title = "US East Coast States from Maine to Virginia")


#. Plot Not running when Group_Species is added
ggplot(east_coast_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "gray90", color = "black") +
  coord_fixed(1.3) +
  geom_point(data = clean, aes(x = lon, y = lat, color = Group_species), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Marine Mammal Observations on the US East Coast",
       color = "Species Group")



################################################################################
# you can attempt to clean up the others 
# subset the problem children not in clean df
clean = select(clean, -flagged)

join_cols <- setdiff(intersect(names(raw_data), names(clean)), c("lat", "lon")) 
problem_children = anti_join(raw_data, clean, by=join_cols) #8096 other problematic obserbations that are not in
unique(problem_children$latitude_units)
unique(problem_children$longitude_units)

# there seem to be a couple reccuring cases so put together a nifty function with the help of chatgpt
coordsfix <- function(x) {
  x <- str_trim(x)
  if (is.na(x) || x == "") return(NA_real_)
  
  # Case 1: Decimal minutes — DDMM.mm
  # e.g. "4406.54" → 44 + 6.54/60 = 44.109
  if (isTRUE(str_detect(x, "^\\d{4}\\.\\d+$"))) {
    deg <- as.numeric(substr(x, 1, 2))
    min <- as.numeric(substr(x, 3, nchar(x)))
    return(deg + min / 60)
    
    # Case 2: Packed DMS — DDMMSS.ss
    # e.g. "365458.0" → 36 + 54/60 + 58/3600 = 36.91611
  } else if (isTRUE(str_detect(x, "^\\d{6,7}\\.\\d+$"))) {
    deg <- as.numeric(substr(x, 1, 2))
    min <- as.numeric(substr(x, 3, 4))
    sec <- as.numeric(substr(x, 5, nchar(x)))
    return(deg + min / 60 + sec / 3600)
    
    # Case 3: Space-separated — "41 28.9" or "41 28 22.2"
    # e.g. "41 28.9" → 41 + 28.9/60 = 41.48167
    #      "41 28 22.2" → 41 + 28/60 + 22.2/3600 = 41.47283
  } else if (isTRUE(str_detect(x, "^\\d{1,3}\\s+\\d+(\\s+\\d+(\\.\\d+)?)?$"))) {
    parts <- as.numeric(unlist(str_split(x, "\\s+")))
    if (length(parts) == 3) {
      return(parts[1] + parts[2] / 60 + parts[3] / 3600)
    } else if (length(parts) == 2) {
      return(parts[1] + parts[2] / 60)
    } else {
      return(NA_real_)
    }
    
    # Case 4: Multi-dot (e.g. "4119.69.2") — malformed
    # Return NA
  } else if (isTRUE(str_count(x, "\\.") > 1)) {
    return(NA_real_)
    
    # Case 5: Unknown or unhandled format
    # Return NA
  } else {
    return(NA_real_)
  }
}



#fix lat
problem_children$lat= sapply(problem_children$latitude, coordsfix)

#fix lon
problem_children$lon = sapply(problem_children$longitude, coordsfix)

problem_children = problem_children[problem_children$lat <= 47 & problem_children$lat >= 36 & !is.na(problem_children$lat), ] # dropping that observation outside of the range we want 
hist(problem_children$lat) # seems to be in the right range 

hist(problem_children$lon) # same poitives problem, again will assume if they are int he right range they should be negatives 
problem_children$lon = ifelse(problem_children$lon>0, -problem_children$lon, problem_children$lon)
problem_children = problem_children[problem_children$lon >= -700 & problem_children$lon <= -60 & !is.na(problem_children$lon) , ] # dropping the -700 outlier 
hist(problem_children$lon)
sum(is.na(problem_children$lon)) #and it fixed most of the odd observations 

# This addds 6426 observations back which is most of them with not too much effort 

# plot of  problem children 
ggplot(east_coast_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray", color = "black") +
  coord_fixed(1.3) +
  geom_point(data = problem_children, aes(x = lon, y = lat, group = species, color= species), size = 2) +
  theme_minimal() +
  labs(title = "US East Coast States from Maine to Virginia")



################################################################################
# combine clean data with problem children 
# lat lon are in decimal degrees 
colnames(clean)
colnames(problem_children)
final_clean = rbind(clean, problem_children)

# write out this dataset and use it in analysis instead of cleaning everytime 
#write.csv(final_clean, "some location on your laptop.csv")


# plot of clean lat lon with no outliers 
ggplot(east_coast_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray", color = "black") +
  coord_fixed(1.3) +
  geom_point(data = final_clean, aes(x = lon, y = lat, group = species, color= species), size = 2) +
  theme_minimal() +
  labs(title = "US East Coast States from Maine to Virginia")






