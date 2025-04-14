##New Data Sets For All Cetacean Strandings----

cetacean_data <- stranding_data %>% filter(Group_species =="Cetaceans")

#write.csv(cetacean_data, "Cetacean_Strandings_Data.csv", row.names = FALSE)      #saved to computer already

head(cetacean_data)


##New Data Set for all Pinniped Strandings----
pinniped_data <- stranding_data %>% filter(Group_species =="Pinnipeds")

#write.csv(pinniped_data, "Pinniped_Strandings_Data.csv", row.names = FALSE)      #saved to computer already

head(pinniped_data)



##New Data Set for all Unidentified Strandings----
unidentified_data <- stranding_data %>% filter(Group_species =="Unidentified")

#write.csv(unidentified_data, "Unidentified_Strandings_Data.csv", row.names = FALSE)      #saved to computer already

head(unidentified_data)


#. Plotting Cetacean strandings by year----
cetacean_data <- read.csv("/Users/ebell23/Downloads/Cetacean_Strandings_Data.csv")
colnames(cetacean_data)

cet_annual_strandings <- cetacean_data %>%
  group_by(Year_of_Observation) %>%
  summarise(Count = n(), .groups = "drop")
cet_annual_strandings

ggplot(cet_annual_strandings, aes(x = Year_of_Observation, y = Count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +        # Points for visibility
  labs(title = "Trends in Cetacean Strandings",
       x = "Year",
       y = "Number of Strandings") +
  theme_minimal()
##-graph saved to computer (through export button)

stranding_data$Event_Type <- "Other"  # Default category

#--Plotting UME and Mass Stranding Cetacean Occurences--continue working on----
# Classify events (you can add more patterns if needed)
cetacean_data$Event_Type[grepl("UME|Unusual Mortality Event", cetacean_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE)] <- "UME"
cetacean_data$Event_Type[grepl("Mass Stranding", cetacean_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE)] <- "Mass Stranding"

# Summarize counts
event_counts <- as.data.frame(table(cetacean_data$Event_Type))

# Create a bar chart
ggplot(event_counts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Occurrences of UME and Mass Strandings of Cetaceans",
       x = "Event Type",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

#. Plotting Pinniped Strandings by Year----
pinniped_data <- read.csv("/Users/ebell23/Downloads/Pinniped_Strandings_Data.csv")


pin_annual_strandings <- pinniped_data %>%
  group_by(Year_of_Observation) %>%
  summarise(Count = n(), .groups = "drop")
pin_annual_strandings

ggplot(pin_annual_strandings, aes(x = Year_of_Observation, y = Count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +        # Points for visibility
  labs(title = "Trends in Pinniped Strandings",
       x = "Year",
       y = "Number of Strandings") +
  theme_minimal()
##-graph saved to computer (through export button)

#. Plotting Unidentified Strandings by Year----
unidentified_data <- read.csv("/Users/ebell23/Downloads/Unidentified_Strandings_Data.csv")

uni_annual_strandings <- unidentified_data %>%
  group_by(Year_of_Observation) %>%
  summarise(Count = n(), .groups = "drop")
uni_annual_strandings

ggplot(uni_annual_strandings, aes(x = Year_of_Observation, y = Count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +        # Points for visibility
  labs(title = "Trends in Unidentified Strandings",
       x = "Year",
       y = "Number of Strandings") +
  theme_minimal()


#. Plotting Cetacean Strandings by Species
 #-- Group by Species
species_counts_cet <- cetacean_data %>%
  count(Species)
species_counts_cet
  #--Cetacean x Group Event Number Boxplot
sapply(cetacean_data, is.numeric) #finding all numeric columns
names(cetacean_data)[sapply(cetacean_data, is.numeric)]

group_cet_boxplot <- ggplot(cetacean_data, aes(x = Species, y = Num_of_Animals_in_Group_Event)) +    #boxplot
  geom_boxplot(fill = "lightblue") +
  labs(title = "Number of Animals per Event by Cetacean Species",
       x = "Species",
       y = "Group Size") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
group_cet_boxplot
  #--Cetacean x Observation Year Boxplot
obyear_cet_boxplot <- ggplot(cetacean_data, aes(x = Species, y = Year_of_Observation)) +
  geom_boxplot(fill = "skyblue3") +
  labs(title = "Observation Year by Cetacean Species",
       x = "Species",
       y = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
obyear_cet_boxplot
 
#. Pinniped Boxplots----
  #--Species Count
species_counts_pin <- pinniped_data %>%
  count(Species)
species_counts_pin
  #--Numeric Fields 
sapply(pinniped_data, is.numeric)   #finding all numeric columns
names(pinniped_data)[sapply(pinniped_data, is.numeric)]
  #--Observation Year x Pinniped Species
obyear_pinn_boxplot <- ggplot(pinniped_data, aes(x = Species, y = Year_of_Observation)) +
  geom_boxplot(fill = "skyblue3") +
  labs(title = "Observation Year by Pinniped Species",
       x = "Species",
       y = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
obyear_pinn_boxplot






















