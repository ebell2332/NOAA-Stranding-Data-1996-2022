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


#. Cetacean Line Plot Seasons x Year of Observation----

strandings_by_season <- cetacean_data %>%
  filter(!is.na(Year_of_Observation), !is.na(Seasons)) %>%
  group_by(Year_of_Observation, Seasons) %>%
  summarise(Stranding_Count = n(), .groups = "drop")


ggplot(strandings_by_season, aes(x = Year_of_Observation, y = Stranding_Count, color = Seasons, group = Seasons)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Cetacean Strandings Over Time by Season",
       x = "Year", y = "Stranding Count") +
  theme_minimal()

#. Pinniped Line Plot Seasons x Year of Observation----

strandings_by_season <- pinniped_data %>%
  filter(!is.na(Year_of_Observation), !is.na(Seasons)) %>%
  group_by(Year_of_Observation, Seasons) %>%
  summarise(Stranding_Count = n(), .groups = "drop")


ggplot(strandings_by_season, aes(x = Year_of_Observation, y = Stranding_Count, color = Seasons, group = Seasons)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Pinniped Strandings Over Time by Season",
       x = "Year", y = "Stranding Count") +
  theme_minimal()

summary(strandings_by_season)




#. Cetacean UME Per Year----
#-- Count rows where "UME", "Unusual Mortality Event" appears in the "GE_Type(s)_GE_Module" column
ume_count <- sum(grepl("UME|Unusual Mortality Event", cetacean_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE))


event_counts <- as.data.frame(table(cetacean_data$Event_Type))


cet_ume_data <- cetacean_data %>% filter(Event_Type == "UME")

cet_ume_by_year <- cet_ume_data %>%
  group_by(Year_of_Observation) %>%
  summarise(UME_Count = n(), .groups = "drop")

ggplot(cet_ume_by_year, aes(x = Year_of_Observation, y = UME_Count)) +
  geom_col(fill = "firebrick") +
  labs(title = "Cetacean Unusual Mortality Events by Year",
       x = "Year", y = "Number of UMEs") +
  theme_minimal()


#. Pinniped UME Per Year----

pin_ume_count <- sum(grepl("UME|Unusual Mortality Event", pinniped_data$`GE_Type(s)_GE_Module`, ignore.case = TRUE))

pin_ume_data <- pinniped_data %>% filter(Event_Type == "UME")

pin_ume_by_year <- pin_ume_data %>%
  group_by(Year_of_Observation) %>%
  summarise(UME_Count = n(), .groups = "drop")

ggplot(pin_ume_by_year, aes(x = Year_of_Observation, y = UME_Count)) +
  geom_col(fill = "firebrick") +
  labs(title = "Pinniped Unusual Mortality Events by Year",
       x = "Year", y = "Number of UMEs") +
  theme_minimal()


#. Combined Yearly Line Chart with Year x Strandings for Pinnipeds and Cetaceans----

##. Cetacean Count grouping
cet_annual_strandings <- cetacean_data %>%
  group_by(Year_of_Observation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = "Cetaceans")
cet_annual_strandings

##. Pinniped Count grouping
pin_annual_strandings <- pinniped_data %>%
  group_by(Year_of_Observation) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Group = "Pinnipeds")
pin_annual_strandings

##. Combine the 2 summaries
combined_counts <- bind_rows(cet_annual_strandings, pin_annual_strandings)
head(combined_counts)

#Line graph
cet_pin_year <- ggplot(combined_counts, aes(x = Year_of_Observation, y = Count, color = Group)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Annual Strandings of Cetaceans and Pinnipeds",
       x = "Year", y = "Number of Strandings", color = "Species Group") +
  scale_color_manual(values = c("Cetaceans" = "salmon", "Pinnipeds" = "palegreen4")) +
  theme_minimal() +
  theme(
  theme(legend.position=c("right")) +
  theme(axis.text.x=element_text(size=16,color="black")),
  theme(axis.text.y=element_text(size=16,color="black")),
  theme(axis.title.x=element_text(size=18)),
  theme(axis.title.y=element_text(size=18)),
  theme(plot.title=element_text(size=18)) 
  )
cet_pin_year



##. Boxplot Strandings By Species Group

ggplot(combined_counts, aes(x = Group, y = Count, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Cetaceans" = "salmon", "Pinnipeds" = "palegreen4")) +
  labs(title = "Distribution of Strandings by Species Group",
       x = "Species Group", y = "Stranding Count") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18)
  )






#Boxplot of Species Group by Count with Jitter Points----
statz_ggplot <- theme(legend.position=c("none")) +
  theme(axis.text.x=element_text(size=14,color="gray30")) +
  theme(axis.text.y=element_text(size=14,color="gray30")) +
  theme(axis.title.x=element_text(size=16)) +
  theme(axis.title.y=element_text(size=16)) +
  theme(plot.title=element_text(size=18)) +
  theme(strip.text.x = element_text(size = 16, colour = "black"))

wes_darj <- c("#FF0000", "#00A08A","#F2AD00","#F98400","#5BBCD6","#ECCBAE","#046C9A","#D69C4E","#ABDDDE")



ggplot(combined_counts, aes(x = Group, y = Count)) +
  geom_boxplot(aes(fill=Group),notch=FALSE,outlier.shape=NA) +
  geom_jitter(aes(colour=Group), position=position_jitter(0.25), size=2.5) +
  ggtitle(paste("Strandings by Species Group")) +
  scale_fill_manual(values=wes_darj) +
  scale_color_brewer(palette="Dark2") +
  statz_ggplot



