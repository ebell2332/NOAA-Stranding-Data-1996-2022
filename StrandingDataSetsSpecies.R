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

