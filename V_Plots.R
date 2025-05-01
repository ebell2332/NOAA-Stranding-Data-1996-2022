##- Cetacean Violin Plots----
unique(cetacean_data$Latitude[is.na(as.numeric(cetacean_data$Latitude))])   #looking at all unique values in the latitude column

cetacean_data$Year_of_Observation <- as.factor(cetacean_data$Year_of_Observation) #convert year_of_obs to factor

bad_lat <- cetacean_data$Latitude[is.na(as.numeric(cetacean_data$Latitude))]  #check which values are bad
unique(bad_lat)

cetacean_clean <- cetacean_data %>%   #filter out bad values
  filter(!is.na(Latitude), Latitude != "", Latitude != "NA") %>%  
  mutate(Latitude_num = as.numeric(Latitude)) %>%
  subset(Latitude_num < 46)    #filter to latitude less than 46 degrees
#summary(cetacean_clean$Latitude_num)

cetacean_clean <- cetacean_clean %>%      #convert latitude values to decimal degrees
  mutate(Lat_dd = case_when(
    Latitude_num > 10000 ~ {
      deg <- floor(Latitude_num / 10000)
      min <- floor((Latitude_num %% 10000) / 100)
      sec <- Latitude_num %% 100
      deg + (min / 60) + (sec / 3600)
    },
    Latitude_num > 100 ~ {
      deg <- floor(Latitude_num / 100)
      min <- Latitude_num %% 100
      deg + (min / 60)
    },
    TRUE ~ Latitude_num
  ))
  #. Plotting Data
violin_plot_cet <- ggplot(cetacean_clean, aes(x = Year_of_Observation, y = Lat_dd)) +  #plot year x latitude as violin plot
  geom_violin(fill = "lightblue", color = "darkblue") +   #plot looks horrible and needs work
  labs(title = "Latitude Distribution by Year for Cetaceans",
       x = "Year", y = "Latitude (Decimal Degrees)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
violin_plot_cet


  #. Violin Plot Cetacean Month x Latitude----

#Putting Months as a factor and changing labels 
cetacean_clean$Month_of_Observation = factor(cetacean_clean$Month_of_Observation, levels=c("JAN", "FEB", "MAR", "APR","MAY",
                                                                                           "JUN", "JUL", "AUG","SEP", "OCT","NOV", "DEC"),
                                             labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                                       "Oct", "Nov", "Dec")) #levels = right order, labels = how it should be labeled


ggplot(cetacean_clean, aes(x = Month_of_Observation, y = Lat_dd)) +
geom_violin(fill = "lightblue", color = "darkblue") +   #plot looks horrible and needs work
  labs(title = "Latitude Distribution by Month for Cetaceans",
       x = "Year", y = "Latitude (Decimal Degrees)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Pinniped Violin plot----
unique(pinniped_data$Latitude[is.na(as.numeric(pinniped_data$Latitude))])   #looking at all unique values in the latitude column

pinniped_data$Year_of_Observation <- as.factor(pinniped_data$Year_of_Observation) #convert year_of_obs to factor

bad_lat <- pinniped_data$Latitude[is.na(as.numeric(pinniped_data$Latitude))]  #check which values are bad
unique(bad_lat)

pinniped_clean <- pinniped_data %>%   #filter out bad values
  filter(!is.na(Latitude), Latitude != "", Latitude != "NA") %>%  
  mutate(Latitude_num = as.numeric(Latitude)) %>%
  subset(Latitude_num < 46) %>% #filter to latitude less than 46 degrees (ME latitude)
  subset(Latitude_num > 35) #filter to latitude greater than 35 degrees (VA latitude)
#summary(pinniped_clean$Latitude_num)

pinniped_clean <- pinniped_clean %>%      #convert latitude values to decimal degrees
  mutate(Lat_dd = case_when(
    Latitude_num > 10000 ~ {
      deg <- floor(Latitude_num / 10000)
      min <- floor((Latitude_num %% 10000) / 100)
      sec <- Latitude_num %% 100
      deg + (min / 60) + (sec / 3600)
    },
    Latitude_num > 100 ~ {
      deg <- floor(Latitude_num / 100)
      min <- Latitude_num %% 100
      deg + (min / 60)
    },
    TRUE ~ Latitude_num
  ))
#. Plotting Data
violin_plot_pin <- ggplot(pinniped_clean, aes(x = Year_of_Observation, y = Lat_dd)) +  #plot year x latitude as violin plot
  geom_violin(fill = "lightblue", color = "darkblue") +   
  labs(title = "Latitude Distribution by Year for Pinnipeds",
       x = "Year", y = "Latitude (Decimal Degrees)") +
  theme_minimal() +

  theme(axis.text.x = element_text(angle = 45, hjust = 1))
violin_plot_pin


pinniped_clean$Month_of_Observation = factor(pinniped_clean$Month_of_Observation, levels=c("JAN", "FEB", "MAR", "APR","MAY",
                                                                                           "JUN", "JUL", "AUG","SEP", "OCT","NOV", "DEC"),
                                             labels =c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                                       "Oct", "Nov", "Dec")) #levels = right order, labels = how it should be labeled


ggplot(pinniped_clean, aes(x = Month_of_Observation, y = Lat_dd)) +
  geom_violin(fill = "lightblue", color = "darkblue") +   #plot looks horrible and needs work
  labs(title = "Latitude Distribution by Month for Pinnipeds",
       x = "Year", y = "Latitude (Decimal Degrees)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
