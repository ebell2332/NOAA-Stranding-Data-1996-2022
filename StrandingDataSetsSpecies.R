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
