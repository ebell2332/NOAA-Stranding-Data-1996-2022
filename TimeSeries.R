#install.packages("gganimate")
#install.packages("gifski")  #GIF output
#install.packages("av")    #mp4 output

library(gganimate)              
library(gifski)
library(av)

##############################################
#Time Series Plot
#needs to be fixed, showing lines across states 
p <- ggplot(east_coast_map, aes(x = long, y = lat)) +
  geom_polygon(fill = "gray90", color = "black") +
  geom_point(data = final_clean, aes(x = lon, y = lat, color = Group_species), size = 2) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = 'Year: {closest_state}', color = "Species Group") +
  transition_states(year_of_observation, transition_length = 1, state_length = 1) +
  ease_aes('linear')     

animate(p, renderer = gifski_renderer())


anim_save("~/Downloads/strandings.gif", animation = p)



