library(tidyverse)

sanfrancisco <- read.csv("sanfrancisco_incidents_summer_2014.csv")
seattle <- read.csv("seattle_incidents_summer_2014.csv")

dist_by_PdD_Wk <- sanfrancisco %>% 
  #filter(Category == "DRUG/NARCOTIC") %>%
  group_by(PdDistrict, DayOfWeek) %>%
  summarize(cell = n()) %>%
  group_by(DayOfWeek) %>%
  mutate(dist = cell/n())
fred <- temp %>%
  group_by(PdDistrict) %>%
  summarize(num = n())

ggplot(dist_by_PdD_Wk) + aes(x = DayOfWeek, y = PdDistrict) +
  geom_tile(aes(fill = dist))

ggplot(dist_by_PdD_Wk) + 
  geom_tile(aes(x = DayOfWeek, y = PdDistrict, fill = dist))
  
sanfrancisco %>%
  group_by(DayOfWeek, PdDistrict) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = DayOfWeek, y = PdDistrict)) +
  geom_tile(aes(fill = n))
  
sanfrancisco %>% 
  filter(Category == "DRUG/NARCOTIC") %>%
  ggplot(aes(x = X, y = Y)) + 
  geom_bin2d(bins = 130) +
  scale_fill_gradient(trans = "log10", low = "grey", high = "red")
  
  
ggplot(sanfrancisco) + geom_count(aes(x = DayOfWeek, y = PdDistrict)) +
  ggtitle("Drugs") + theme(plot.title = element_text(hjust = 0.5))


usa <- map_data("usa") 
ggplot(usa) + geom_polygon(aes(x=long, y = lat, group = group, fill = region), 
                           color = "black") + coord_fixed(1.3)

sf_map <- 
  read.csv(url("http://www.sharpsightlabs.com/wp-content/uploads/2014/12/sf_neighborhood_boundaries.txt"))

ggplot(sf_map) + geom_polygon(aes(x=long, y = lat, group = group), color = "black", 
                              fill = "white") + coord_fixed(1.3) #+
  geom_bin2d(data = sanfrancisco, aes(x = X, y = Y), bins = 130) +
  scale_fill_gradient(trans = "log10")  

#center of the map to have the x and y coordinates of the midpoints
#between the extremes in the data
X_center <- (range(sanfrancisco$X)[2] + range(sanfrancisco$X)[1]) / 2
Y_center <- (range(sanfrancisco$Y)[2] + range(sanfrancisco$Y)[1]) / 2
sf_gg <- get_map(location = c(X_center, Y_center), 
                 zoom = 12, maptype = "toner-lite")
data <- filter(sanfrancisco, Category == "ASSAULT")
ggmap(sf_gg, darken = c(.01, "black")) + 
  geom_bin2d(data = data, aes(x = X, y = Y), bins = 200) +
  scale_fill_gradient(trans = "log10") 

#center another in an area with relatively concentrated crime
X_center2 <- -122.41
Y_center2 <- 37.76
sf_gg <- get_map(location = c(X_center2, Y_center2), 
                 zoom = 13, maptype = "toner-lite")
data <- filter(sanfrancisco, Category == "DRUG/NARCOTIC")
ggmap(sf_gg, darken = c(.01, "black")) + 
  geom_bin2d(data = sanfrancisco, aes(x = X, y = Y), bins = 200) +
  scale_fill_gradient(trans = "log10") 

levels(sanfrancisco$Category)






